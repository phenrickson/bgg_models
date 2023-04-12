# recipes for bgg outcomes

#message("creating recipes...")

### helper functions

# basic recipe setup
base_recipe_func = function(data,
                            outcome) {
        
        outcome = enquo(outcome)
        
        recipe(x=data) %>%
                # set bgg id variables
                update_role(
                        one_of("game_id",
                               "name",
                               "yearpublished",
                               "image",
                               "thumbnail",
                               "description",
                               # "load_ts",
                               "average",
                               "usersrated",
                               "log_usersrated",
                               "stddev",
                               "numcomments",
                               "numweights",
                               "owned",
                               "trading",
                               "wanting",
                               "wishing",
                               "users_threshold",
                               "pred_hurdle",
                               "averageweight",
                               "bayesaverage",
                               "username", 
                               "type", 
                               "rating",
                               "own",
                               "ever_owned",
                               "rated",
                               "highly_rated",
                               "preordered",
                               "prevowned",
                               "fortrade",
                               "want",
                               "wanttoplay",
                               "wanttobuy",
                               "wishlist",
                               "wishlistpriority",
                               "url", 
                               "user_load_ts"),
                        new_role = "id") %>%
                # set outcome varable
                update_role(!!outcome,
                            new_role = "outcome") %>%
                # set all others as predictors
                update_role(-has_role("id"),
                            -has_role("outcome"),
                            new_role = "predictor") %>%
                # indicate missingness in numeric features
                step_indicate_na(all_numeric_predictors(),
                                 prefix = "missing") %>%
                # make time per player variable
                step_mutate(time_per_player = playingtime/ maxplayers) %>% 
                # remove zero variance predictors
                step_zv(all_predictors())
        
}

# function for extract dummies from nominal
dummy_extract_variable = function(recipe,
                                  variable,
                                  threshold = 100) {
        
        var = enquo(variable)
        
        recipe %>%
                # tokenize 
                step_dummy_extract(!!var,
                                   sep = ", ",
                                   other = "remove_other_field",
                                   threshold = threshold) %>%
                # remove other var
                step_rm(contains("remove_other_field"))
        
}

# standard dummy recipes
dummy_recipe_func = function(recipe) {
        
        recipe %>%
                # include most mechanics
                dummy_extract_variable(mechanics,
                                       threshold = 50) %>%
                # include all categories
                dummy_extract_variable(categories,
                                       threshold = 1) %>%
                # families at min 100
                dummy_extract_variable(families,
                                       threshold = 100) %>%
                # publishers at 50
                dummy_extract_variable(publishers,
                                       threshold = 50) %>%
                # designers at 25
                dummy_extract_variable(designers,
                                       threshold = 25) %>%
                # artists min 50
                dummy_extract_variable(artists,
                                       threshold = 50)
}

# imputation
impute_recipe_func = function(recipe) {
        
        recipe %>%
                # impute missingness in selected features with median
                step_impute_median(playingtime,
                                   minplayers,
                                   maxplayers,
                                   minage,
                                   time_per_player) %>% # medianimpute numeric predictors
                # truncate minage to no greater than 18
                step_mutate(minage = dplyr::case_when(minage > 18 ~ 18,
                                                      minage < 0 ~ 0,
                                                      TRUE ~ minage)) %>%
                # truncate player counts
                step_mutate(minplayers = dplyr::case_when(minplayers > 10 ~ 10,
                                                          TRUE ~ minplayers)) %>%
                step_mutate(maxplayers = dplyr::case_when(maxplayers > 20 ~ 10,
                                                          maxplayers <=1 ~ 1,
                                                          TRUE ~ maxplayers)) %>%
                step_rm(minplaytime, maxplaytime)
}

# additional preprocessing steps
preproc_recipe_func = function(recipe) {
        
        recipe %>%
                # number_mechanics
                step_mutate(number_mechanics = rowSums(dplyr::across(starts_with("mechanics")))) %>%
                # number categories
                step_mutate(number_categories = rowSums(dplyr::across(starts_with("categories")))) %>%
                # specific feature for unmatched series
                step_mutate(family_unmatched_series = dplyr::case_when(grepl("Unmatched:", name) ~ 1,
                                                                       TRUE ~ 0)) %>%
                # log time per player and playingtime
                step_log(time_per_player,
                         playingtime,
                         offset = 1) %>%
                # truncate yearpublished
                step_mutate(year = dplyr::case_when(yearpublished < 1900 ~ 1900,
                                                    TRUE ~ yearpublished),
                            role = "predictor") %>%
                # indicator for published before 1900
                step_mutate(published_before_1900 = dplyr::case_when(yearpublished < 1900 ~ 1,
                                                                     TRUE ~ 0)) %>%
                # solo game
                # big box/deluxe/anniversary edition
                step_mutate(deluxe_edition = dplyr::case_when(grepl("kickstarter|big box|deluxe|mega box", tolower(name))==T ~ 1,
                                                              TRUE ~ 0)) %>%
                # word count
                step_mutate(word_count = stringi::stri_count_words(description)) %>%
                step_mutate(word_count = tidyr::replace_na(word_count, 0)) %>%
                # magical phrase in description
                step_mutate(description_from_publisher = dplyr::case_when(grepl("description from publisher", tolower(description))==T ~ 1,
                                                                          TRUE ~ 0))
} 

# splines
# add splines for nonlinear effects for linear models
splines_recipe_func = function(recipe) {
        
        recipe %>%
                #add spline for truncated yearpublished
                step_ns(year,
                        deg_free = 5) %>%
                # spline for number mechanics
                step_ns(number_mechanics,
                        deg_free = 5) %>%
                # spline for number categories
                step_ns(number_categories,
                        deg_free = 5) %>%
                # spliens for averageweight
                step_ns(est_averageweight,
                        deg_free = 5)
}


# normalize
normalize_recipe_func = function(recipe) {
        
        recipe %>%
                step_normalize(all_numeric_predictors())
}

### one function to create standard recipes given outcome
# create standard recipes for users
make_user_recipes = function(data,
                             outcome) {
        
        outcome = enquo(outcome)
        
        # basic recipe without publishers/artists/designers
        # tokenize mechanics and categories
        # impute missigness
        # preprocess
        base_impute_recipe = 
                data %>%
                # remove selected variables
                select(-families, -publishers, -artists, -designers) %>%
                # make base recipe %>%
                base_recipe_func(outcome = !!outcome) %>%
                # dummy extract categories
                dummy_extract_variable(c(categories),
                                       threshold = 1) %>%
                # dummy extract mechanics
                dummy_extract_variable(c(mechanics),
                                       threshold = 50) %>%
                # impute missingness
                impute_recipe_func() %>%
                # basic preprocessing
                preproc_recipe_func() %>%
                # remove unmatched
                step_rm(family_unmatched_series) %>%
                # normalize
                step_normalize(all_numeric_predictors()) %>%
                # check missing
                check_missing(all_predictors())
        
        # basic recipe without publishers/artists/designers
        # tokenize mechanics and categories
        # impute missigness
        # preprocess
        # add splines
        base_splines_recipe = 
                data %>%
                # remove selected variables
                select(-families, -publishers, -artists, -designers) %>%
                # make base recipe %>%
                base_recipe_func(outcome = !!outcome) %>%
                # dummy extract categories
                dummy_extract_variable(c(categories),
                                       threshold = 1) %>%
                # dummy extract mechanics
                dummy_extract_variable(c(mechanics),
                                       threshold = 50) %>%
                # impute missingness
                impute_recipe_func() %>%
                # basic preprocessing
                preproc_recipe_func() %>%
                # remove unmatched
                step_rm(family_unmatched_series) %>%
                # splines
                splines_recipe_func() %>%
                # normalize
                step_normalize(all_numeric_predictors()) %>%
                # check missing
                check_missing(all_predictors())
        
        # recipe with all features
        # dummy extract
        # preprocessing
        # imputation
        all_impute_recipe = 
                data %>%
                # make base recipe %>%
                base_recipe_func(outcome = !!outcome) %>%
                # dummy extract categorical
                dummy_recipe_func() %>%
                # impute missingness
                impute_recipe_func() %>%
                # basic preprocessing
                preproc_recipe_func() %>%
                # normalize
                step_normalize(all_numeric_predictors()) %>%
                # check missing
                check_missing(all_predictors())
        
        # splines
        all_impute_splines_recipe =
                data %>%
                # make base recipe %>%
                base_recipe_func(outcome = !!outcome) %>%
                # dummy extract categorical
                dummy_recipe_func() %>%
                # impute missingness
                impute_recipe_func() %>%
                # basic preprocessing
                preproc_recipe_func() %>%
                # splines
                splines_recipe_func() %>%
                # normalize
                step_normalize(all_numeric_predictors()) %>%
                # check missing
                check_missing(all_predictors())
        
        # base with trees
        base_trees_recipe = 
                data %>%
                # remove selected variables
                select(-families, -publishers, -artists, -designers) %>%
                # make base recipe %>%
                base_recipe_func(outcome = !!outcome) %>%
                # dummy extract categories and mechanics
                dummy_extract_variable(c(mechanics, categories),
                                       threshold = 1) %>%
                # basic preprocessing
                preproc_recipe_func()
        
        
        # recipe with all features
        # for trees, so less preprocessing
        all_trees_recipe = 
                data %>%
                # make base recipe %>%
                base_recipe_func(outcome = !!outcome) %>%
                # dummy extract categorical
                dummy_recipe_func() %>%
                # basic preprocessing
                preproc_recipe_func()
        
        recipes = list("minimal_impute" = base_impute_recipe,
                       "minimal_trees" = base_trees_recipe,
                       "minimal_splines" = base_splines_recipe,
                       "all_impute" = all_impute_recipe,
                       "all_impute_splines" = all_impute_splines_recipe,
                       "all_trees" = all_trees_recipe)
        
        return(recipes)
        
}
