# make user recipes


# build a predefined set of user recipes
make_user_recipes_list = function(data,
                              outcome) {
        
        outcome = enquo(outcome)
        
        # recipe without designers/artists/publishers/families
        minimal_recipe = 
                data %>%
                # remove bgg variables      
                select(-families, -publishers, -artists, -designers) %>%
                # build recipe
                make_user_recipe(outcome = !!outcome) %>%
                add_imputation() %>%
                add_dummies(mechanics,
                            threshold = 50) %>%
                add_dummies(categories,
                            threshold = 1) %>%
                add_preprocessing()
        
        # recipe with all features
        all_recipe = 
                data %>%
                # remove bgg variables      
                # build recipe
                make_user_recipe(outcome = !!outcome) %>%
                add_imputation() %>%
                add_bgg_dummies() %>%
                add_preprocessing() %>%
                add_unmatched() 
        
        # all features recipe
        minimal_recipe = 
                data %>%
                # remove bgg variables      
                select(-families, -publishers, -artists, -designers) %>%
                # build recipe
                make_user_recipe(outcome = !!outcome) %>%
                add_imputation() %>%
                add_dummies(mechanics,
                            threshold = 50) %>%
                add_dummies(categories,
                            threshold = 1) %>%
                add_preprocessing()
        
                
        # recipe without designers/artists/publishers/families
        # add imputation and splines
        minimal_splines_recipe = 
                minimal_recipe %>%
                add_splines() %>%
                add_normalize() %>%
                add_zv() %>%
                check_missing(all_predictors())
        
        # recipe without designers/artists/publishers/families
        # add zero variance and checks
        minimal_trees_recipe = 
                minimal_recipe %>%
                add_zv() %>%
                check_missing(all_predictors())
        
        # recipe with all features
        # for trees
        # add imputation and splines
        all_trees_recipe = 
                all_recipe %>%
                add_zv() %>%
                check_missing(all_predictors())
        
        # recipe with all features
        # add imputation and splines
        all_splines_recipe = 
                all_recipe %>%
                add_splines() %>%
                add_normalize() %>%
                add_zv() %>%
                check_missing(all_predictors())
        
        # recipe with all features
        # add corr filter
        all_corr_recipe = 
                all_splines_recipe %>%
                add_corr() %>%
                check_missing(all_predictors())
        
        # recipe with all features
        # add pca
        all_pca_recipe = 
                all_splines_recipe %>%
                add_pca() %>%
                check_missing(all_predictors())
        
        # rm start recipes
        rm(minimal_recipe,
           all_recipe)
         
        # return all recipes
        mget( grep("_recipe$", names(environment()),  value=TRUE)) %>%
                # set names
                magrittr::set_names(., gsub("_recipe", "", names(.)))
        
}
        
        
# helper functions
# basic recipe setup
make_user_recipe = function(data,
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
                               "load_ts",
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
                               "lastmodified",
                               "user_load_ts"),
                        new_role = "id") %>%
                # set outcome variable
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

# function for extracting dummies from nominal features
add_dummies = function(recipe,
                       variable,
                       threshold = 100) {
        
        variable = enquo(variable)
        
        recipe %>%
                # tokenize 
                step_dummy_extract(!!variable,
                                   sep = ", ",
                                   other = "remove_other_field",
                                   threshold = threshold) %>%
                # remove other var
                step_rm(contains("remove_other_field"))
        
        
}

# standard dummy recipes
add_bgg_dummies = function(recipe) {
        
        recipe %>%
                # include most mechanics
                add_dummies(mechanics,
                            threshold = 50) %>%
                # include all categories
                add_dummies(categories,
                            threshold = 1) %>%
                # families at min 100
                add_dummies(families,
                            threshold = 100) %>%
                # publishers at 50
                add_dummies(publishers,
                            threshold = 50) %>%
                # designers at 25
                add_dummies(designers,
                           threshold = 25) %>%
                # artists min 50
                add_dummies(artists,
                            threshold = 50)
}

# imputation
add_imputation = function(recipe) {

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
add_preprocessing = function(recipe) {
        
        recipe %>%
                # number_mechanics
                step_mutate(number_mechanics = rowSums(dplyr::across(starts_with("mechanics")))) %>%
                # number categories
                step_mutate(number_categories = rowSums(dplyr::across(starts_with("categories")))) %>%
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

# add additional features
# specific feature for unmatched series
add_unmatched = function(recipe) {
        
        recipe %>%
                step_mutate(family_unmatched_series = dplyr::case_when(grepl("Unmatched:", name) ~ 1,
                                                                       TRUE ~ 0))
        
}


# splines
# add splines for nonlinear effects for linear models
add_splines= function(recipe) {
        
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

# normalize all numeric predictors
add_normalize = function(recipe) {
        
        recipe %>%
                step_normalize(all_numeric_predictors())
}

# add pca
add_pca = function(recipe,
                   threshold = .75) {
        
        recipe %>%
                step_pca(all_numeric_predictors(),
                         threshold = .75)
}

# add corr
add_corr = function(recipe,
                    threshold = 0.9) {
        
        recipe %>%
                step_corr(all_numeric_predictors(),
                          threshold = 0.9)
        
}


# step to remove zero variance
add_zv = function(recipe) {
        
        recipe %>%
                step_zv(all_numeric_predictors())
        
}