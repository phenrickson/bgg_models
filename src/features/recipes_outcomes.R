# recipes for bgg outcomes

predictor_vars= function(vars = 
                                 c("minage",
                                   "minplayers",
                                   "maxplayers",
                                   "playingtime",
                                   "minplaytime",
                                   "maxplaytime",
                                   "categories",
                                   "mechanics",
                                   "families",
                                   "designers",
                                   "artists",
                                   "publishers")) {vars}

id_vars = function(vars = 
                           c("game_id",
                             "name",
                             "yearpublished",
                             "averageweight",
                             "average",
                             "usersrated",
                             "image",
                             "description")) {vars}

spline_vars = function(vars = c("year",
                                "number_mechanics",
                                "number_categories")) {vars}

discrete_vars = function(vars = spline_vars()) {vars}


# basic recipe setup
make_recipe = function(data,
                       outcome,
                       ids = id_vars(),
                       predictors = predictor_vars(),
                       ...) {
        
        recipe(x=data) %>%
                # set ids
                update_role(
                        any_of(ids),
                        new_role = "id"
                ) %>%
                # set predictors
                update_role(
                        any_of(predictors),
                        new_role = "predictor"
                ) %>%
                # set outcome
                update_role(
                        {{ outcome }},
                        new_role = "outcome"
                ) %>%
                # set anything else as extra
                update_role(
                        -has_role("id"),
                        -has_role("predictor"),
                        -has_role("outcome"),
                        new_role = "extra"
                ) 
        
}


make_outcome_recipes = function(recipe,
                                spline_vars = c("year",
                                                "number_mechanics",
                                                "number_categories"),
                                corr_threshold = 0.95) {
                list(
                        "trees" = 
                                recipe,
                        # "impute_normalize" = 
                        #         recipe %>%
                        #         add_imputation() %>%
                        #         add_zv() %>%
                        #         add_corr(my_threshold = corr_threshold) %>%
                        #         add_normalize(),
                        "impute_splines" = 
                                recipe %>%
                                add_imputation() %>%
                                add_splines(vars = spline_vars) %>%
                                add_zv() %>%
                                add_corr(my_threshold = corr_threshold) %>%
                                add_normalize()
                        # ,
                        # "impute_pca" = 
                        #         recipe %>%
                        #         add_imputation() %>%
                        #         add_splines(vars = spline_vars) %>%
                        #         add_zv() %>%
                        #         add_corr(my_threshold = corr_threshold) %>%
                        #         add_normalize() %>%
                        #         add_pca()
                )
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
add_bgg_dummies = function(recipe,
                           mechanics_threshold = 50,
                           categories_threshold = 1,
                           families_threshold = 100,
                           publishers_threshold = 25,
                           designers_threshold = 25,
                           artists_threshold = 50
) {
        
        recipe %>%
                # include most mechanics
                add_dummies(mechanics,
                            threshold = mechanics_threshold) %>%
                # include all categories
                add_dummies(categories,
                            threshold = categories_threshold) %>%
                # families at min 100
                add_dummies(families,
                            threshold = families_threshold) %>%
                # publishers at 50
                add_dummies(publishers,
                            threshold = publishers_threshold) %>%
                # designers at 25
                add_dummies(designers,
                            threshold = designers_threshold) %>%
                # artists min 50
                add_dummies(artists,
                            threshold = artists_threshold)
}


# basic recipe setup
build_recipe = function(data,
                        outcome,
                        ids = set_id_vars(),
                        predictors = set_predictor_vars(),
                        ...) {
        
        recipe(x=data) %>%
                # set ids
                update_role(
                        any_of(ids),
                        new_role = "id"
                ) %>%
                # set predictors
                update_role(
                        any_of(predictors),
                        new_role = "predictor"
                ) %>%
                # set outcome
                update_role(
                        {{ outcome }},
                        new_role = "outcome"
                ) %>%
                # set anything else as extra
                update_role(
                        -has_role("id"),
                        -has_role("predictor"),
                        -has_role("outcome"),
                        new_role = "extra"
                ) 
        
}


# standardized preprocessing
add_preprocessing = function(recipe) {
        
        recipe %>%
                # indicate missingness in numeric features
                step_indicate_na(all_numeric_predictors(),
                                 prefix = "missing") %>%
                # make time per player variable
                step_mutate(time_per_player = playingtime/ maxplayers) %>% 
                # remove zero variance predictors
                step_zv(all_predictors()) %>%
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


# normalize all numeric predictors
add_normalize = function(recipe) {
        
        recipe %>%
                step_normalize(all_numeric_predictors())
}


# add pca
add_pca = function(recipe,
                   ...) {
        
        recipe %>%
                step_pca(all_numeric_predictors(),
                         ...)
}

# add corr
add_corr = function(recipe,
                    my_threshold = 0.9) {
        
        recipe %>%
                step_corr(all_numeric_predictors(),
                          threshold = my_threshold)
        
}


# step to remove zero variance
add_zv = function(recipe) {
        
        recipe %>%
                step_zv(all_numeric_predictors())
        
}

# discretize variables with cart
add_discrete = function(recipe,
                        outcome,
                        vars = c("year",
                                 "number_mechanics",
                                 "number_categories")) {
        
        outcome = enquo(outcome)
        
        # embed categorical feature via mixed
        step_discretize_feature = function(recipe,
                                       feature,
                                       outcome) {
                
                feature = enquo(feature)
                outcome = enquo(outcome)
                
                recipe %>%
                        embed::step_discretize_cart(
                                !!feature,
                                outcome = vars(!!outcome))
        }
        
        for (i in 1 :length(vars)) {
                
                recipe = recipe %>%
                        step_discretize_feature(feature = !!vars[i],
                                                outcome = !!outcome)
                        # add_role(!!vars[i],
                        #          new_role = "discrete")
                                 
        }
        
        recipe %>%
                step_dummy(all_nominal_predictors())
        # recipe %>%
        #         step_novel(any_of(has_role("discrete"))) %>%
        #         step_dummy(has_role("discrete"))
        
}

# splines
# add splines for nonlinear effects for linear models
add_splines= function(recipe,
                      vars = c("year",
                               "number_mechanics",
                               "number_categories"),
                      degree = 5) {
        
        
        step_spline_feature = function(recipe,
                                       feature,
                                       ...) {
                
                feature = enquo(feature)
                
                recipe %>%
                        # add splines
                        step_ns(
                                !!feature,
                                deg_free = degree)
        }
        
        for (i in 1 :length(vars)) {
                
                recipe = recipe %>%
                        step_spline_feature(feature = !!vars[i])
                
        }
        
        recipe
        
        
}

