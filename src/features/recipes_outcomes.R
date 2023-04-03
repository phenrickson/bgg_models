# recipes for bgg outcomes

message("creating recipes...")

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
                               "averageweight",
                               "bayesaverage"),
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


# function for standard dummies from categorical data
dummy_recipe_func = function(recipe) {
        
        recipe %>%
                # include all mechanics
                dummy_extract_variable(mechanics,
                                       threshold = 1) %>%
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


# function for tokenizing specific variable with minimum number of times 
tokenize_variable = function(recipe,
                             variable,
                             max_tokens = 500,
                             min_times = 100) {
        
        variable = enquo(variable)
        
        recipe %>%
                # tokenize 
                step_tokenize(!!variable) %>%
                # token filter mechanics and categories with min n of 50
                step_tokenfilter(!!variable,
                                 max_tokens = max_tokens,
                                 min_times = min_times) %>%
                step_tf(!!variable,
                        prefix = "d")
        
}

# standard tokenization approach i'll use
tokenize_recipe_func = function(recipe) {
        
        recipe %>%
                # include all mechanics and categories
                tokenize_variable(c(mechanics, categories),
                                  min_times = 0) %>%
                # families at min 100
                tokenize_variable(families,
                                  min_times = 100) %>%
                # publishers and designers min 10
                tokenize_variable(c(publishers, designers),
                                  min_times = 10) %>%
                # artists min 25
                tokenize_variable(artists,
                                  min_times = 25)
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


# normalize
normalize_recipe_func = function(recipe) {
        
        recipe %>%
                step_normalize(all_numeric_predictors())
}
