# what: estimate average weight for games with previously trained averageweight model

# packages ----------------------------------------------------------------

# suppressPackageStartupMessages({
#         
#         # tidyverse packages
#         library(tidyverse)
#         library(tidymodels)
#         # set conflict preferences
#         tidymodels_prefer()
#         
#         # my own package
#         library(bggUtils)
#         
# })

# data --------------------------------------------------------------------

# # load local games nested
# source(here::here("src","data","load_local_processed_games.R"))
# 
# # functions for standardized preprocessing
# source(here::here("src", "features", "make_games_features.R"))


# # models ------------------------------------------------------------------
# 
# 
# # load hurdle model
# hurdle_fit = vetiver::vetiver_pin_read(deployed_board,
#                                        "hurdle_vetiver")
# 
# # load averageweight model
# averageweight_fit = vetiver::vetiver_pin_read(deployed_board,
#                                       "averageweight_vetiver")



# functions ---------------------------------------------------------------


# estimate averageweight
impute_averageweight =
        function(games,
                 model) {
                
                games %>%
                        # # apply preprocessing from fuction 
                        # preprocess_games() %>%
                        # create outcome variable for hurdle model
                        mutate(users_threshold = factor(case_when(!is.na(bayesaverage) ~ 'yes',
                                                                  is.na(bayesaverage) ~ 'no'),
                                                        levels = c('no', 'yes'))) %>%
                        # predict
                        augment(x=model,
                                new_data = .) %>%
                        # rename 
                        rename(est_averageweight = .pred)
        }

# estimate probability game will get over hurdle of 100 user ratings
impute_hurdle = 
        function(games,
                 model) {
                
                games %>%
                        # predict with hurdle
                        augment(x = model,
                                new_data = .,
                                type = 'prob') %>%
                        # rename pred to hurdle
                        rename(pred_hurdle = .pred_yes) %>%
                        # remove extraneous predictions
                        select(-.pred_no,
                               -.pred_class)
                
        }
                
                
# # use function to preprocess nested data
# games_imputed = games_nested %>%
#         # apply preprocessing from fuction 
#         preprocess_games() %>%
#         # create outcome variable for hurdle model
#         mutate(users_threshold = factor(case_when(!is.na(bayesaverage) ~ 'yes',
#                                                   is.na(bayesaverage) ~ 'no'),
#                                         levels = c('no', 'yes'))) %>%
#         # predict
#         augment(x=averageweight_fit,
#                 new_data = .) %>%
#         # rename 
#         rename(est_averageweight = .pred) %>%
#         # predict with hurdle
#         augment(x = hurdle_fit,
#                 new_data = .,
#                 type = 'prob') %>%
#         # rename
#         rename(pred_hurdle = .pred_yes) %>%
#         # remove 
#         select(-.pred_no,
#                -.pred_class)
# 
# 
# # local processed folder
# processed_board = 
#         pins::board_folder(here::here("data", "processed"))
# 
# # save imputed games
# games_imputed %>%
#         pins::pin_write(board = processed_board,
#                         type = 'rds',
#                         name = 'games_imputed')
