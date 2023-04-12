
# gcs ---------------------------------------------------------------------


# connect to gcs 
source(here::here("src", "data", "connect_to_gcs.R"))


# packages ----------------------------------------------------------------


# tidyverse packages
library(tidyverse)
library(tidymodels)
tidymodels_prefer()

# my own package
library(bggUtils)


# data --------------------------------------------------------------------


# load games nested
source(here::here("src","data","load_games_data.R"))

# functions for standardized preprocessing
source(here::here("src", "features", "preprocess_games.R"))



# models ------------------------------------------------------------------


# hurdle model
hurdle_fit = vetiver::vetiver_pin_read(deployed_board,
                                       "hurdle_vetiver")

# load averageweight model
averageweight_fit = vetiver::vetiver_pin_read(deployed_board,
                                      "averageweight_vetiver")

# use function to preprocess nested data
games_imputed = games_nested %>%
        # apply preprocessing from fuction 
        preprocess_games() %>%
        # create outcome variable for hurdle model
        mutate(users_threshold = factor(case_when(!is.na(bayesaverage) ~ 'yes',
                                                  is.na(bayesaverage) ~ 'no'),
                                        levels = c('no', 'yes'))) %>%
        # predict
        augment(x=averageweight_fit,
                new_data = .) %>%
        # rename 
        rename(est_averageweight = .pred) %>%
        # predict with hurdle
        augment(x = hurdle_fit,
                new_data = .,
                type = 'prob') %>%
        # rename
        rename(pred_hurdle = .pred_yes) %>%
        # remove 
        select(-.pred_no,
               -.pred_class)


# save
processed_board = pins::board_folder(here::here("data", "processed"))

# save as
games_imputed %>%
        pins::pin_write(board = processed_board,
                        type = 'rds',
                        name = 'games_imputed')

