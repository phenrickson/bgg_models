
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
                .) %>%
        # rename 
        rename(est_averageweight = .pred)

# save
processed_board = pins::board_folder(here::here("data", "processed"))

# save as
games_imputed %>%
        pins::pin_write(board = processed_board,
                        name = 'games_imputed')

