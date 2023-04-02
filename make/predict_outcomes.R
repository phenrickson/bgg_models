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
source(here::here("src", "data", "preprocess_games.R"))

# use function to preprocess nested data
games_processed = games_nested %>%
        # apply preprocessing from fuction 
        preprocess_games() %>%
        # create outcome variable for hurdle model
        mutate(users_threshold = factor(case_when(!is.na(bayesaverage) ~ 'yes',
                                                  is.na(bayesaverage) ~ 'no'),
                                        levels = c('no', 'yes')))

# upcoming games
games_upcoming = games_processed %>%
        filter(yearpublished > 2021)

# functions ---------------------------------------------------------------

# augment games with estimated averageweight
impute_averageweight = function(averageweight_mod,
                                data) {
        
        averageweight_mod %>%
                augment(data) %>%
                rename(est_averageweight = .pred)
        
}

# predict all bgg outcomes with mods
# function to predict
predict_bgg_outcomes = function(data,
                                averageweight_mod,
                                average_mod,
                                usersrated_mod,
                                pivot = T,
                                ratings = 2000) {
        
        
        # predict with hurdle model
        preds = 
                data %>%
                # impute averageweight
                impute_averageweight(averageweight_mod,
                                     data =.) %>%
                mutate(.pred_averageweight = est_averageweight,
                       .actual_averageweight = averageweight) %>%
                # predict with average
                augment(average_mod,
                        new_data = .) %>%
                rename(.pred_average = .pred) %>%
                mutate(.actual_average = average) %>%
                # predict with usersrated
                augment(usersrated_mod,
                        new_data = .) %>%
                rename(.pred_usersrated = .pred) %>%
                mutate(.pred_usersrated = exp(.pred_usersrated)) %>%
                # round
                mutate(.pred_usersrated = plyr::round_any(.pred_usersrated, 50)) %>%
                mutate(.actual_usersrated = usersrated) %>%
                # estimate bayesaverage putting these together
                mutate(.pred_bayesaverage =
                               # numerator
                               ((ratings * 5.5) + (.pred_average * .pred_usersrated)) /
                               # denominator
                               (ratings + .pred_usersrated)
                ) %>%
                # get actual
                rename(.actual_bayesaverage = bayesaverage)
        
        if (pivot == T) {
                
                # get predictions
                preds = 
                        preds %>%
                        select(yearpublished,
                               game_id,
                               name,
                               starts_with(".pred")) %>%
                        pivot_longer(cols = -c(yearpublished,
                                               game_id,
                                               name),
                                     names_to = c("outcome"),
                                     values_to = c(".pred")) %>%
                        mutate(outcome = gsub(".pred_", "", outcome)) %>%
                        left_join(.,
                                  preds %>%
                                          select(yearpublished,
                                                 game_id,
                                                 name,
                                                 starts_with(".actual")) %>%
                                          pivot_longer(cols = -c(yearpublished,
                                                                 game_id,
                                                                 name),
                                                       names_to = c("outcome"),
                                                       values_to = c(".actual")) %>%
                                          mutate(outcome = gsub(".actual_", "", outcome)),
                                  by = c("yearpublished", "game_id", "name", "outcome")
                        )
        }
        
}

# models ------------------------------------------------------------------


# load models

# hurdle from vetiver
hurdle_fit =
        vetiver::vetiver_pin_read(board = deployed_board,
                 name = 'hurdle_vetiver')

# averageweight
averageweight_fit =
        pin_read(board = models_board,
                  name = 'averageweight_fit')

# average
average_fit =
        pin_read(board = models_board,
                 name = 'average_fit')

# usersrated
usersrated_fit =
        pin_read(board = models_board,
                 name = 'usersrated_fit')

# bind together into one df
outcome_models = tribble(~outcome, ~model,
                         "averageweight", averageweight_fit,
                         "average", average_fit,
                         "usersrated", usersrated_fit)

# get predictions for upcoming games
game_preds = 
        # predict outcomes
        games_upcoming %>%
        predict_bgg_outcomes(data = .,
                             averageweight_fit,
                             average_fit,
                             usersrated_fit) %>%
        # add in hurdle predictions
        left_join(.,
                  augment(hurdle_fit,
                          games_upcoming) %>%
                          select(game_id, name, usersrated, .pred_yes),
                  by = c("game_id", "name"))

# top for 2023
game_preds %>%
        filter(.pred_yes > .15) %>%
        filter(outcome == 'bayesaverage') %>%
        filter(yearpublished == 2022) %>%
        arrange(desc(.pred)) %>%
        mutate(rank = row_number()) %>%
        view()

game_preds %>%
        filter(.pred_yes > .25) %>%
        filter(outcome == 'bayesaverage') %>%
        filter(yearpublished == 2021) %>%
        arrange(desc(.pred)) %>%
        mutate(rank = row_number()) %>%
        view()

game_preds %>%
        filter(.pred_yes > .25) %>%
        select(yearpublished, game_id, name, outcome, .pred) %>%
        spread(outcome, .pred) %>%
        ggplot(aes(x=average,
                   y=usersrated))+
        geom_point(size = 0.5,
                   position = position_jitternormal(sd_y = 0.05))+
        scale_y_log10()+
        theme_minimal()+
        facet_wrap(yearpublished ~.)

