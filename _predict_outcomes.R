
library(targets)

# packages
tar_option_set(
        packages = c("tidyverse",
                     "tidymodels",
                     "workflowsets",
                     "pins",
                     "vetiver",
                     "bggUtils"),
        # packages that your targets need to run
        format = "rds" # default storage format
        # Set other options as needed.
)

# source code
# connect to gcs boards
tar_source(here::here("src", "data", "connect_to_bigquery.R"))
tar_source(here::here("src", "data", "connect_to_gcs_boards.R"))

# functions used for preprocessing data
tar_source(here::here("src", "features", "preprocess_games.R"))

# functions used for making recipes for outcome models
tar_source(here::here("src", "features", "recipes_outcomes.R"))

# functions used in training outcome models
tar_source(here::here("src", "models", "train_outcomes.R"))

list(
        # boards
        tar_target(deployed_board,
                   get_gcs_board(board = "deployed")),
        # load raw data
        tar_target(
                name = games_raw,
                command = 
                        pins::pin_read(
                                board = pins::board_folder(here::here("data", "processed")),
                                name = "games"),
        ),
        # process
        tar_target(
                name = games_processed,
                command = 
                        games_raw %>%
                        # apply preprocessing from function
                        preprocess_games() %>%
                        # add outcome for hurdle model
                        add_users_threshold()
        ),
        # models
        tar_target(hurdle_mod,
                   vetiver::vetiver_pin_read(deployed_board,
                                             "hurdle_vetiver")),
        tar_target(averageweight_mod,
                   vetiver::vetiver_pin_read(deployed_board,
                                             "vetiver_averageweight_")),
        tar_target(average_mod,
                   vetiver::vetiver_pin_read(deployed_board,
                                             "vetiver_average_")),
        tar_target(usersrated_mod,
                   vetiver::vetiver_pin_read(deployed_board,
                                             "vetiver_usersrated")),
        # get model end train year
        tar_target(
                name = end_train_year,
                command = 
                        average_mod$metadata$user$end_train_year),
        # impute averageweight
        tar_target(
                name = games_imputed,
                command = 
                        games_processed %>%
                        impute_averageweight(
                                data = .,
                                fit = averageweight_mod
                        )
        ),
        # estimate outcomes
        # # predictions
        tar_target(
                name = games_predicted,
                command =
                        games_imputed %>%
                        mutate(bayesaverage = replace_na(bayesaverage, 5.5)) %>%
                        predict_average(
                                workflow = average_mod
                        ) %>%
                        predict_usersrated(
                                workflow = usersrated_mod
                        ) %>%
                        calculate_bayesaverage() %>%
                        mutate(.pred_averageweight = est_averageweight) %>%
                        mutate(type = case_when(yearpublished <= end_train_year ~ 'training',
                                                yearpublished > end_train_year ~ 'upcoming'))
        )
)