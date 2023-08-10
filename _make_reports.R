# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)
library(crew)

# library(tarchetypes) # Load other packages as needed. # nolint
options(clustermq.scheduler = "multicore")

# packages ----------------------------------------------------------------

# Set target options:
# packages
tar_option_set(packages = c("tidyverse",
                            "tidymodels",
                            "bonsai",
                            "bggUtils",
                            "pins",
                            "vetiver",
                            "gt",
                            "gtExtras",
                            "DT"),
               controller = crew_controller_local(workers = parallel::detectCores())
)

# src ------------------------------------------------------------------

# gcs and boards
tar_source(here::here("src", "data", "connect_to_bigquery.R"))
tar_source(here::here("src", "data", "connect_to_gcs_boards.R"))

# functions used for preprocessing
tar_source(here::here("src", "features", "preprocess_games.R"))

# recipes for user models
tar_source(here::here("src", "features", "recipes_user.R"))

# functions for training user models
tar_source(here::here("src", "models", "train_user_models.R"))

# functions for visualizing workflows
tar_source(here::here("src", "visualizations", "viz_workflows.R"))

# functions for user report
tar_source(here::here("src", "reports", "user_collection_report.R"))


# usernames to run dynamic branches
usernames = data.frame(usernames = c("mrbananagrabber",
                                     "GOBBluth89",
                                     "Gyges"))

list(
        # data 
        tar_target(data_board,
                   get_gcs_board('data')
        ),
        # imputed games for model training
        tar_target(
                name = games,
                command = 
                        pin_read(data_board,
                                 name = "games_imputed")
        ),
        # additional info for games to be used in reports
        tar_target(
                name = games_info,
                command = 
                        pin_read(data_board,
                                 name = "games_info")
                
        ),
        # collections
        tar_map(
                values = usernames,
                tar_target(
                        name = user_collection,
                        command =
                                usernames %>%
                                load_user_collection(username = .),
                        cue = tar_cue_age(
                                as.difftime(30, units = "days")
                        )
                )
        )
        # # collections
        # tar_map(
        #         values = user_collection,
        #         tar_target(
        #                 name = user_output,
        #                 command =
        #                         user_collection %>%
        #                         train_user_model(user_collection = .,
        #                                          bgg_games = games,
        #                                          outcome = 'own',
        #                                          end_train_year = 2021,
        #                                          valid_window = 2,
        #                                          retrain_window = 0,
        #                                          model_specs = c('glmnet','lightgbm'),
        #                                          tune_metric = 'mn_log_loss')
        #         )
        # )
)
