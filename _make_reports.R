# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)
library(crew)
library(clustermq)

# library(tarchetypes) # Load other packages as needed. # nolint
options(clustermq.scheduler = "multicore")

# tar_make_future() configuration (okay to leave alone):
future::plan(future.callr::callr)

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
               format = "qs"
               #  controller = crew_controller_local(workers = parallel::detectCores())
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

# rmarkdown settings
source(here::here("src", "reports", "knitr_settings.R"))

# # usernames to run dynamic branches
usernames = data.frame(usernames = c("mrbananagrabber"))

# "Gyges"))
# "GOBBluth89",
# "Quinns"))

future::plan(future::multisession, workers = 4)

set.seed(1999)
list(
        # connect to gcs
        # data board
        tar_target(
                name = data_board,
                format = "rds",
                command = 
                        pins::board_gcs(
                                bucket = gcs_bucket(),
                                prefix = paste0('data', "/"),
                                versioned = T)
        ),
        # retreive data from pins
        # imputed games for model training
        tar_target(
                name = games,
                command =
                        pins::pin_read(data_board,
                                       name = "games_imputed")
        ),
        # additional info for games to be used in reports
        tar_target(
                name = games_info,
                command =
                        pin_read(data_board,
                                 name = "games_info"),
        ),
        # parameters
        tar_plan(
                end_train_year = 2020,
                valid_window = 2,
                retrain_window = 2,
                outcome = 'own',
                model_specs = c('glmnet', 'lightgbm')
        ),
        # run over usernames
        tar_map(
                # load collection
                values =  usernames,
                tar_target(
                        name = user_collection,
                        command = 
                                usernames %>%
                                load_user_collection()
                ),
                # train user model
                tar_target(
                        name = user_results,
                        command = 
                                user_collection %>%
                                train_user_model(user_collection = .,
                                                 bgg_games = games,
                                                 outcome = outcome,
                                                 end_train_year = end_train_year,
                                                 valid_window = valid_window,
                                                 retrain_window = retrain_window,
                                                 model_specs = model_specs,
                                                 tune_metric = 'mn_log_loss',
                                                 save_workflow = F)
                ),
                # render user report
                tar_render(
                        name = user_report,
                        path = here::here("notebooks", "test.Rmd"),
                        params = list(username = usernames,
                                      collection = user_collection,
                                      results = user_results),
                        output_dir = here::here("reports"),
                        output_file = paste(usernames, outcome, end_train_year, sep = "_")
                )
        )
)
# tar_target(
#         name = user_results,
#         command = 
#                 user_collection %>%
#                 train_user_model(user_collection = .,
#                                  bgg_games = games,
#                                  outcome = 'own',
#                                  end_train_year = 2021,
#                                  valid_window = 2,
#                                  retrain_window = 0,
#                                  model_specs = c('glmnet','lightgbm'),
#                                  tune_metric = 'mn_log_loss')
#                 # )
#         )
# )