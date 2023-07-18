# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)

# library(tarchetypes) # Load other packages as needed. # nolint

# Set target options:
# packages
tar_option_set(packages = c("bigrquery",
                            "tidyverse",
                            "pins",
                            "tidymodels",
                            "vetiver",
                            "googleCloudStorageR",
                            "bggUtils"))


# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multicore")

# tar_make_future() configuration (okay to leave alone):
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.

# Run the R scripts in the R/ folder with your custom functions:

# connecting to bigquery
tar_source(here::here("src", "data", "connect_to_bigquery.R"))
tar_source(here::here("src", "data", "connect_to_gcs_boards.R"))
tar_source(here::here("src", "data", "load_games.R"))
tar_source(here::here("src", "features", "preprocess_games.R"))
tar_source(here::here("src", "models", "impute_games.R"))

# tar
list(
        # tables from bigquery
        # boards for storing processed data
        tar_target(data_board,
                   pins::board_folder(here::here("data", "processed"))),
        tar_target(gcs_board,
                   get_gcs_board("data")),
        # tables to load
        tar_target(games_table, 
                   load_table(table_name = "games"),
                   cue = tar_cue(mode = "always")),
        tar_target(categorical_tables, 
                   load_categorical_tables(tables = c("descriptions",
                                                      "categories",
                                                      "mechanics",
                                                      "designers",
                                                      "artists",
                                                      "families",
                                                      "publishers",
                                                      "implementations",
                                                      "compilations")),
                   cue = tar_cue(mode = "always")),
        # transform
        tar_target(games_transformed,
                   transform_games(data = games_table)),
        tar_target(games, 
                   left_join(games_transformed, categorical_tables, by = c("game_id"))),
        # models
        tar_target(deployed_board,
                   get_gcs_board(board = "deployed")),
        tar_target(hurdle_mod,
                   vetiver::vetiver_pin_read(deployed_board,
                                             "hurdle_vetiver")),
        tar_target(averageweight_mod,
                   vetiver::vetiver_pin_read(deployed_board,
                                             "averageweight_vetiver")),
        # impute
        tar_target(games_imputed,
                   games %>%
                           preprocess_games(data = .) %>%
                           impute_averageweight(
                                   games = .,
                                   model = averageweight_mod) %>%
                           impute_hurdle(
                                   games = .,
                                   model = hurdle_mod)),
        # pin to data board
        tar_target(local_pin_games, 
                   pins::pin_write(x = games,
                                   board = data_board,
                                   name = "games",
                                   versioned = T,
                                   tags = "data")),
        tar_target(local_pin_games_imputed,
                   pins::pin_write(x = games_imputed,
                                   board = data_board,
                                   name = "games_imputed",
                                   versioned = T,
                                   tags = c("data", "averageweight", "hurdle"))),
        # pin to gcs board
        tar_target(gcs_pin_games, 
                   pins::pin_write(x = games,
                                   board = gcs_board,
                                   name = "games",
                                   versioned = T,
                                   tags = "data")),
        tar_target(gcs_pin_games_imputed,
                   pins::pin_write(x = games_imputed,
                                   board = gcs_board,
                                   name = "games_imputed",
                                   versioned = T,
                                   tags = c("data", "averageweight", "hurdle")))
        
)