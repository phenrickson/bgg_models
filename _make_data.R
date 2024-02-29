# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)

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
tar_source(here::here("src", "models", "train_outcomes.R"))
tar_source(here::here("src", "reports", "user_collection_report.R"))
tar_source(here::here("src", "reports", "outcome_reports.R"))

pin_load_hash = function(board, name) {
        
        pin =
                pins::pin_meta(
                        board = board,
                        name = name)
        
        pin$pin_hash
        
}

pin_load_version = function(board, name) {
        
        pins::pin_versions(board, 
                           name)%>% 
                filter(created == max(created)) %>% 
                head(1) %>%
                pull(version)
}

# load and process games
list(
        # tables from bigquery
        # boards for storing processed data
        tar_target(data_board,
                   pins::board_folder(here::here("data", "processed"))),
        # board for deployed models
        tar_target(deployed_board,
                   get_gcs_board(board = "deployed"),
                   cue = tar_cue_age(
                           name = deployed_board,
                           age = as.difftime(7, units = "days")
                   )
        ),
        # board for saving data to gcs
        tar_target(gcs_board,
                   get_gcs_board("data"),
                   cue = tar_cue_age(
                           name = gcs_board,
                           age = as.difftime(7, units = "days")
                   )
        ),
        # tables to load
        tar_target(games_table, 
                   load_table(table_name = "games"),
                   cue = tar_cue_age(
                           name = games_table,
                           age = as.difftime(7, units = "days")
                   )
        ),
        # categorical
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
                   cue = tar_cue_age(
                           name = categorical_tables,
                           age = as.difftime(7, units = "days")
                   )
        ),
        #  cue = tar_cue(mode = "always")),
        # transform
        # apply standardized transformations
        tar_target(games_transformed,
                   transform_games(data = games_table)),
        # join transformed with categorical tables
        tar_target(games, 
                   left_join(games_transformed, categorical_tables, by = c("game_id")
                   )
        ),
        tar_target(
                name = games_processed,
                command = 
                        games%>%
                        # apply preprocessing from function
                        preprocess_games() %>%
                        # add outcome for hurdle model
                        add_users_threshold()
        ),
        # load models
        tar_map(
                values = data.frame(models = 
                                            c("vetiver_averageweight_",
                                              "vetiver_average_",
                                              "vetiver_usersrated",
                                              "hurdle_vetiver")),
                # get pin version
                tar_target(
                        name = pin_version,
                        command = 
                                pin_load_version(
                                        deployed_board,
                                        models)
                ),
                # read model
                tar_target(
                        name = model,
                        command = 
                                vetiver::vetiver_pin_read(deployed_board,
                                                          models,
                                                          version = pin_version,
                                                          check_renv = T)
                )
        ),
        # get model end train year
        tar_target(
                name = end_train_year,
                command = 
                        model_vetiver_average_$metadata$user$end_train_year),
        # impute averageweight
        tar_target(
                name = games_imputed,
                command = 
                        games_processed %>%
                        impute_averageweight(
                                data = .,
                                fit = model_vetiver_averageweight_
                        ) %>%
                        mutate(pred_hurdle = NA)
                # %>%
                #         predict_hurdle(
                #                 workflow = model_hurdle_vetiver
                #         )
        ),
        # estimate outcomes
        # # predictions
        tar_target(
                name = games_predicted,
                command =
                        games_imputed %>%
                        mutate(bayesaverage = replace_na(bayesaverage, 5.5)) %>%
                        predict_average(
                                workflow = model_vetiver_average_
                        ) %>%
                        predict_usersrated(
                                workflow = model_vetiver_usersrated
                        ) %>%
                        calculate_bayesaverage() %>%
                        mutate(.pred_averageweight = est_averageweight) %>%
                        mutate(type = case_when(yearpublished <= end_train_year ~ 'training',
                                                yearpublished > end_train_year ~ 'upcoming'))
        ),
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
        tar_target(local_pin_games_predicted,
                   pins::pin_write(x = games_predicted,
                                   board = data_board,
                                   name = "games_predicted",
                                   versioned = T,
                                   tags = c("data", "averageweight", "average", "usersrated", "bayesaverage"))),
        # pin to gcs board
        tar_target(gcs_pin_games, 
                   pins::pin_write(x = games,
                                   board = gcs_board,
                                   name = "games_info",
                                   versioned = T,
                                   tags = "data")),
        tar_target(gcs_pin_games_imputed,
                   pins::pin_write(x = games_imputed,
                                   board = gcs_board,
                                   name = "games_imputed",
                                   versioned = T,
                                   tags = c("data", "averageweight", "hurdle"))),
        tar_target(gcs_pin_games_predicted,
                   pins::pin_write(x = games_predicted,
                                   board = gcs_board,
                                   name = "games_predicted",
                                   versioned = T,
                                   tags = c("data", "averageweight", "average", "usersrated", "bayesaverage")))
        # reports
        # top predictions for 
        # tar_render(
        #         name = report_upcoming_predictions,
        #         path = here::here("notebooks", "upcoming_predictions.Rmd")
        # )
        
)
