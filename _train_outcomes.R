# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) # Load other packages as needed. # nolint

# Set target options:
tar_option_set(
        packages = c("tidyverse",
                     "tidymodels",
                     "tidyselect",
                     "textrecipes",
                     "workflowsets",
                     "finetune",
                     "bonsai",
                     "plsmod",
                     "pins",
                     "vetiver",
                     "bggUtils"),
        # packages that your targets need to run
        format = "rds" # default storage format
        # Set other options as needed.
)

# set conflict preferences
tidymodels::tidymodels_prefer()
conflicted::conflicts_prefer(purrr::flatten)
conflicted::conflicts_prefer(purrr::set_names)

# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multicore")

# tar_make_future() configuration (okay to leave alone):
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.

# Run the R scripts in the R/ folder with your custom functions:

# connect to gcs boards
tar_source(here::here("src", "data", "connect_to_bigquery.R"))
tar_source(here::here("src", "data", "connect_to_gcs_boards.R"))

# functions used for preprocessing data
tar_source(here::here("src", "features", "preprocess_games.R"))

# functions used for making recipes for outcome models
tar_source(here::here("src", "features", "recipes_outcomes.R"))

# functions used in training outcome models
tar_source(here::here("src", "models", "train_outcomes.R"))

# source("other_functions.R") # Source other scripts as needed. # nolint

# Replace the target list below with your own:
list(
        # params
        tar_target(
                name = end_train_year,
                command = 2020,
        ),
        # validation window for retraining
        tar_target(
                name = valid_window ,
                command = 2,
        ),
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
        # linear model specs
        tar_target(
                name = linear_model_specs,
                command = linear_models(),
        ),
        # tree model specs
        tar_target(
                name = trees_model_specs,
                command = tree_models(),
        ),
        # train averageweight models
        # linear
        tar_target(
                name =  linear_averageweight_models,
                command = 
                        games_processed %>%
                        train_outcome_model(
                                data = .,
                                end_train_year = end_train_year,
                                valid_window = valid_window,
                                outcome = averageweight,
                                models = linear_model_specs,
                                metrics = reg_metrics(),
                                remove_wflow_ids = "^trees_",
                                tune_method = "tune_race_anova"
                        )
        ),
        # trees
        tar_target(
                name =  trees_averageweight_models,
                command =
                        games_processed %>%
                        train_outcome_model(
                                data = .,
                                end_train_year = end_train_year,
                                valid_window = valid_window,
                                outcome = averageweight,
                                models = trees_model_specs,
                                metrics = reg_metrics(),
                                remove_wflow_ids = "impute",
                                tune_method = "tune_race_anova"
                        )
        ),
        # impute averageweight
        # # select best mod
        tar_target(
                name = best_averageweight_method,
                command =
                        {
                                averageweight_models =
                                        list("linear" = linear_averageweight_models,
                                             "trees" = trees_averageweight_models)
                                
                                map(averageweight_models,
                                    ~ .x %>% pluck("tuning_metrics")) %>%
                                        bind_rows(.id = 'method') %>%
                                        filter(.metric == 'rmse') %>%
                                        arrange(mean) %>%
                                        head(1) %>%
                                        pull(method)
                        }
        ),
        # impute with best mod
        tar_target(
                name = games_imputed,
                command = 
                        games_processed %>%
                        impute_averageweight(
                                data = .,
                                fit = trees_averageweight_models$train_fit
                        )
        ),
        # model outcomes
        # average
        # linear
        tar_target(
                name = linear_average_models,
                command = 
                        games_imputed %>%
                        train_outcome_model(
                                data = .,
                                end_train_year = end_train_year,
                                valid_window = valid_window,
                                outcome = 'average',
                                models = linear_model_specs,
                                metrics = reg_metrics(),
                                ids = c(id_vars(), "averageweight"),
                                predictors = c(predictor_vars(), "est_averageweight"),
                                splines = c(spline_vars(), "est_averageweight"),
                                remove_wflow_ids = "^trees_",
                                tune_method = "tune_race_anova"
                        )
        ),
        # trees
        tar_target(
                name = trees_average_models,
                command = 
                        games_imputed %>%
                        train_outcome_model(
                                data = .,
                                end_train_year = end_train_year,
                                valid_window = valid_window,
                                outcome = 'average',
                                models = trees_model_specs,
                                metrics = reg_metrics(),
                                ids = c(id_vars(), "averageweight"),
                                predictors = c(predictor_vars(), "est_averageweight"),
                                splines = c(spline_vars(), "est_averageweight"),
                                remove_wflow_ids = "splines_",
                                tune_method = "tune_race_anova"
                        )
        ),
        # usersrated
        tar_target(
                name = linear_usersrated_models,
                command = 
                        games_imputed %>%
                        train_outcome_model(
                                data = .,
                                end_train_year = end_train_year,
                                valid_window = valid_window,
                                outcome = 'log_usersrated',
                                models = linear_model_specs,
                                metrics = reg_metrics(),
                                ids = c(id_vars(), "averageweight"),
                                predictors = c(predictor_vars(), "est_averageweight"),
                                splines = c(spline_vars(), "est_averageweight"),
                                remove_wflow_ids = "^trees_",
                                tune_method = "tune_race_anova"
                        )
        ),
        # # trees
        tar_target(
                name = trees_usersrated_models,
                command =
                        games_imputed %>%
                        train_outcome_model(
                                data = .,
                                end_train_year = end_train_year,
                                valid_window = valid_window,
                                outcome = 'log_usersrated',
                                models = trees_model_specs,
                                metrics = reg_metrics(),
                                ids = c(id_vars(), "averageweight"),
                                predictors = c(predictor_vars(), "est_averageweight"),
                                splines = c(spline_vars(), "est_averageweight"),
                                remove_wflow_ids = "splines_",
                                tune_method = "tune_race_anova"
                        )
        ),
        # # predictions
        tar_target(
                name = valid_preds,
                command =
                        games_imputed %>%
                        filter(yearpublished < end_train_year + valid_window) %>%
                        mutate(bayesaverage = replace_na(bayesaverage, 5.5)) %>%
                        predict_average(
                                workflow = trees_average_models$train_fit
                        ) %>%
                        predict_usersrated(
                                workflow = trees_usersrated_models$train_fit
                        ) %>%
                        calculate_bayesaverage() %>%
                        mutate(.pred_averageweight = est_averageweight) %>%
                        mutate(type = case_when(yearpublished <= end_train_year ~ 'training',
                                                yearpublished > end_train_year ~ 'valid'))
        ),
        # deploy final models with vetiver
        # averageweight
        tar_target(
                name = pin_vetiver_averageweight,
                command = {
                        
                        model = trees_averageweight_models$final_fit
                        
                        xgboost::xgb.Booster.complete(model %>% extract_fit_engine())
                        
                        model %>%
                                vetiver_model(model_name = "vetiver_averageweight_",
                                              metadata = list("end_train_year" = end_train_year + valid_window,
                                                              "training_ts" = games_processed$load_ts[1],
                                                              "training" = games_processed %>%
                                                                      preprocess_outcome(outcome = averageweight) %>% 
                                                                      filter(yearpublished <= end_train_year + valid_window))) %>%
                                vetiver_pin_write(board = deployed_board)
                }
        ),
        # average
        tar_target(
                name = pin_vetiver_average,
                command = {
                        model = trees_average_models$final_fit
                        
                        xgboost::xgb.Booster.complete(model %>% extract_fit_engine())
                        
                        model %>%
                                vetiver_model(model_name = "vetiver_average_",
                                              metadata = list("end_train_year" = end_train_year + valid_window,
                                                              "training_ts" = games_imputed$load_ts[1],
                                                              "training" = games_imputed %>%
                                                                      preprocess_outcome(outcome = average) %>% 
                                                                      filter(yearpublished <= end_train_year + valid_window))) %>%
                                vetiver_pin_write(board = deployed_board)
                }
        ),
        # usersrated
        tar_target(
                name = pin_vetiver_usersrated,
                command = {
                        model = trees_usersrated_models$final_fit
                        
                        xgboost::xgb.Booster.complete(model %>% extract_fit_engine())
                        
                        model %>%
                                vetiver_model(model_name = "vetiver_usersrated",
                                              metadata = list("end_train_year" = end_train_year + valid_window,
                                                              "training_ts" = games_imputed$load_ts[1],
                                                              "training" = games_imputed %>%
                                                                      preprocess_outcome(outcome = usersrated) %>% 
                                                                      filter(yearpublished <= end_train_year + valid_window))) %>%
                                vetiver_pin_write(board = deployed_board)
                }
        )
        # ,
        # # predict upcoming
        # tar_target(
        #         name = upcoming_preds,
        #         command = {
        #                 
        #                 averageweight_fit = vetiver_pin_read(deployed_board,
        #                                                      "vetiver_averageweight")
        #                 
        #                 average_fit = vetiver_pin_read(deployed_board,
        #                                                "vetiver_average")
        #                 
        #                 usersrated_fit = vetiver_pin_read(deployed_board,
        #                                                   "vetiver_usersrated")
        #                 
        #                 games_processed %>% %>%
        #                         filter(yearpublished > end_train_year + valid_window)
        #                         impute_averageweight(
        #                                 fit = averageweight_fit
        #                         ) 
        #                 filter(yearpublished > end_train_year + valid_window) %>%
        #                 mutate(bayesaverage = replace_na(bayesaverage, 5.5)) %>%
        #                 predict_average(
        #                         workflow = 
        #                 ) %>%
        #                 predict_usersrated(
        #                         workflow = trees_usersrated_models$train_fit
        #                 ) %>%
        #                 calculate_bayesaverage() %>%
        #                 mutate(.pred_averageweight = est_averageweight) %>%
        #                 mutate(type = case_when(yearpublished <= end_train_year ~ 'training',
        #                                         yearpublished > end_train_year ~ 'valid'))
        #                 
        # )
)