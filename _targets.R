# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) # Load other packages as needed.

# authenticate to gcs
googleCloudStorageR:::set_scopes()

# authenticate
googleCloudStorageR::gcs_auth(json_file = Sys.getenv("GCS_AUTH_FILE"))

# set bucket
suppressMessages({googleCloudStorageR::gcs_global_bucket("bgg_models")})

# packages
tar_option_set(
    packages = c("dplyr",
                 "purrr",
                 "bggUtils",
                 "tidymodels",
                 "glmnet",
                 "lightgbm",
                 "bonsai",
                 "gert",
                 "quarto",
                 "qs"),
    repository = "local",
    resources = tar_resources(
        gcp = tar_resources_gcp(
            bucket = "bgg_models",
            prefix = 'bgg_models'
        )
    ),
    memory = "transient",
    format = "qs"
)
# # create local model board
model_board = pins::board_gcs("bgg_models",
                              prefix = "model/bgg/",
                              versioned = T)

# functions
suppressMessages({tar_source("src")})

# # model board
# model_board = gcs_model_board()

# parameters for targets
end_train_year = 2020
valid_years = 2
retrain_years = valid_years - 1

# Replace the target list below with your own:
# targets definine data splitting strategy for training and validation
list(
    tar_target(
        games_raw,
        command = 
            load_games(generation = "1708980495752949"),
        packages = c("googleCloudStorageR")
    ),
    tar_target(
        name = games_prepared,
        command = 
            games_raw |>
            prepare_games()
    ),
    # create train, validation, testing split based on year
    tar_target(
        split,
        command = 
            games_prepared |>
            create_year_split(
                end_train_year = end_train_year,
                valid_years = valid_years
            )
    ),
    # create model to predict hurdle
    tar_target(
        hurdle_tuned,
        command = 
            split |>
            training() |>
            add_hurdle() |>
            train_outcome_wflow(outcome = 'hurdle',
                                weights = 0,
                                ratings = 0,
                                valid_years = valid_years,
                                recipe = recipe_hurdle,
                                model_spec = lightgbm_spec() |> 
                                    set_mode("classification"),
                                metrics = tune_class_metrics(),
                                grid = lightgbm_grid()),
    ),
    # get hurdle results and thresholds
    tar_target(
        hurdle_results,
        command = 
            hurdle_tuned |>
            finalize_outcome_wflow(metric = 'roc_auc') |>
            extract_tune_preds() |>
            assess_class_threshold(class_metrics = my_class_metrics()),
        packages = c("bonsai", "lightgbm")
    ),
    tar_target(
        hurdle_threshold,
        command = 
            hurdle_results |>
            filter(.metric == 'f2_meas') |>
            slice_max(.estimate, n = 1, with_ties = F) |>
            pull(threshold)
    ),
    # finalize model
    tar_target(
        hurdle_fit,
        command = 
            hurdle_tuned |>
            finalize_outcome_wflow(metric = 'mn_log_loss') |>
            fit_outcome_wflow(data = 'all') |>
            bundle_wflow()
    ),
    # identify threshold for hurdle model based on validation set
    # define tuning split given outcome
    tar_target(
        # build workflow
        averageweight_tuned,
        command = 
            split |>
            training() |>
            train_outcome_wflow(outcome = 'averageweight',
                                weights = 5,
                                valid_years = valid_years,
                                recipe = recipe_trees,
                                model_spec = lightgbm_spec(),
                                grid = lightgbm_grid()),
        packages = c("bonsai", "lightgbm")
    ),
    # now fit model
    tar_target(
        averageweight_fit,
        command = 
            averageweight_tuned |>
            finalize_outcome_wflow() |>
            fit_outcome_wflow(data = 'all') |>
            bundle_wflow()
    ),
    # use model to impute averageweight for training set
    tar_target(
        name = training_imputed,
        command =
            split |>
            training() |>
            impute_averageweight(model = bundle::unbundle(averageweight_fit))
    ),
    # # now train average and usersrated models
    tar_target(
        name = average_tuned,
        command =
            training_imputed |>
            train_outcome_wflow(outcome = 'average',
                                ratings = 25,
                                valid_years = valid_years,
                                recipe = recipe_linear,
                                model_spec = glmnet_spec(),
                                grid = glmnet_grid(),
                                ids = id_vars(),
                                predictors = c("est_averageweight", predictor_vars()),
                                splines = c("est_averageweight", spline_vars()))
    ),
    # now train usersrated
    tar_target(
        name = usersrated_tuned,
        command = 
            training_imputed |>
            train_outcome_wflow(outcome = 'usersrated',
                                ratings = 25,
                                valid_years = valid_years,
                                recipe = recipe_linear,
                                model_spec = glmnet_spec(),
                                grid = glmnet_grid(),
                                ids = id_vars(),
                                predictors = c("est_averageweight", predictor_vars()),
                                splines = c("est_averageweight", spline_vars()))
    ),
    # # extract tuning plots
    # tar_target(
    #     name = tuning_plots,
    #     command =
    #         bind_rows(averageweight_tuned,
    #                   average_tuned,
    #                   usersrated_tuned) |>
    #         get_tuning_plots()
    # ),
    # fit models to whole of training
    # average
    tar_target(
        name = average_fit,
        command = 
            average_tuned |>
            finalize_outcome_wflow() |>
            fit_outcome_wflow(data = 'all') |>
            bundle_wflow()
    ),
    # usersrated
    tar_target(
        name = usersrated_fit,
        command = 
            usersrated_tuned |>
            finalize_outcome_wflow() |>
            fit_outcome_wflow(data = 'all') |>
            bundle_wflow()
    ),
    ## now predict the validation set
    tar_target(
        name = validation_imputed,
        command =
            split |>
            validation() |>
            impute_averageweight(model = bundle::unbundle(averageweight_fit)) |>
            predict_hurdle(model = bundle::unbundle(hurdle_fit),
                           threshold = hurdle_threshold)
    ),
    # predict validation set
    # predict with average + usersrated
    tar_target(
        name = valid_predictions,
        command = 
            validation_imputed |>
            predict_bayesaverage(average_model = bundle::unbundle(average_fit),
                                 usersrated_model = bundle::unbundle(usersrated_fit))
    ),
    # assess hurdle results
    tar_target(
        valid_hurdle_metrics,
        command = 
            {
                class_metrics = my_class_metrics()
                valid_predictions |>
                    group_by(outcome = 'hurdle') |>
                    class_metrics(
                        truth = hurdle,
                        estimate = .pred_hurdle_class,
                        event_level = 'second'
                    )
            }
    ),
    tar_target(
        hurdle_tracking,
        command = 
            valid_hurdle_metrics |>
            mutate(.estimate = round(.estimate, 4)) |>
            write.csv(file = 'targets-runs/hurdle.csv'),
        format = 'file'
    ),
    # assess results
    tar_target(
        name = valid_metrics,
        command =
            valid_predictions |>
            assess_outcomes_by_threshold(metrics = my_reg_metrics(),
                                         groups = c("outcome"),
                                         threshold = c(0, 25))
    ),
    tar_target(
        name = details,
        command = 
            bind_rows(average_tuned,
                      usersrated_tuned,
                      averageweight_tuned) |>
            finalize_outcome_wflow() |>
            select(outcome, wflow_id, params)
    ),
    tar_target(
        name = tracking,
        command = 
            write_tracking(metrics = valid_metrics,
                           details = details,
                           file = "targets-runs/tracking.csv"),
        format = "file"
    ),
    ## finalize models and predict test set
    # get training and validation
    tar_target(
        name = training_and_validation,
        command =
            bind_rows(
                training_imputed,
                validation_imputed
            ) |>
            # only add in one year from the validation set
            filter(yearpublished <= end_train_year + retrain_years)
    ),
    ## finalize and predict
    tar_target(
        name = averageweight_final,
        command = 
            averageweight_fit |>
            bundle::unbundle() |>
            finalize_model(data = training_and_validation)
    ),
    tar_target(
        name = average_final,
        command = 
            average_fit |>
            bundle::unbundle() |>
            finalize_model(data = training_and_validation)
    ),
    tar_target(
        name = usersrated_final,
        command =
            usersrated_fit |>
            bundle::unbundle() |>
            finalize_model(data = training_and_validation)
    ),
    tar_target(
        name = hurdle_final,
        command = 
            hurdle_fit |>
            bundle::unbundle() |>
            finalize_model(data = training_and_validation |> add_hurdle(),
                           ratings = 0,
                           weights = 0),
        packages = c("lightgbm")
    ),
    tar_target(
        name = test_predictions,
        command = 
            split |>
            testing() |>
            impute_averageweight(model = unbundle(averageweight_final)) |>
            predict_bayesaverage(average_model = unbundle(average_final),
                                 usersrated_model = unbundle(usersrated_final)),
        packages = c("bundle")
    ),
    ## vetiver versions of models
    tar_target(
        name = averageweight_vetiver,
        command = 
            averageweight_final |>
            prepare_wflow() |>
            pin_outcome_model(metrics = valid_metrics,
                              data = training_and_validation,
                              tuning = averageweight_tuned,
                              board = model_board)
    ),
    tar_target(
        name = average_vetiver,
        command = 
            average_final |>
            prepare_wflow() |>
            pin_outcome_model(metrics = valid_metrics,
                              data = training_and_validation,
                              tuning = average_tuned,
                              board = model_board)
    ),
    tar_target(
        name = usersrated_vetiver,
        command = 
            usersrated_final |>
            prepare_wflow() |>
            pin_outcome_model(metrics = valid_metrics,
                              data = training_and_validation,
                              tuning = usersrated_tuned,
                              board = model_board)
    ),
    tar_target(
        name = hurdle_vetiver,
        command = 
            hurdle_final |>
            prepare_wflow() |>
            pin_outcome_model(metrics = valid_hurdle_metrics,
                              data = training_and_validation |> add_hurdle(),
                              tuning = hurdle_tuned,
                              board = model_board,
                              ratings = 0,
                              weights = 0)
    ),
    tar_target(
        active_games,
        get_games_from_gcp(bucket = "bgg_data") |>
            prepare_games ()
    ),
    tar_target(
        upcoming_games,
        command = 
            active_games |>
            filter(yearpublished > end_train_year + valid_years + 1)
    ),
    # use trained models to predict new games
    tar_target(
        predictions,
        command =
            {
                averageweight_fit =
                    pin_read_model(model_board,
                                   averageweight_vetiver)
                
                average_fit =
                    pin_read_model(model_board,
                                   average_vetiver)
                
                usersrated_fit =
                    pin_read_model(model_board,
                                   usersrated_vetiver)
                
                hurdle_fit =
                    pin_read_model(model_board,
                                   hurdle_vetiver)

                upcoming_games |>
                    impute_averageweight(
                        model = averageweight_fit
                    ) |>
                    predict_hurdle(
                        model = hurdle_fit,
                        threshold = hurdle_threshold
                    ) |>
                    predict_bayesaverage(
                        average_model = average_fit,
                        usersrated_model = usersrated_fit
                    )
            }
    ),
    # render reports
    tar_quarto(
        name = reports,
        path = ".",
        quiet = F,
        cue = tar_cue(mode = 'always')
    )
)