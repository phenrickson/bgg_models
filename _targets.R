# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) # Load other packages as needed.

# packages
tar_option_set(
    packages = c("dplyr",
                 "bggUtils",
                 "tidymodels",
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
# create local model board
model_board = pins::board_folder("models",
                                 versioned = T)

# functions
tar_source("src/data/load_data.R")
tar_source("src/models/training.R")
tar_source("src/models/assess.R")
tar_source("src/visualizations/models.R")
tar_source("src/models/tracking.R")

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
            impute_averageweight(model = bundle::unbundle(averageweight_fit))
    ),
    tar_target(
        name = valid_predictions,
        command = 
            validation_imputed |>
            predict_bayesaverage(average_model = bundle::unbundle(average_fit),
                                 usersrated_model = bundle::unbundle(usersrated_fit))
    ),
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
    # # render report with quarto
    # tar_quarto(
    #     report,
    #     path = "results.qmd",
    #     quiet = F
    # ),
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
            bundle::unbundle() |>
            pin_outcome_model(metrics = valid_metrics,
                              data = training_and_validation,
                              tuning = averageweight_tuned,
                              board = model_board),
        format = "file"
    ),
    tar_target(
        name = average_vetiver,
        command = 
            average_final |>
            bundle::unbundle() |>
            pin_outcome_model(metrics = valid_metrics,
                              data = training_and_validation,
                              tuning = average_tuned,
                              board = model_board),
        format = "file"
    ),
    tar_target(
        name = usersrated_vetiver,
        command = 
            usersrated_final |>
            bundle::unbundle() |>
            pin_outcome_model(metrics = valid_metrics,
                              data = training_and_validation,
                              tuning = usersrated_tuned,
                              board = model_board),
        format = "file"
    ),
    # render reports
    tar_quarto(
        name = reports,
        path = ".",
        quiet = F
    )
)