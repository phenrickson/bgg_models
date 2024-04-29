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
    format = "qs"
)

# function to retrieve games stored in gcp bucket
load_games = function(object_name = "raw/objects/games",
                      bucket = "bgg_data",
                      ...) {
    
    bggUtils::get_games_from_gcp(
        bucket = bucket,
        object_name = object_name,
        generation = generation)
    
}

# function to preprocess games
preprocess_games = function(data) {
    data |>
        bggUtils::preprocess_bgg_games()
}

# functions
tar_source("src/data/load_data.R")
tar_source("src/models/training.R")
tar_source("src/models/assess.R")
tar_source("src/visualizations/models.R")
tar_source("src/models/tracking.R")

# function to build a recipe and apply series of steps given an outcome
build_outcome_recipe = 
    function(data,
             outcome,
             id_vars,
             predictor_vars,
             spline_vars) {
        
        data |>
            build_recipe(
                outcome = {{outcome}},
                ids = id_vars,
                predictors = predictor_vars
            ) |>
            # standard preprocessing
            add_preprocessing() |>
            # simple imputation for numeric
            add_imputation() |>
            # create dummies
            add_bgg_dummies() |>
            # add splines for nonlinearities
            # splines with a fourth degree polynomial for year
            add_splines(vars = "year", degree = 4) |>
            # splines with fifth degree polynomials for mechanics/categories
            add_splines(vars = spline_vars) |>
            # remove zero variance
            step_zv(all_numeric_predictors()) |>
            # remove highly correlated
            step_corr(all_numeric_predictors(), threshold = 0.95) |>
            # normalize
            step_normalize(all_numeric_predictors())
    }

# function to build workflow for an outcome given a model specification and a recipe
build_outcome_workflow = 
    function(model,
             recipe) {
        
        workflows::workflow() |>
            workflows::add_model(
                model
            ) |>
            workflows::add_recipe(
                recipe
            )
    }

# function to tune model given workflow and resamples
tune_workflow = function(workflow,
                         resamples,
                         metrics,
                         grid,
                         save_pred = T,
                         ...) {
    
    workflow |>
        tune_grid(
            resamples = resamples,
            grid = grid,
            metrics = metrics,
            control = tune::control_grid(save_pred = save_pred,
                                         verbose = T),
            ...
        )
}

# function to predict and calculate bayesaverage given average and usersrated
predict_bayesaverage = function(data,
                                average_model,
                                usersrated_model,
                                ratings = 2000) {
    
    # get predictions
        data |>
        predict_average(
            model = average_model
        ) |>
        predict_usersrated(
            model = usersrated_model
        ) |>
        calculate_bayesaverage(
            ratings = ratings
        ) |>
        mutate(.pred_averageweight = est_averageweight) |>
        select(yearpublished,
               game_id,
               name,
               starts_with(".pred"),
               everything()
        )
    
}

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
        command = games_raw |>
            # apply preprocessing
            preprocess_games() |>
            # remove games missing yearpublished
            filter(!is.na(yearpublished))
    ),
    tar_target(
        name = reg_metrics,
        command = my_reg_metrics()
    ),
    # create training, validation, and test splits based around the yearpublished
    # model and grid
    tar_target(
        name = model_spec,
        command = 
            linear_reg(
                engine = "glmnet",
                penalty = tune::tune(),
                mixture = tune::tune()
            )
    ),
    # grid
    tar_target(
        name = tuning_grid,
        command =
            grid_regular(
                penalty(range = c(-4, -1)),
                mixture(),
                levels = c(mixture = 5,
                           penalty = 10)
            )
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
    tar_target(
        averageweight_split,
        command = 
            split |>
            training() |>
            outcome_tuning_split(
                outcome = averageweight,
                weights = 5,
                valid_years = valid_years,
                test_years = 0
            )
    ),
    # define tuning split given outcome
    tar_target(
        # build workflow
        averageweight_wflow,
        command = 
            averageweight_split |>
            training()  |>
            # define recipe
            build_outcome_recipe(
                outcome = averageweight,
                id_vars = id_vars(),
                predictor_vars = predictor_vars(),
                spline_vars = spline_vars()
            ) |>
            # create workflow
            build_outcome_workflow(
                model = model_spec
            )
    ),
    # tune workflow on validation set
    tar_target(
        averageweight_tuned,
        command = 
            averageweight_wflow |>
            tune_workflow(
                resamples =
                    averageweight_split |>
                    validation_set(),
                metrics = reg_metrics,
                grid = tuning_grid
            )
    ),
    # fit model to entirety of training
    tar_target(
        averageweight_fit,
        command = 
            averageweight_wflow |>
            finalize_workflow(
                parameters = averageweight_tuned |>  
                    select_best(metric = 'rmse')
            ) |>
            fit(
                averageweight_split |>
                    pluck("data")
            )
    ),
    # use model to impute averageweight for training set
    tar_target(
        name = training_imputed,
        command =
            split |>
            training() |>
            impute_averageweight(
                model = averageweight_fit
            )
    ),
    ### predict average
    tar_target(
        name = average_split,
        command = 
            training_imputed |>
            outcome_tuning_split(
                outcome = average,
                valid_years = valid_years,
                test_years = 0
            )
    ),
    # create workflow for average
    tar_target(
        average_wflow,
        command = 
            average_split |>
            training() |>
            # create recipe
            build_outcome_recipe(
                outcome = average,
                id_vars = id_vars(),
                predictor_vars = c(predictor_vars(),
                                   "est_averageweight"),
                spline_vars = c(spline_vars(),
                                "est_averageweight")
            ) |>
            # create workflow
            build_outcome_workflow(
                model = model_spec
            )
    ),
    # tune model on validation_set
    tar_target(
        name = average_tuned,
        command = 
            average_wflow |>
            # tune workflow on validation set
            tune_workflow(
                resamples =
                    average_split |>
                    validation_set(),
                metrics = reg_metrics,
                grid = tuning_grid
            )
    ),
    # fit model on training set
    tar_target(
        name = average_fit,
        command = 
            average_wflow |>
            finalize_workflow(
                parameters = average_tuned |>
                    select_best(metric = 'rmse')
            ) |>
            fit(
                average_split |>
                    pluck("data")
            )
    ),
    ### predict usersrated
    tar_target(
        name = usersrated_split,
        command = 
            training_imputed |>
            mutate(log_usersrated = log(usersrated)) |>
            outcome_tuning_split(
                outcome = log_usersrated,
                valid_years = valid_years,
                test_years = 0
            )
    ),
    # create workflow for usersrated
    tar_target(
        usersrated_wflow,
        command = 
            usersrated_split |>
            training() |>
            # create recipe
            build_outcome_recipe(
                outcome = log_usersrated,
                id_vars = id_vars(),
                predictor_vars = c(predictor_vars(),
                                   "est_averageweight"),
                spline_vars = c(spline_vars(),
                                "est_averageweight")
            ) |>
            # create workflow
            build_outcome_workflow(
                model = model_spec
            )
    ),
    # tune model on validation_set
    tar_target(
        name = usersrated_tuned,
        command = 
            usersrated_wflow |>
            # tune workflow on validation set
            tune_workflow(
                resamples =
                    usersrated_split |>
                    validation_set(),
                metrics = reg_metrics,
                grid = tuning_grid
            )
    ),
    # fit model on training set
    tar_target(
        name = usersrated_fit,
        command = 
            usersrated_wflow |>
            finalize_workflow(
                parameters = usersrated_tuned |>
                    select_best(metric = 'rmse')
            ) |>
            fit(
                usersrated_split |>
                    pluck("data")
            )
    ),
    # now, predict validation set
    tar_target(
        name = validation_imputed,
        command =
            split |>
            validation() |>
            impute_averageweight(
                model = averageweight_fit
            )
    ),
    # get predictions from models
    tar_target(
        name = predictions,
        command =
            validation_imputed |>
            predict_bayesaverage(
                average_model = average_fit,
                usersrated_model = usersrated_fit
            )
    ),
    # now calculate how the model performed 
    # for the outcomes, we'll limit to games with at least 25 user ratings...
    # will doublecheck on this
    tar_target(
        name = metrics,
        command = 
            predictions |>
            assess_outcomes_by_threshold(
                metrics = my_reg_metrics(),
                groups = c("outcome"),
                threshold = c(0, 25)
            )
    ),
    # get workflow details to this
    tar_target(
        name = details,
        command = 
            map_df(
                list(
                    average_fit,
                    averageweight_fit,
                    usersrated_fit
                ),
                extract_workflow_details
            )
    ),
    # render report with quarto
    tar_quarto(
        report,
        path = "targets-runs/results.qmd",
        quiet = F
    ),
    ## finalize models
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
    # fit final models and convert to vetiver
    tar_target(
        averageweight_vetiver,
        command = 
            averageweight_fit |>
            fit(
                training_and_validation |>
                    preprocess_outcome(
                        outcome = averageweight,
                        weights = 5
                    )
            ) |>
            vetiver::vetiver_model(
                "bgg_averageweight"
            )
    ),
    tar_target(
        average_vetiver,
        command = 
            average_fit |>
            fit(
                training_and_validation |>
                    preprocess_outcome(
                        outcome = average,
                        weights = 0
                    )
            ) |>
            vetiver::vetiver_model(
                "bgg_average"
            )
    ),
    tar_target(
        usersrated_vetiver,
        command = 
            usersrated_fit |>
            fit(
                training_and_validation |>
                    mutate(log_usersrated = log(usersrated)) |>
                    preprocess_outcome(
                        outcome = usersrated
                    )
            ) |>
            vetiver::vetiver_model(
                "bgg_usersrated"
            )
    )
)