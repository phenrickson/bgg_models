# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) # Load other packages as needed.

# authenticate to GCR and and set bucket
library(googleCloudStorageR)

# set global bucket
gcs_global_bucket("bgg_data")

# function to set where i'm storing
set_gcp_prefix = function(bucket = gcs_get_global_bucket(), project, branch) {
    
    paste(
        bucket,
        project,
        branch,
        sep = "/"
    )
}

# packages
tar_option_set(
    packages = c("dplyr",
                 "bggUtils",
                 "googleCloudStorageR",
                 "tidymodels",
                 "gert",
                 "quarto",
                 "qs"),
    repository = "gcp",
    resources = tar_resources(
        gcp = tar_resources_gcp(
            bucket = "bgg_data",
            prefix = set_gcp_prefix(
                bucket = gcs_get_global_bucket(),
                project = "bgg_models",
                branch = gert::git_branch()
            )
        )
    ),
    format = "qs"
)

# function to laod games
load_games = function(object_name = "raw/objects/games",
                      generation = "1708980495752949",
                      bucket = "bgg_data",
                      ...) {
    
    googleCloudStorageR::gcs_get_object(object_name = object_name,
                                        generation = generation,
                                        bucket = bucket,
                                        ...) |>
        qs::qdeserialize()
    
}

# functions
tar_source("src/features/preprocess_games.R")
tar_source("src/models/train_outcomes.R")
tar_source("src/models/assess.R")
tar_source("src/visualizations/models.R")

# parameter for targets
end_train_year = 2019
valid_years = 3

# function to extract model name
extract_engine_name = function(workflow) {
    
    outcome = 
        workflow |> 
        extract_mold() |>
        pluck("outcomes") |> 
        names()
    
    engine = 
        workflow |>
        extract_spec_parsnip() |>
        pluck("engine")
    
    paste(outcome, engine, sep = "-")
    
}

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

# Replace the target list below with your own:
# targets definine data splitting strategy for training and validation
list(
    tar_target(
        games_raw,
        command = 
            load_games()
    ),
    tar_target(
        name = games_prepared,
        command = games_raw |>
            preprocess_games() |>
            filter(!is.na(yearpublished))
    ),
    tar_target(
        name = reg_metrics,
        command = metric_set(
            yardstick::rmse,
            yardstick::mae,
            yardstick::mape,
            yardstick::rsq
        )
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
                valid_years = 2,
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
                valid_years = 2,
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
                valid_years = 2,
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
        name = outcome_metrics_filtered,
        command = 
            predictions |>
            filter(usersrated >= 25) |>
            pivot_outcomes() |>
            group_by(outcome, yearpublished) |>
            assess_outcomes(
                metrics = my_reg_metrics()
            )
    ),
    tar_target(
        name = outcome_metrics_all,
        command = 
            predictions |>
            pivot_outcomes() |>
            group_by(outcome, yearpublished) |>
            assess_outcomes(
                metrics = my_reg_metrics()
            )
    ),
    tar_target(
        bgg_hit_metrics,
        command = 
            predictions |>
            mutate(bayesaverage = replace_na(bayesaverage, 5.5)) |>
            calculate_bgg_hit(threshold = 6.5) |>
            group_by(yearpublished, threshold) |>
            assess_bgg_hit(
                metrics = my_class_metrics()
            )
    ),
    # render report
    tar_quarto(
        report,
        path = "results.qmd",
        quiet = F
    )
)