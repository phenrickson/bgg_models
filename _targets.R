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
        )
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

# function to extract model name
extract_model_name = function(workflow) {
        
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

# year splits for static branching based on end training year
year_splits = data.frame(
        end_train_year = c(2019)
)

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
                        add_preprocessing() |>
                        add_imputation() |>
                        add_bgg_dummies() |>
                        add_splines(vars = spline_vars) |>
                        step_zv(all_numeric_predictors()) |>
                        step_corr(all_numeric_predictors(), threshold = 0.95) |>
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
                         ...) {
        
        workflow |>
                tune_grid(
                        resamples = resamples,
                        grid = grid,
                        metrics = metrics,
                        control = tune::control_grid(save_pred = T,
                                                     verbose = T),
                        ...
                )
}


# Replace the target list below with your own:
list(
        tar_target(
                games_raw,
                command = 
                        load_games()
        ),
        tar_target(
                name = games_prepared,
                command = games_raw |>
                        preprocess_bgg_games()
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
        tar_map(
                values = year_splits,
                tar_target(
                        split,
                        command = 
                                games_prepared |>
                                create_year_split(
                                        end_train_year = end_train_year,
                                        valid_years = 2
                                )
                ),
                # create training, validation, and test split based on original split
                tar_target(
                        averageweight_split,
                        command =    
                                split |>
                                training() |>
                                outcome_tuning_split(
                                        outcome = averageweight,
                                        weights = 5
                                )
                ),
                # create recipe for estimating averageweight
                tar_target(
                        averageweight_recipe,
                        command = 
                                averageweight_split |>
                                training() |>
                                build_outcome_recipe(
                                        outcome = averageweight,
                                        id_vars = id_vars(),
                                        predictor_vars = predictor_vars(),
                                        spline_vars = spline_vars()
                                )
                ),
                # create workflow
                tar_target(
                        averageweight_workflow,
                        command = 
                                averageweight_recipe |>
                                build_outcome_workflow(
                                        model = model_spec
                                )
                ),
                # tune on validation set
                tar_target(
                        averageweight_tuned,
                        command = 
                                averageweight_workflow |>
                                tune_workflow(
                                        resamples = 
                                                averageweight_split |>
                                                validation_set(),
                                        metrics = reg_metrics,
                                        grid = tuning_grid
                                )
                ),
                # fit on train, validation, assess on test
                tar_target(
                        averageweight_last_fit,
                        command = 
                                averageweight_workflow |>
                                finalize_workflow(parameters = select_best(averageweight_tuned, metric = 'rmse')) |>
                                last_fit(
                                        split =  averageweight_split,
                                        metrics = reg_metrics,
                                        add_validation_set = T
                                ) |>
                                select(-.workflow)
                ),
                tar_target(
                        averageweight_model,
                        command = 
                                averageweight_workflow |>
                                finalize_workflow(parameters = select_best(averageweight_tuned, metric = 'rmse')) |>
                                fit(
                                        averageweight_split$data
                                )
                ),
                # now use that model to estimate the averageweight for the data in the original data set
                # as currently constructed, this means that yes we are estimating the averageweight in the training set
                # using a model that was trained on the same dataset
                # whether this is a problem or not depends on what we see for performance in the validation set
                tar_target(
                        training_imputed,
                        command = 
                                split |>
                                training() |>
                                impute_averageweight(model = averageweight_model)
                        
                ),
                ### train average models
                tar_target(
                        average_split,
                        command = 
                                training_imputed |>
                                outcome_tuning_split(
                                        outcome = average
                                )
                ),
                tar_target(
                        average_recipe,
                        command = 
                                average_split |>
                                training() |>
                                build_outcome_recipe(
                                        outcome = average,
                                        id_vars = id_vars(),
                                        predictor_vars = c(predictor_vars(),
                                                           "est_averageweight"),
                                        spline_vars = c(spline_vars(),
                                                        "est_averageweight")
                                )
                ),
                tar_target(
                        average_workflow,
                        command = 
                                average_recipe |>
                                build_outcome_workflow(
                                        model = model_spec
                                )
                ),
                tar_target(
                        average_tuned,
                        command = 
                                average_workflow |>
                                tune_workflow(
                                        resamples = 
                                                average_split |>
                                                validation_set(),
                                        metrics = reg_metrics,
                                        grid = tuning_grid
                                )
                ),
                tar_target(
                        average_last_fit,
                        command = 
                                average_workflow |>
                                finalize_workflow(parameters = select_best(average_tuned, metric = 'rmse')) |>
                                last_fit(
                                        split =   average_split,
                                        metrics = reg_metrics,
                                        add_validation_set = T
                                ) |>
                                select(-.workflow)
                ),
                tar_target(
                        average_model,
                        command = 
                                average_workflow |>
                                finalize_workflow(parameters = select_best(average_tuned, metric = 'rmse')) |>
                                fit(
                                        average_split$data
                                )
                ),
                ### train usersrated models
                tar_target(
                        usersrated_split,
                        command = 
                                training_imputed |>
                                mutate(log_usersrated = log(usersrated)) |>
                                outcome_tuning_split(
                                        outcome = log_usersrated
                                )
                ),
                tar_target(
                        usersrated_recipe,
                        command = 
                                usersrated_split |>
                                training() |>
                                build_outcome_recipe(
                                        outcome = log_usersrated,
                                        id_vars = id_vars(),
                                        predictor_vars = c(predictor_vars(),
                                                           "est_averageweight"),
                                        spline_vars = c(spline_vars(),
                                                        "est_averageweight")
                                )
                ),
                tar_target(
                        usersrated_workflow,
                        command =
                                usersrated_recipe |>
                                build_outcome_workflow(
                                        model = model_spec
                                )
                ),
                tar_target(
                        usersrated_tuned,
                        command = 
                                usersrated_workflow |>
                                tune_workflow(
                                        resamples = 
                                                usersrated_split |> 
                                                validation_set(),
                                        grid = tuning_grid,
                                        metrics = reg_metrics
                                )
                ),
                tar_target(
                        usersrated_last_fit,
                        command = 
                                usersrated_workflow |>
                                finalize_workflow(parameters = select_best(usersrated_tuned, metric = 'rmse')) |>
                                last_fit(
                                        split =   usersrated_split,
                                        metrics = reg_metrics,
                                        add_validation_set = T
                                ) |>
                                select(-.workflow)
                ),
                tar_target(
                        usersrated_model,
                        command = 
                                usersrated_workflow |>
                                finalize_workflow(parameters = select_best(usersrated_tuned, metric = 'rmse')) |>
                                fit(
                                        usersrated_split$data
                                )
                ),
                ### assess on validation set
                # get validation set
                tar_target(
                        name = validation_imputed,
                        command = 
                                split |>
                                validation() |>
                                impute_averageweight(
                                        model = averageweight_model
                                )
                ),
                # get predictions from models
                tar_target(
                        name = predictions,
                        command = 
                                validation_imputed |>
                                predict_average(
                                        model = average_model
                                ) |>
                                predict_usersrated(
                                        model = usersrated_model
                                ) |>
                                calculate_bayesaverage() |>
                                mutate(.pred_averageweight = est_averageweight) |>
                                select(yearpublished, 
                                       game_id, 
                                       name,
                                       starts_with(".pred"), 
                                       everything()
                                )
                ),
                # render model report
                tar_render(name = model_report,
                           "model_report.Rmd",
                           params = list(data = names(predictions),
                                         end_train_year = end_train_year)
                )
                # # assess
                # tar_target(
                #         name = metrics,
                #         command = 
                #                 predictions |>
                #                 select(game_id, name, yearpublished,
                #                        starts_with(".pred"),
                #                        average,
                #                        averageweight,
                #                        usersrated,
                #                        log_usersrated,
                #                        bayesaverage) |>
                #                 pivot_longer(
                #                         cols 
                #                 )
                # )
                # model cards
        )
)