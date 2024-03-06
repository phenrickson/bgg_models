# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) # Load other packages as needed.

# Set target options:
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
                        prefix = "models"
                )
        )
)

# authenticate to GCR and and set bucket
library(googleCloudStorageR)

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

# year splits for static branching based on end training year
year_splits = data.frame(
        end_train_year = c(2019)
)

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
                )
        )
)