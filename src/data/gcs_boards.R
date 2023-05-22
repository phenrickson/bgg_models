# what: connect to pinboards on gcs for project

# authenticates via info set in .Renviron
library(googleCloudStorageR)

# set bucket
my_bucket = "bgg_bucket"

# data board
data_board =
        pins::board_gcs(
                bucket = my_bucket,
                prefix = "data/",
                versioned = T)

# results board
results_board =
        pins::board_gcs(
                bucket = my_bucket,
                prefix = "results/",
                versioned = T)

# models board
models_board =
        pins::board_gcs(
                bucket = my_bucket,
                prefix = "models/",
                versioned = T)

# deployed board
deployed_board =
        pins::board_gcs(
                bucket = my_bucket,
                prefix = "deployed/",
                versioned = T)