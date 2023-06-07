# what: connect to pinboards on gcs for project

# authenticates via info set in .Renviron
library(googleCloudStorageR)

# set bucket
#my_bucket = "bgg_bucket"

gcs_bucket = function() {
        "bgg_bucket"
}

get_gcs_board = function(board,
                         ...) {
        
        board_prefix = paste0(board, "/")
        
                pins::board_gcs(
                        bucket = gcs_bucket(),
                        prefix = board_prefix,
                        versioned = T)
        
        
}

# # data board
# data_board =
#         pins::board_gcs(
#                 bucket = gcs_bucket,
#                 prefix = "data/",
#                 versioned = T)
# 
# # results board
# results_board =
#         pins::board_gcs(
#                 bucket = gcs_bucket,
#                 prefix = "results/",
#                 versioned = T)
# 
# # models board
# models_board =
#         pins::board_gcs(
#                 bucket = gcs_bucket,
#                 prefix = "models/",
#                 versioned = T)
# 
# # deployed board
# deployed_board =
#         pins::board_gcs(
#                 bucket = gcs_bucket,
#                 prefix = "deployed/",
#                 versioned = T)