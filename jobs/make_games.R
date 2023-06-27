# # what:
# # load tables from gcp 
# # preprocess and join to create tables for models
# # pin to local folders
# 
# library(targets)
# 
# rstudioapi::restartSession() # Remove in-memory detritus.
# #targets::tar_make(callr_function = NULL)
# 
# # config ------------------------------------------------------------------
# 
# # tar_config_set(script = here::here("make", "_foo.R"),
# #                store = here::here("make"),
# #                project = "foo")
# 
# # functions ---------------------------------------------------------------
# 
# 
# custom_sum = function() {
#         
#         2+2
#         
# }
# 
# # connecting to bigquery
# source(here::here("src", "data", "connect_to_bigquery.R"))
# 
# # for loading games
# source(here::here("src", "data", "load_games_data.R"))
# 
# # packages ----------------------------------------------------------------
# 
# #tar_option_reset()
# 
# # packages
# tar_option_set(packages = c("bigrquery",
#                             "dplyr",
#                             "tidyr",
#                             "pins"))
# 
# # tar ---------------------------------------------------------------------
# 
# tar_script(
#         list(
#                 tar_target(games_table, "games"),
#                 tar_target(categorical_tables, c("mechanics", "designers")),
#                 tar_target(games, load_table(table_name = games_table))
#               #  tar_target(categorical, load_categorical_tables(categorical_tables))                # tar_target(table_name, "games"),
#                 # #    tar_target(games_table, "games", format = "file"),
#                 # tar_target(data, load_table(table_name)))
# )
# 
# #    tar_target(categorical_table, load_categorical_tables(c("mechanics", "designers"))),
# #    tar_target(processed, preprocess_games(games_table))
# # tar_target(categorical, load_categorical_tables())
# # tar_target(processed, preprocess_games(analysis)),
# # tar_target(games, left_join(processed, categorical, by = c("game_id"))),
# # tar_target(data_board,  pins::board_folder(here::here("data", "processed"))),
# # tar_target(pinned, pins::pin_write(x = games,
# #                           board = data_board,
# #                           name = "games_processed",
# #                           versioned = T,
# #                           tags = "data"))
# 
# tar_manifest()
# tar_visnetwork()
# tar_make()
