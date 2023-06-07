# what: local version of processed games for modeling along with allowed publisher names and bgg families

# connect to gcs boards
source(here::here("src", "data", "connect_to_gcs_boards.R"))

# load tables used in modeling
games_nested = 
        pins::pin_read(
                board = pins::board_folder(here::here("data", "processed")),
                name = "games_nested")

# read in publisher allow list
publisher_allow_names = 
        pins::pin_read(
                board = data_board,
                name = "publisher_allow_names")

# read in publisher allow list
families_filter_names = 
        pins::pin_read(board = data_board,
                       name = "families_filter_names")
