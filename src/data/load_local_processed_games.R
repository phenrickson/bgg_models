# what: local version of processed games for modeling along with allowed publisher names and bgg families

# load tables used in modeling
load(here::here("data", "processed", "games_nested.Rdata"))

# connect to gcs boards
source(here::here("src", "data", "gcs_boards.R"))

# read in publisher allow list
publisher_allow_names = data_board |>
        pins::pin_read("publisher_allow_names")

# read in publisher allow list
families_filter_names = data_board |>
        pins::pin_read("families_filter_names")
