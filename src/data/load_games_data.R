# make data for hurdle and outcome models
# load tables used in modeling

### pull games nested locally
load(here::here("data", "processed", "games_nested.Rdata"))

### pull publisher and family filters from gcs bucket

# connect to gcs
source(here::here("src", "data", "connect_to_gcs.R"))

# read in publisher allow list
publisher_allow_names = data_board %>%
        pin_read("publisher_allow_names")

# read in publisher allow list
families_filter_names = data_board %>%
        pin_read("families_filter_names")
