# run
library(targets)

# make data
tar_config_set(script = "_make_data.R",
                 store = "_make_data")

# make
tar_make()
tar_glimpse()
tar_visnetwork()
