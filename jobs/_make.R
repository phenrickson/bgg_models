# run
library(targets)


# data --------------------------------------------------------------------

# data pipeline
# set config to make yaml
tar_config_set(script = "_make_data.R",
               store = "_make_data",
               config = "_make_data.yaml")

# make
tar_make(script = "_make_data.R",
         store = "_make_data")

# viz
tar_glimpse(script = "_make_data.R")

tar_visnetwork(script = "_make_data.R")


# models ------------------------------------------------------------------

