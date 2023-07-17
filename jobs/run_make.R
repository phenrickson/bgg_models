# run
library(targets)


# data --------------------------------------------------------------------

# data pipeline
my_script = "_make_data.R"

# make data
tar_config_set(script = "_make_data.R",
               store = "_make_data",
               config = "_make_data.yaml")

# make
tar_make(script = my_script)

# viz
tar_glimpse(script = my_script)
tar_visnetwork(script = my_script)


# models ------------------------------------------------------------------

