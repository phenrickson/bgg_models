Sys.setenv(TAR_PROJECT = "project_data")

# run
targets::tar_make()

# glimpse
targets::tar_glimpse()

# network
targets::tar_visnetwork()