Sys.setenv(TAR_PROJECT = "project_outcomes")

# run
targets::tar_make()

# glimpse
targets::tar_glimpse()