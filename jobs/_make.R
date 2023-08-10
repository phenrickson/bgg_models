# run
library(targets)

# config ------------------------------------------------------------------

#data pipeline
tar_config_set(script = "_make_data.R",
               store = "_make_data",
               project = "project_data")

# models pipeline
tar_config_set(script = "_train_outcomes.R",
               store = "_train_outcomes",
               project = "project_outcomes")

# bgg estimates pipeline
tar_config_set(script = "_predict_outcomes.R",
               store = "_predict_outcomes",
               project = "project_predict")

# data --------------------------------------------------------------------

Sys.setenv(TAR_PROJECT = "project_data")

# run
tar_make()

# glimpse
tar_glimpse()

# train outcome models ------------------------------------------------------------------

Sys.setenv(TAR_PROJECT = "project_outcomes")

# run
tar_make()

# glimpse
tar_glimpse()

# predict with outcome models ------------------------------------------------------------------

Sys.setenv(TAR_PROJECT = "project_predict")

# run
tar_make()

# glimpse
tar_glimpse()



