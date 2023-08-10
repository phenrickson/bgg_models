# run
#library(targets)

# config ------------------------------------------------------------------

#data pipeline
targets::tar_config_set(script = "_make_data.R",
               store = "_make_data",
               project = "project_data")

# models pipeline
targets::tar_config_set(script = "_train_outcomes.R",
               store = "_train_outcomes",
               project = "project_outcomes")

# bgg estimates pipeline
targets::tar_config_set(script = "_predict_outcomes.R",
               store = "_predict_outcomes",
               project = "project_predict")