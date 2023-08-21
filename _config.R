# config ------------------------------------------------------------------

#data pipeline
targets::tar_config_set(script = "_make_data.R",
               store = "_make_data",
               project = "project_data")

# models pipeline
targets::tar_config_set(script = "_train_outcomes.R",
               store = "_train_outcomes",
               project = "project_outcomes")

# reports pipeline
targets::tar_config_set(script = "_make_reports.R",
                        store = "_make_reports",
                        project = "project_reports")