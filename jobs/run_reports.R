# reports pipeline
targets::tar_config_set(script = "_make_reports.R",
                        store = "_make_reports",
                        project = "project_reports")

Sys.setenv(TAR_PROJECT = "project_reports")

# run
targets::tar_make()

tar_manifest()

# glimpse
targets::tar_glimpse()
