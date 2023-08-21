# reports pipeline
targets::tar_config_set(script = "_make_reports.R",
                        store = "_make_reports",
                        project = "project_reports")

Sys.setenv(TAR_PROJECT = "project_reports")

# refresh collections after  days
stale_collections = tar_older(Sys.time() - as.difftime(7, units = "days")) |>
        grep(pattern = "^user_collection", value = T)

# invalidate; request these collections again
tar_invalidate(any_of(stale_collections))

# make with future
targets::tar_make()

targets::tar_manifest()

targets::tar_glimpse()

# glimpse
#targets::tar_glimpse()
