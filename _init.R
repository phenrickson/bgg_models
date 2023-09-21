# install renv
install.packages('renv')

# restore from lockfile
renv::restore()

# create data folder
dir.create(here::here("data"))

# gcs setup
googleCloudStorageR::gcs_setup()
