# what: connect to googleCloudStorageR

# loading this package autodetects gcs connection
library(googleCloudStorageR)

# otherwise, authenticate directly

# # authenticate via json file
# googleCloudStorageR::gcs_auth(Sys.getenv("GCS_AUTH_FILE"))
# 
# # set bucket
# my_bucket = Sys.getenv("GCS_DEFAULT_BUCKET")
