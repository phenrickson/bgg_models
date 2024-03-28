gcs_add_object_name = function(obj, object_name) {
    
    if(is.data.frame(obj)) {
        obj$object_name = object_name
        
        obj = obj |>
            select(object_name, everything())
    }
    
    obj
    
}

gcs_qread = function(object_name, ...) {
    
    gcs_obj = googleCloudStorageR::gcs_get_object(
        object_name,
        ...
    )
    
    gcs_obj |>
        qs::qdeserialize() |>
        gcs_add_object_name(object_name)
    
}

gcs_qread.download = function(object_name, ...) {
    
    tmp = tempfile()
    
    gcs_obj = googleCloudStorageR::gcs_get_object(
        object_name,
        saveToDisk = tmp,
        ...
    )
    
    qs::qread(tmp) |>
        gcs_add_object_name(object_name)
    
}

gcs_separate_object_name = function(data,
                                    names = c("bucket", "project", "branch_type", "branch", "objects", "object"),
                                    delim = "/",
                                    ...) {
    
    data |>
        separate_wider_delim(cols = object_name,  delim = delim, names = names, ...)
    
}