# function to render quarto report and output given username
render_report = function(file,
                         predictions,
                         ...) {
    
    output = paste0('docs/', file, '.html')
    
    quarto::quarto_render(
        input = file,
        execute_params = list(predictions = predictions),
        ...
    )
    
    gsub(".qmd", "", output)
    
}

# function to upload
upload_report = function(file,
                         prefix = 'bgg_models/',
                         bucket = 'bgg_reports',
                         predefinedAcl = 'bucketLevel',
                         type = 'text/html') {
    
    name = paste0(prefix, file)
    googleCloudStorageR::gcs_upload(file = file,
                                    name = name,
                                    bucket = bucket,
                                    type = type,
                                    predefinedAcl = predefinedAcl)
    
    name
}

# # not run
# upload_report(file = here::here('docs/phenrickson.html'))