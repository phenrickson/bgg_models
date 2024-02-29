# connect to big query data warehouse

# project id
#project_id <- 'gcp-analytics-326219'

# set bq project id
bigquery_project_id = function() {
        'gcp-analytics-326219'
}

# set bq dataset
bigquery_dataset = function() {
        "bgg"
}


# connect to specified data on gcp
bigquery_connect = function(my_project_id = bigquery_project_id(),
                            my_dataset = bigquery_dataset()) {
        
        require(bigrquery)
        
        # authenticate via json
        bigrquery::bq_auth(
                path = Sys.getenv("GCS_AUTH_FILE")
        )
        
        # establish connection
        bigrquery::dbConnect(
                bigrquery::bigquery(),
                project = my_project_id,
                dataset = my_dataset
        )
}

# download table from bigquery
bigquery_download_table = 
        function(my_query) {
                
                bigquery_connect()
                
                bq_table_download(
                        bq_project_query(
                                bigquery_project_id(),
                                my_query)
                )
        } 


# bq_dataset(bigquery_project_id(),
#            bigquery_dataset())

# not run
# connect_to_gcp()
