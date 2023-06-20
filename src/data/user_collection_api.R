#/xmlapi2/collection?parameters

library(httr2)
library(xml2)
library(dplyr)
library(purrr)

username = 'mrbananagrabber'

# functions ---------------------------------------------------------------

get_user_collection = function(username) {
        
        xml = 
                request_collection(username) |>
                get_collection_xml()
        
        collection = 
                xml |>
                get_collection()
        
        dplyr::tibble(
                username = username,
                collection,
                username_load_ts = Sys.time()
        )
}

# build url for username api request
build_url = function(username) { 
        
        paste0("https://www.boardgamegeek.com/xmlapi2/collection?username=",
               username,
               "&subtype=boardgame",
               "&stats=1")
}

# build request to keep trying for up to two minutes based on status
build_request = function(url) {
        
        request(url) |>
                req_retry(
                        is_transient = ~ resp_status(.x) == 202,
                        max_seconds = 120,
                        backoff = ~ 10)
}

# perform user request
request_collection = function(username) {
        
        username |>
                build_url() |>
                build_request() |>
                req_perform()
        
}

# get xml from response
get_collection_xml = function(response) {
        
        response |>
                resp_body_xml(simplifyVector = T) |>
                xml_children()
}

# get collection info
get_collection = function(xml) {
        
        
        get_status_vars = function(xml, var) {
                
                
                vec = 
                        xml |>
                        xml_find_all("//status") |>
                        xml_attr(var)
                
                col = 
                        dplyr::tibble(vec)
                
                names(col) = var
                
                col
        }
        
        types = 
                xml |>
                xml_attr("subtype")
        
        ids = xml |>
                xml_attr("objectid")
        
        collids =
                xml |>
                xml_attr("collid")
        
        names =
                xml %>%
                xml_find_all("//name") |>
                xml_text()
        
        status_vars =
                c("own",
                  "prevowned",
                  "fortrade",
                  "want",
                  "wanttoplay",
                  "wanttobuy",
                  "wishlist",
                  "lastmodified")
        
        status =
                purrr::map(status_vars,
                           ~ get_status_vars(xml, .x)) |>
                dplyr::bind_cols()
        
        rating =
                xml |>
                xml_find_all("//rating") |>
                xml_attr("value")
        
        dplyr::bind_cols(
                dplyr::tibble(game_id = ids),
                dplyr::tibble(collection_id = collids),
                dplyr::tibble(type = types),
                dplyr::tibble(name = names),
                status,
                dplyr::tibble(rating = rating)
        )
        
}

