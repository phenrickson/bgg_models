# what: make game-level nested dataset containing all bgg features


# packages:
# tidyverse

#  
# functions ------------------------------------------------------------------

# connect to biquery
source(here::here("src", "data", "connect_to_bigquery.R"))

# load table from bigquery
load_table = 
        function(table_name) {
                
                message("loading ", table_name)
                query_table(table_name)
        }

# load tables that are then nested by their name
load_categorical_tables = 
        function(tables = c("descriptions",
                            "categories",
                            "mechanics",
                            "designers",
                            "artists",
                            "families",
                            "publishers",
                            "implementations",
                            "compilations")) {
                
                # load table and then nest it by name
                load_nested_table = 
                        function(table_name) {
                                
                                # nest table by specified variable
                                nest_table = function(table,
                                                      name) {
                                        
                                        table %>%
                                                select(-load_ts) %>%
                                                nest(!!name := -game_id)
                                }
                                
                                query_table(table_name) |>
                                        nest_table(name = gsub("^game_", "", table_name))
                                
                        }
                
                # set names
                table_names = paste0("game_", tables)
                
                # load all 
                get_tables = map(table_names,
                    ~ load_nested_table(.x))
                
                # join together
                reduce(get_tables, full_join, by = c("game_id"))
                
        }

# create nested data frame at the game level using tables from bgg
transform_games =
        function(data) {
        
        ### analysis layer
        data %>%
                # change zeros to missingness
                mutate(
                        across(c("yearpublished",
                                 "averageweight",
                                 "average",
                                 "bayesaverage",
                                 "stddev",
                                 "minplayers",
                                 "maxplayers",
                                 "minage",
                                 "playingtime",
                                 "minplaytime",
                                 "maxplaytime"),
                               ~ na_if(., 0))) %>%
                # change some integers to numeric
                mutate_at(
                        vars(c("yearpublished",
                               "minage",
                               "minplayers",
                               "maxplayers",
                               "usersrated",
                               "playingtime",
                               "minplaytime",
                               "maxplaytime")),
                        as.numeric
                ) %>%
                # arrange by game id
                arrange(game_id) %>%
                # bgg_info 
                nest(bgg_info = c(yearpublished, minage, minplayers, maxplayers, playingtime, minplaytime, maxplaytime)) %>%
                # nest bgg outcomes
                nest(bgg_outcomes = c(averageweight, average, bayesaverage, usersrated, stddev)) %>%
                # nest bgg community 
                nest(bgg_community = c(numcomments, numweights, owned, trading, wanting, wishing)) %>%
                # nest images
                nest(images = c(image, thumbnail)) %>%
                # nest ranks
                nest(ranks = starts_with("rank_")) %>%
                # nest playercounts
                nest(playercounts = starts_with("playercount"))
        
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


# query most recent file from analysis layer
query_table = 
        function(table_name,
                 ...) {
                
                # point table name to bgg dataset and analysis prefix (schema)
                create_table_name = function(table_name,
                                             schema = 'analysis',
                                             dataset = 'bgg') {
                        
                        table_name = paste0("bgg.", schema, "_", table_name)
                        
                }
                
                table_name = create_table_name(table_name)
                
                query = paste(
                        'SELECT * FROM',
                        table_name,
                        'WHERE load_ts = (SELECT max(load_ts) as date FROM', 
                        table_name, ')')
                
                bigquery_download_table(query)
                 
        }

# 
# # functions ------------------------------------------------------------------
# 
# # connect to biquery
# source(here::here("src", "data", "connect_to_bigquery.R"))
# 
# # function to load tables
# make_games_data = function() {
#         
#         games = 
#         load_table("games") |>
#                 preprocess_analysis() |>
#                 join_with_nested_tables()
#         
# }
# 
# # create nested data frame at the game level using tables from bgg
# preprocess_analysis = function(data) {
#         
#         ### analysis layer
#         data %>%
#                 # change zeros to missingness
#                 mutate(
#                         across(c("yearpublished",
#                                  "averageweight",
#                                  "average",
#                                  "bayesaverage",
#                                  "stddev",
#                                  "minplayers",
#                                  "maxplayers",
#                                  "minage",
#                                  "playingtime",
#                                  "minplaytime",
#                                  "maxplaytime"),
#                                ~ na_if(., 0))) %>%
#                 # change some integers to numeric
#                 mutate_at(
#                         vars(c("yearpublished",
#                                "minage",
#                                "minplayers",
#                                "maxplayers",
#                                "usersrated",
#                                "playingtime",
#                                "minplaytime",
#                                "maxplaytime")),
#                         as.numeric
#                 ) %>%
#                 # arrange by game id
#                 arrange(game_id) %>%
#                 # bgg_info 
#                 nest(bgg_info = c(yearpublished, minage, minplayers, maxplayers, playingtime, minplaytime, maxplaytime)) %>%
#                 # nest bgg outcomes
#                 nest(bgg_outcomes = c(averageweight, average, bayesaverage, usersrated, stddev)) %>%
#                 # nest bgg community 
#                 nest(bgg_community = c(numcomments, numweights, owned, trading, wanting, wishing)) %>%
#                 # nest images
#                 nest(images = c(image, thumbnail)) %>%
#                 # nest ranks
#                 nest(ranks = starts_with("rank_")) %>%
#                 # nest playercounts
#                 nest(playercounts = starts_with("playercount"))
#         
# }
# 
# # join nested categorical tables with analysis
# join_with_nested_tables = function(analysis) {
#         
#         # descriptions
#         # categories
#         # mechanics
#         # designers
#         # artists
#         # families
#         # publishers
#         # implementations
#         # compilations
#         
#         analysis %>%
#                 left_join(.,
#                           load_nested_table('game_descriptions'),
#                           by = c("game_id")) %>%
#                 left_join(.,
#                           load_nested_table('game_categories'),
#                           by = c("game_id")) %>%
#                 left_join(.,
#                           load_nested_table('game_mechanics'),
#                           by = c("game_id")) %>%
#                 left_join(.,
#                           load_nested_table('game_designers'),
#                           by = c("game_id")) %>%
#                 left_join(.,
#                           load_nested_table('game_artists'),
#                           by = c("game_id")) %>%
#                 left_join(.,
#                           load_nested_table('game_families'),
#                           by = c("game_id")) %>%
#                 left_join(.,
#                           load_nested_table('game_publishers'),
#                           by = c("game_id")) %>%
#                 left_join(.,
#                           load_nested_table('game_implementations'),
#                           by = c("game_id")) %>%
#                 left_join(.,
#                           load_nested_table('game_compilations'),
#                           by = c("game_id"))
#         
# }
#         
# # load table from big query
# load_table = 
#         function(table_name) {
#                 
#                 message("loading ", table_name)
#                 query_table(table_name)
#         }
# 
# # load table and nest
# load_nested_table = 
#         function(table_name) {
#                 
#                 # nest table by specified variable
#                 nest_table = function(table,
#                                       name) {
#                         
#                         table %>%
#                                 select(-load_ts) %>%
#                                 nest(!!name := -game_id)
#                 }
#                 
#                 query_table(table_name) |>
#                         nest_table(name = gsub("^game_", "", table_name))
#                 
#         }
# 
# # download table from bigquery
# bigquery_download_table = 
#         function(my_query) {
#                 
#                 bigquery_connect()
#                 
#                 bq_table_download(
#                         bq_project_query(
#                                 bigquery_project_id(),
#                                 my_query)
#                 )
#         } 
# 
# # point table name to bgg dataset and analysis prefix (schema)
# create_table_name = function(table_name,
#                           schema = 'analysis',
#                           dataset = 'bgg') {
#                 
#         table_name = paste0("bgg.", schema, "_", table_name)
#                 
# }
# 
# # query most recent file from analysis layer
# query_table = 
#         function(table_name) {
#                 
#                 table_name = create_table_name(table_name)
#                 
#                 query = paste(
#                         'SELECT * FROM',
#                         table_name,
#                         'WHERE load_ts = (SELECT max(load_ts) as date FROM', 
#                         table_name, ')')
#                 
#                 bigquery_download_table(query)
#                  
#         }
# 
# # pin table to local raw
# pin_table = function(table,
#                      name) {
#         
#         table %>%
#         pins::pin_write(name = name,
#                         board = pins::board_folder(path = here::here("data", "raw"),
#                                              versioned = F))
#         
# }
# 
# 
# 
# library(targets)
# source("R/functions.R")
# tar_option_set(packages = c("readr", "dplyr", "ggplot2"))
# list(
#         tar_target(file, "data.csv", format = "file"),
#         tar_target(data, get_data(file)),
#         tar_target(model, fit_model(data)),
#         tar_target(plot, plot_model(model, data))
# )
# 
# 
# # load --------------------------------------------------------------------
# 
# # load analysis tables
# load_analysis_tables =
#         function() {
#                 
#                 bigquery_download_table(
#                         query_analysis_table("games")
#                 )
#         }
# 
# 
# # not run
# # bigquery_download_table(
# #         query_analysis_table("games")
# # )
# 
# load_tables = function() {
#         
#         # connect to bigquery
#         bigquery_con = bigquery_connect()
#         
#         # analysis
#         analysis_games = 
#                 bigquery_download_table(
#                         query_analysis_table("games")
#                 )
#         
#                         bq_table_download(
#                         bq_project_query(
#                                 bigquery_project_id(),
#                                 query = 'SELECT * FROM bgg.analysis_games
#                                    WHERE load_ts = (SELECT max(load_ts) as date 
#                                    FROM bgg.analysis_games)'))
#         
#         load_analysis_table()
#         
#         load_supplementary_tables()
#         
#         load_s
#         
#         
#         
# }
# 
# load_analysis_table = function() {
#         
#         # pull most recent load of game tables to analysis layer
#         
#         # analysis layer
#         analysis_games <-
#                 bq_table_download(
#                         bq_project_query(
#                                 bigquery_project_id(),
#                                 query = 'SELECT * FROM bgg.analysis_games
#                                    WHERE load_ts = (SELECT max(load_ts) as date 
#                                    FROM bgg.analysis_games)'))
# 
# }
# 
# load_supplementary_tables = function() {
#         
#         bigquery_con = bigquery_connect()
#         
#         # unreleased games
#         unreleased_games = 
#                 bq_table_download(
#                         bq_project_query(
#                                 bigquery_project_id(),
#                                 query = 'SELECT * FROM bgg.analysis_unreleased_games
#                                    WHERE load_ts = (SELECT max(load_ts) as date 
#                                    FROM bgg.analysis_unreleased_games)'))
#         
#         # ganmes to drop due to data quality issues
#         drop_games = 
#                 bq_table_download(
#                         bq_project_query(
#                                 bigquery_project_id(),
#                                 query = 'SELECT * FROM bgg.analysis_drop_games
#                                    WHERE load_ts = (SELECT max(load_ts) as date 
#                                    FROM bgg.analysis_drop_games)'))
#         
#         # descriptions
#         game_descriptions = 
#                 bq_table_download(
#                         bq_project_query(
#                                 bigquery_project_id(),
#                                 query = 'SELECT * FROM bgg.analysis_game_descriptions
#                                    WHERE load_ts = (SELECT max(load_ts) as date 
#                                    FROM bgg.analysis_game_descriptions)'))
#         
#         # images
#         game_images = 
#                 bq_table_download(
#                         bq_project_query(
#                                 bigquery_project_id(),
#                                 query = 'SELECT * FROM bgg.analysis_game_images
#                                    WHERE load_ts = (SELECT max(load_ts) as date 
#                                    FROM bgg.analysis_game_images)'))
#         
# }
# 
# load_categorical_tables = function() {
#         
#         # categories
#         game_categories<- bq_table_download(
#                 bq_project_query(
#                         bigquery_project_id(),
#                         query = 'SELECT * FROM bgg.analysis_game_categories
#                                    WHERE load_ts = (SELECT max(load_ts) as date 
#                                    FROM bgg.analysis_game_categories)'))
#         
#         # compilations
#         game_compilations<-
#                 bq_table_download(bq_project_query(
#                         bigquery_project_id(),
#                         query = 'SELECT * FROM bgg.analysis_game_compilations
#                                    WHERE load_ts = (SELECT max(load_ts) as date 
#                                    FROM bgg.analysis_game_compilations)'))
#         
#         # designers
#         game_designers<- 
#                 bq_table_download(
#                         bq_project_query(
#                                 bigquery_project_id(),
#                                 query = 'SELECT * FROM bgg.analysis_game_designers
#                                    WHERE load_ts = (SELECT max(load_ts) as date 
#                                    FROM bgg.analysis_game_designers)'))
#         
#         # publishers
#         game_publishers<-
#                 bq_table_download(
#                         bq_project_query(
#                                 bigquery_project_id(),
#                                 query = 'SELECT * FROM bgg.analysis_game_publishers
#                                    WHERE load_ts = (SELECT max(load_ts) as date 
#                                    FROM bgg.analysis_game_publishers)'))
#         
#         # families
#         game_families<- bq_table_download(
#                 bq_project_query(
#                         bigquery_project_id(),
#                         query = 'SELECT * FROM bgg.analysis_game_families
#                                    WHERE load_ts = (SELECT max(load_ts) as date 
#                                    FROM bgg.analysis_game_families)'))
#         
#         # implementations
#         game_implementations<- 
#                 bq_table_download(
#                         bq_project_query(
#                                 bigquery_project_id(),
#                                 query = 'SELECT * FROM bgg.analysis_game_implementations
#                                    WHERE load_ts = (SELECT max(load_ts) as date 
#                                    FROM bgg.analysis_game_implementations)'))
#         
#         # artists
#         game_artists<-
#                 bq_table_download(
#                         bq_project_query(
#                                 bigquery_project_id(),
#                                 query = 'SELECT * FROM bgg.analysis_game_artists
#                                    WHERE load_ts = (SELECT max(load_ts) as date 
#                                    FROM bgg.analysis_game_artists)'))
#         
#         # mechanics
#         game_mechanics<- bq_table_download(
#                 bq_project_query(project_id,
#                                                             'SELECT * FROM bgg.analysis_game_mechanics
#                                    WHERE load_ts = (SELECT max(load_ts) as date 
#                                    FROM bgg.analysis_game_mechanics)'))
#         
#         # player counts for games
#         game_playercounts = bq_table_download(bq_project_query(project_id,
#                                                                'SELECT * FROM bgg.api_game_playercounts
#                                    WHERE load_ts = (SELECT max(load_ts) as date 
#                                    FROM bgg.api_game_playercounts)'))
# }
# 
# {
#         
#         # unreleased games
#         unreleased_games<- bq_table_download(bq_project_query(project_id,
#                                                               'SELECT * FROM bgg.analysis_unreleased_games
#                                    WHERE load_ts = (SELECT max(load_ts) as date 
#                                    FROM bgg.analysis_unreleased_games)'))
#         
#         # ganmes to drop due to data quality issues
#         drop_games<- bq_table_download(bq_project_query(project_id,
#                                                         'SELECT * FROM bgg.analysis_drop_games
#                                    WHERE load_ts = (SELECT max(load_ts) as date 
#                                    FROM bgg.analysis_drop_games)'))
#         
#         # descriptions
#         game_descriptions<- bq_table_download(bq_project_query(project_id,
#                                                                'SELECT * FROM bgg.analysis_game_descriptions
#                                    WHERE load_ts = (SELECT max(load_ts) as date 
#                                    FROM bgg.analysis_game_descriptions)'))
#         
#         # images
#         game_images<- bq_table_download(bq_project_query(project_id,
#                                                          'SELECT * FROM bgg.analysis_game_images
#                                    WHERE load_ts = (SELECT max(load_ts) as date 
#                                    FROM bgg.analysis_game_images)'))
#         
#         # categories
#         game_categories<- bq_table_download(bq_project_query(project_id,
#                                                              'SELECT * FROM bgg.analysis_game_categories
#                                    WHERE load_ts = (SELECT max(load_ts) as date 
#                                    FROM bgg.analysis_game_categories)'))
#         
#         # compilations
#         game_compilations<- bq_table_download(bq_project_query(project_id,
#                                                                'SELECT * FROM bgg.analysis_game_compilations
#                                    WHERE load_ts = (SELECT max(load_ts) as date 
#                                    FROM bgg.analysis_game_compilations)'))
#         
#         # designers
#         game_designers<- bq_table_download(bq_project_query(project_id,
#                                                             'SELECT * FROM bgg.analysis_game_designers
#                                    WHERE load_ts = (SELECT max(load_ts) as date 
#                                    FROM bgg.analysis_game_designers)'))
#         
#         # publishers
#         game_publishers<- bq_table_download(bq_project_query(project_id,
#                                                              'SELECT * FROM bgg.analysis_game_publishers
#                                    WHERE load_ts = (SELECT max(load_ts) as date 
#                                    FROM bgg.analysis_game_publishers)'))
#         
#         # families
#         game_families<- bq_table_download(bq_project_query(project_id,
#                                                            'SELECT * FROM bgg.analysis_game_families
#                                    WHERE load_ts = (SELECT max(load_ts) as date 
#                                    FROM bgg.analysis_game_families)'))
#         
#         # implementations
#         game_implementations<- bq_table_download(bq_project_query(project_id,
#                                                                   'SELECT * FROM bgg.analysis_game_implementations
#                                    WHERE load_ts = (SELECT max(load_ts) as date 
#                                    FROM bgg.analysis_game_implementations)'))
#         
#         # artists
#         game_artists<- bq_table_download(bq_project_query(project_id,
#                                                           'SELECT * FROM bgg.analysis_game_artists
#                                    WHERE load_ts = (SELECT max(load_ts) as date 
#                                    FROM bgg.analysis_game_artists)'))
#         
#         # mechanics
#         game_mechanics<- bq_table_download(bq_project_query(project_id,
#                                                             'SELECT * FROM bgg.analysis_game_mechanics
#                                    WHERE load_ts = (SELECT max(load_ts) as date 
#                                    FROM bgg.analysis_game_mechanics)'))
#         
#         # player counts for games
#         game_playercounts = bq_table_download(bq_project_query(project_id,
#                                                                'SELECT * FROM bgg.api_game_playercounts
#                                    WHERE load_ts = (SELECT max(load_ts) as date 
#                                    FROM bgg.api_game_playercounts)'))
# }
# 
# 
# # create nested data frame at the game level using tables from bgg
# preprocess_analysis = function(data) {
#         
#         ### analysis layer
#         data %>%
#                 # change zeros to missingness
#                 mutate(
#                         across(c("yearpublished",
#                                "averageweight",
#                                "average",
#                                "bayesaverage",
#                                "stddev",
#                                "minplayers",
#                                "maxplayers",
#                                "minage",
#                                "playingtime",
#                                "minplaytime",
#                                "maxplaytime"),
#                         ~ na_if(., 0))) %>%
#                 # change some integers to numeric
#                 mutate_at(
#                         vars(c("yearpublished",
#                                "minage",
#                                "minplayers",
#                                "maxplayers",
#                                "usersrated",
#                                "playingtime",
#                                "minplaytime",
#                                "maxplaytime")),
#                         as.numeric
#                 ) %>%
#                 # arrange by game id
#                 arrange(game_id) %>%
#                 # bgg_info 
#                 nest(bgg_info = c(yearpublished, minage, minplayers, maxplayers, playingtime, minplaytime, maxplaytime)) %>%
#                 # nest bgg outcomes
#                 nest(bgg_outcomes = c(averageweight, average, bayesaverage, usersrated, stddev)) %>%
#                 # nest bgg community 
#                 nest(bgg_community = c(numcomments, numweights, owned, trading, wanting, wishing)) %>%
#                 # nest images
#                 nest(images = c(image, thumbnail)) %>%
#                 # nest ranks
#                 nest(ranks = starts_with("rank_")) %>%
#                 # nest playercounts
#                 nest(playercounts = starts_with("playercount"))
#         
# }
# 
# # join nested categorical tables with analysis
# join_with_nested = function(data) {
#         
#         data %>%
#                 # description
#                 left_join(.,
#                           game_descriptions %>%
#                                   select(-load_ts) %>%
#                                   nest(description = description),
#                           by = c("game_id")) %>%
#                 # categories
#                 left_join(.,
#                           game_categories %>% 
#                                   select(-load_ts) %>%
#                                   nest(categories = -game_id),
#                           by = c("game_id")
#                 ) %>%
#                 # mechanics
#                 left_join(.,
#                           game_mechanics %>% 
#                                   select(-load_ts) %>%
#                                   nest(mechanics = -game_id),
#                           by = c("game_id")
#                 ) %>%
#                 # designers
#                 left_join(.,
#                           game_designers %>% 
#                                   select(-load_ts) %>%
#                                   nest(designers = -game_id),
#                           by = c("game_id")
#                 ) %>%
#                 # artists
#                 left_join(.,
#                           game_artists %>% 
#                                   select(-load_ts) %>%
#                                   nest(artists = -game_id),
#                           by = c("game_id")
#                 ) %>%
#                 # families
#                 left_join(.,
#                           game_families %>% 
#                                   select(-load_ts) %>%
#                                   nest(families = -game_id),
#                           by = c("game_id")
#                 ) %>%
#                 # publishers
#                 left_join(.,
#                           game_publishers %>% 
#                                   select(-load_ts) %>%
#                                   nest(publishers = -game_id),
#                           by = c("game_id")
#                 ) %>%
#                 # implementations
#                 left_join(.,
#                           game_implementations %>%
#                                   select(-load_ts) %>%
#                                   nest(implementations = -game_id),
#                           by = c("game_id")
#                 ) %>%
#                 # compilations
#                 left_join(.,
#                           game_compilations %>%
#                                   select(-load_ts) %>%
#                                   nest(compilations = -game_id),
#                           by = c("game_id")
#                 )
# }
# 
# # load raw games data from biquery ----------------------------------------------------------
# load_analysis_tables = function() {
#         
#         # pull most recent load of game tables to analysis layer
#         bigquery_con = bigquery_connect()
#         
#         # analysis layer
#         analysis_games =
#                 bq_table_download(
#                         bq_project_query(
#                                 bigquery_project_id(),
#                                 query = 'SELECT * FROM bgg.analysis_games
#                                    WHERE load_ts = (SELECT max(load_ts) as date 
#                                    FROM bgg.analysis_games)'))
#         
#         # unreleased games
#         unreleased_games<- bq_table_download(bq_project_query(project_id,
#                                                               'SELECT * FROM bgg.analysis_unreleased_games
#                                    WHERE load_ts = (SELECT max(load_ts) as date 
#                                    FROM bgg.analysis_unreleased_games)'))
#         
#         # ganmes to drop due to data quality issues
#         drop_games<- bq_table_download(bq_project_query(project_id,
#                                                         'SELECT * FROM bgg.analysis_drop_games
#                                    WHERE load_ts = (SELECT max(load_ts) as date 
#                                    FROM bgg.analysis_drop_games)'))
#         
#         # descriptions
#         game_descriptions<- bq_table_download(bq_project_query(project_id,
#                                                                'SELECT * FROM bgg.analysis_game_descriptions
#                                    WHERE load_ts = (SELECT max(load_ts) as date 
#                                    FROM bgg.analysis_game_descriptions)'))
#         
#         # images
#         game_images<- bq_table_download(bq_project_query(project_id,
#                                                          'SELECT * FROM bgg.analysis_game_images
#                                    WHERE load_ts = (SELECT max(load_ts) as date 
#                                    FROM bgg.analysis_game_images)'))
#         
#         # categories
#         game_categories<- bq_table_download(bq_project_query(project_id,
#                                                              'SELECT * FROM bgg.analysis_game_categories
#                                    WHERE load_ts = (SELECT max(load_ts) as date 
#                                    FROM bgg.analysis_game_categories)'))
#         
#         # compilations
#         game_compilations<- bq_table_download(bq_project_query(project_id,
#                                                                'SELECT * FROM bgg.analysis_game_compilations
#                                    WHERE load_ts = (SELECT max(load_ts) as date 
#                                    FROM bgg.analysis_game_compilations)'))
#         
#         # designers
#         game_designers<- bq_table_download(bq_project_query(project_id,
#                                                             'SELECT * FROM bgg.analysis_game_designers
#                                    WHERE load_ts = (SELECT max(load_ts) as date 
#                                    FROM bgg.analysis_game_designers)'))
#         
#         # publishers
#         game_publishers<- bq_table_download(bq_project_query(project_id,
#                                                              'SELECT * FROM bgg.analysis_game_publishers
#                                    WHERE load_ts = (SELECT max(load_ts) as date 
#                                    FROM bgg.analysis_game_publishers)'))
#         
#         # families
#         game_families<- bq_table_download(bq_project_query(project_id,
#                                                            'SELECT * FROM bgg.analysis_game_families
#                                    WHERE load_ts = (SELECT max(load_ts) as date 
#                                    FROM bgg.analysis_game_families)'))
#         
#         # implementations
#         game_implementations<- bq_table_download(bq_project_query(project_id,
#                                                                   'SELECT * FROM bgg.analysis_game_implementations
#                                    WHERE load_ts = (SELECT max(load_ts) as date 
#                                    FROM bgg.analysis_game_implementations)'))
#         
#         # artists
#         game_artists<- bq_table_download(bq_project_query(project_id,
#                                                           'SELECT * FROM bgg.analysis_game_artists
#                                    WHERE load_ts = (SELECT max(load_ts) as date 
#                                    FROM bgg.analysis_game_artists)'))
#         
#         # mechanics
#         game_mechanics<- bq_table_download(bq_project_query(project_id,
#                                                             'SELECT * FROM bgg.analysis_game_mechanics
#                                    WHERE load_ts = (SELECT max(load_ts) as date 
#                                    FROM bgg.analysis_game_mechanics)'))
#         
#         # player counts for games
#         game_playercounts = bq_table_download(bq_project_query(project_id,
#                                                                'SELECT * FROM bgg.api_game_playercounts
#                                    WHERE load_ts = (SELECT max(load_ts) as date 
#                                    FROM bgg.api_game_playercounts)'))
#         
# }
# 
# # nested ------------------------------------------------------------------
# 
# # create nested dataset using functions
# games_nested = 
#         load_analysis_games() %>%
#         preprocess_analysis_games() %>%
#         join_with_nested_categorical()
#         nest_categorical()
# 
# 
# # save local versions---------------------------------------------------------------------
# 
# message("saving analysis tables..")
# 
# # make list with names
# games_analysis = 
#         list("analysis_games" = analysis_games,
#              "unreleased_games" = unreleased_games,
#              "drop_games" = drop_games,
#              "game_descriptions" = game_descriptions,
#              "game_images" = game_images,
#              "game_categories" = game_categories,
#              "game_compilations" = game_compilations,
#              "game_designers" = game_designers,
#              "game_families" =  game_families,
#              "game_implementations" = game_implementations,
#              "game_playercounts" = game_playercounts,
#              "game_publishers" = game_publishers,
#              "game_artists" = game_artists,
#              "game_mechanics" = game_mechanics)
# 
# # pin to raw board
# games_analysis %>%
#         pins::pin_write(
#                 name = "analysis_games_tables",
#                 board =         
#                         pins::board_folder(
#                                 here::here("data", "raw")),
#                 versioned = T)
# 
# # create nested dataset using functions
# games_nested = 
#         analysis_games %>%
#         prep_analysis() %>%
#         nest_categorical()
# 
# message("saving nested games..")
# 
# # save locally to processed board
# games_nested %>%
#         pins::pin_write(
#                 name = "games_nested",
#                 board =         
#                         pins::board_folder(
#                                 here::here("data", "processed")),
#                 versioned = T)
# 
# # # save local ------------------------------------------------------------------
# # 
# # message("saving games_nested locally")
# # 
# # save(games_nested,
# #      file = here::here("data", "local", "games_nested.Rdata"))
# 
# 
