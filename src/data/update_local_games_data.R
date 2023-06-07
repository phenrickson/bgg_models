# what: make game-level nested dataset containing all bgg features

# packages ---------------------------------------------------------------

suppressPackageStartupMessages(
        suppressMessages({
                {
                        library(tidyverse)
                        
                        conflicted::conflict_prefer("dplyr", "filter")
                        conflicted::conflict_prefer("dplyr", "lag")
                }
        }
        )
)

# connect to gcp ----------------------------------------------------------

source(here::here("src", "data", "connect_to_gcp.R"))

# functions ------------------------------------------------------------------

# create nested data frame at the game level using tables from bgg
prep_analysis = function(data) {
        
        ### analysis layer
        data %>%
                # change zeros to missingness
                mutate_at(
                        vars(c("yearpublished",
                               "averageweight",
                               "average",
                               "bayesaverage",
                               "stddev",
                               "minplayers",
                               "maxplayers",
                               "minage",
                               "playingtime",
                               "minplaytime",
                               "maxplaytime")),
                        ~ na_if(., 0)) %>%
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

# join nested categorical tables with analysis
nest_categorical = function(data) {
        
        data %>%
                # description
                left_join(.,
                          game_descriptions %>%
                                  select(-load_ts) %>%
                                  nest(description = description),
                          by = c("game_id")) %>%
                # categories
                left_join(.,
                          game_categories %>% 
                                  select(-load_ts) %>%
                                  nest(categories = -game_id),
                          by = c("game_id")
                ) %>%
                # mechanics
                left_join(.,
                          game_mechanics %>% 
                                  select(-load_ts) %>%
                                  nest(mechanics = -game_id),
                          by = c("game_id")
                ) %>%
                # designers
                left_join(.,
                          game_designers %>% 
                                  select(-load_ts) %>%
                                  nest(designers = -game_id),
                          by = c("game_id")
                ) %>%
                # artists
                left_join(.,
                          game_artists %>% 
                                  select(-load_ts) %>%
                                  nest(artists = -game_id),
                          by = c("game_id")
                ) %>%
                # families
                left_join(.,
                          game_families %>% 
                                  select(-load_ts) %>%
                                  nest(families = -game_id),
                          by = c("game_id")
                ) %>%
                # publishers
                left_join(.,
                          game_publishers %>% 
                                  select(-load_ts) %>%
                                  nest(publishers = -game_id),
                          by = c("game_id")
                ) %>%
                # implementations
                left_join(.,
                          game_implementations %>%
                                  select(-load_ts) %>%
                                  nest(implementations = -game_id),
                          by = c("game_id")
                ) %>%
                # compilations
                left_join(.,
                          game_compilations %>%
                                  select(-load_ts) %>%
                                  nest(compilations = -game_id),
                          by = c("game_id")
                )
}


# laod data from gcp ----------------------------------------------------------

# pull most recent load of game tables to analysis layer

# analysis games
analysis_games = bq_table_download(bq_project_query(project_id,
                                                    'SELECT * FROM bgg.analysis_games
                                   WHERE load_ts = (SELECT max(load_ts) as date 
                                   FROM bgg.analysis_games)'))


# unreleased games
unreleased_games<- bq_table_download(bq_project_query(project_id,
                                                      'SELECT * FROM bgg.analysis_unreleased_games
                                   WHERE load_ts = (SELECT max(load_ts) as date 
                                   FROM bgg.analysis_unreleased_games)'))

# ganmes to drop due to data quality issues
drop_games<- bq_table_download(bq_project_query(project_id,
                                                'SELECT * FROM bgg.analysis_drop_games
                                   WHERE load_ts = (SELECT max(load_ts) as date 
                                   FROM bgg.analysis_drop_games)'))

# descriptions
game_descriptions<- bq_table_download(bq_project_query(project_id,
                                                       'SELECT * FROM bgg.analysis_game_descriptions
                                   WHERE load_ts = (SELECT max(load_ts) as date 
                                   FROM bgg.analysis_game_descriptions)'))

# images
game_images<- bq_table_download(bq_project_query(project_id,
                                                 'SELECT * FROM bgg.analysis_game_images
                                   WHERE load_ts = (SELECT max(load_ts) as date 
                                   FROM bgg.analysis_game_images)'))

# categories
game_categories<- bq_table_download(bq_project_query(project_id,
                                                     'SELECT * FROM bgg.analysis_game_categories
                                   WHERE load_ts = (SELECT max(load_ts) as date 
                                   FROM bgg.analysis_game_categories)'))

# compilations
game_compilations<- bq_table_download(bq_project_query(project_id,
                                                       'SELECT * FROM bgg.analysis_game_compilations
                                   WHERE load_ts = (SELECT max(load_ts) as date 
                                   FROM bgg.analysis_game_compilations)'))

# designers
game_designers<- bq_table_download(bq_project_query(project_id,
                                                    'SELECT * FROM bgg.analysis_game_designers
                                   WHERE load_ts = (SELECT max(load_ts) as date 
                                   FROM bgg.analysis_game_designers)'))

# publishers
game_publishers<- bq_table_download(bq_project_query(project_id,
                                                     'SELECT * FROM bgg.analysis_game_publishers
                                   WHERE load_ts = (SELECT max(load_ts) as date 
                                   FROM bgg.analysis_game_publishers)'))

# families
game_families<- bq_table_download(bq_project_query(project_id,
                                                   'SELECT * FROM bgg.analysis_game_families
                                   WHERE load_ts = (SELECT max(load_ts) as date 
                                   FROM bgg.analysis_game_families)'))

# implementations
game_implementations<- bq_table_download(bq_project_query(project_id,
                                                          'SELECT * FROM bgg.analysis_game_implementations
                                   WHERE load_ts = (SELECT max(load_ts) as date 
                                   FROM bgg.analysis_game_implementations)'))

# artists
game_artists<- bq_table_download(bq_project_query(project_id,
                                                  'SELECT * FROM bgg.analysis_game_artists
                                   WHERE load_ts = (SELECT max(load_ts) as date 
                                   FROM bgg.analysis_game_artists)'))

# mechanics
game_mechanics<- bq_table_download(bq_project_query(project_id,
                                                    'SELECT * FROM bgg.analysis_game_mechanics
                                   WHERE load_ts = (SELECT max(load_ts) as date 
                                   FROM bgg.analysis_game_mechanics)'))

# player counts for games
game_playercounts = bq_table_download(bq_project_query(project_id,
                                                       'SELECT * FROM bgg.api_game_playercounts
                                   WHERE load_ts = (SELECT max(load_ts) as date 
                                   FROM bgg.api_game_playercounts)'))


# nested ------------------------------------------------------------------

# create nested dataset using functions
games_nested = 
        analysis_games %>%
        prep_analysis() %>%
        nest_categorical()


# save local versions---------------------------------------------------------------------

message("saving analysis tables..")

# make list with names
games_analysis = 
        list("analysis_games" = analysis_games,
             "unreleased_games" = unreleased_games,
             "drop_games" = drop_games,
             "game_descriptions" = game_descriptions,
             "game_images" = game_images,
             "game_categories" = game_categories,
             "game_compilations" = game_compilations,
             "game_designers" = game_designers,
             "game_families" =  game_families,
             "game_implementations" = game_implementations,
             "game_playercounts" = game_playercounts,
             "game_publishers" = game_publishers,
             "game_artists" = game_artists,
             "game_mechanics" = game_mechanics)

# pin to raw board
# not using versions for this
games_analysis %>%
        pins::pin_write(
                name = "analysis_games_tables",
                board =         
                        pins::board_folder(
                                here::here("data", "raw")),
                versioned = F)

# create nested dataset using functions
games_nested = 
        analysis_games %>%
        prep_analysis() %>%
        nest_categorical()

message("saving nested table...")

# save locally to processed board
games_nested %>%
        pins::pin_write(
                name = "games_nested",
                board =         
                        pins::board_folder(
                                here::here("data", "processed")),
                versioned = T)

# # save local ------------------------------------------------------------------
# 
# message("saving games_nested locally")
# 
# save(games_nested,
#      file = here::here("data", "local", "games_nested.Rdata"))


