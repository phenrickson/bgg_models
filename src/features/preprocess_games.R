# packages:

# tidyverse
# bggUtils

#' Applies all previous functions to nested table
#' 
preprocess_games = function(data) {
        
        data %>%
                # collapse categorical variables
                process_categorical() %>%
                # select variables to keep
                select(game_id,
                       name,
                       bgg_info,
                       bgg_outcomes,
                       bgg_community,
                       images,
                       descriptions,
                       categories,
                       mechanics,
                       families,
                       designers,
                       artists,
                       publishers,
                       load_ts
                ) %>%
                # unnest bgg info
                unnest(c(bgg_info,
                         bgg_outcomes,
                         bgg_community,
                         images,
                         descriptions)) %>%
                # make logged usersrated
                mutate(log_usersrated = log(usersrated))
        
}

#' Takes values from nested cateogorical variables
#' in nested games data and collapses the value for tokenization via textrecipes
#' 
#' 
#' Uses abbreviate_text for abbreviation
#' Collapses using a simple comma
#' applies filtering for publishers and families
collapse_categorical = function(data,
                                var) {
        
        var = ensym(var)
        
        var_name = enexpr(var)
        
        if (var_name == 'publishers') {
                
                message("filtering to selected publishers...")
                
                data %>%
                        select(-!!var) %>%
                        left_join(.,
                                  # collapse value within nested var
                                  data %>%
                                          select(game_id, !!var) %>%
                                          unnest(!!var) %>%
                                          # filter
                                          filter(value %in% get_allow_games_publishers(data)) %>%
                                          # abbreviate
                                          mutate(value = clean_text(value)) %>%
                                          # collapse
                                          group_by(game_id) %>%
                                          summarize(!!var := paste(value, collapse = ", "),
                                                    .groups = 'drop'),
                                  by = c("game_id")
                        )
        } else if (var_name == 'families') {
                
                message("removing selected bgg families...")
                
                data %>%
                        select(-!!var) %>%
                        left_join(.,
                                  # collapse value within nested var
                                  data %>%
                                          select(game_id, !!var) %>%
                                          unnest(!!var) %>%
                                          # filter
                                          filter(!(value %in% get_filtered_games_families(data))) %>%
                                          # abbreviate
                                          mutate(value = clean_text(value)) %>%
                                          # collapse
                                          group_by(game_id) %>%
                                          summarize(!!var := paste(value, collapse = ", "),
                                                    .groups = 'drop'),
                                  by = c("game_id")
                        )
        } else {
                
                # join with original data
                data %>%
                        select(-!!var) %>%
                        left_join(.,
                                  # collapse value within nested var
                                  data %>%
                                          select(game_id, !!var) %>%
                                          unnest(!!var) %>%
                                          # abbreviate
                                          mutate(value = clean_text(value)) %>%
                                          # collapse
                                          group_by(game_id) %>%
                                          summarize(!!var := paste(value, collapse = ", "),
                                                    .groups = 'drop'),
                                  by = c("game_id")
                        )
        }
}

#' Process games for modeling by collapsing all specified categorical variables
process_categorical = function(data) {
        
        message("collapsing categorical variables...")
        
        data %>%
                # categories
                collapse_categorical(.,
                                     var = categories) %>%
                # mechanics
                collapse_categorical(.,
                                     var = mechanics) %>%
                # families
                collapse_categorical(.,
                                     var = families) %>%
                # designers
                collapse_categorical(.,
                                     var = designers) %>%
                # artists
                collapse_categorical(.,
                                     var = artists) %>%
                # publishers
                collapse_categorical(.,
                                     var = publishers)
        
}


#' Filter game families for modeling
get_filtered_games_families = function(data) {
        
        game_families = 
                data %>%
                select(families) %>%
                unnest(families) %>%
                distinct(family_value, id, type, family_value, value)
        
        families_filter_table = 
                bind_rows(
                        # better description needed 
                        game_families %>%
                                filter(grepl("Admin Better Description", value)),
                        # digital versions (leakage)
                        game_families %>%
                                filter(grepl("Digital Implementations", value)),
                        # misc values (leakage)
                        game_families %>%
                                filter(grepl("Misc", value)),
                        # promotional (leakage)
                        game_families %>%
                                filter(grepl("Promotional Implementations", value)),
                        # upcoming
                        game_families %>%
                                filter(grepl("Upcoming", value)),
                        # unreleased
                        game_families %>%
                                filter(grepl("Unreleased", value)),
                        # components typically added after release
                        game_families %>%
                                filter(grepl("Components Game Trayzinside", value)),
                        # specifc type of crowdfunding
                        game_families %>%
                                filter(grepl("Spieleschmiede|Verkami|Indiegogo", value))
                )
        
        # pull remaining families
        families_filter_table %>%
                pull(value)
        
}

# Specify which publisher features are allowed to enter model
get_allow_games_publishers = function(data) {
        
        # list of ids allowed to enter model for publisher
        publisher_allow_ids = c(
                51 # Hasbo
                ,10 # Mayfair Games
                ,102 # Decision Games
                ,196 # Multi-Man Publishing
                ,396 # Alderac Entertainment Group aka AEG
                ,1027 # Days of Wonder
                ,21847 # Pandasaurus Games
                ,1001 # (web published)
                ,4 # (Self-Published)
                ,140 # Splotter Spellen
                ,157 # Asmodee
                ,34 # Ravensburger
                ,28 # Parker Brothers
                ,39 # Pegasus Speile
                ,37 # KOSMOS
                ,20 # Milton Bradley
                ,3 # Rio Grande Games
                ,538 # Z-Man Games
                ,52 # GMT Games
                # ,8923 # IELLO
                ,17 # Fantasy Flight Games
                ,5 # Avalon Hill
                ,3320 # (Unknown)
                ,597 # Eagle-Gryphon Games
                ,5400 # Matagot
                ,26 # Games Workshop Ltd
                ,47 # Queen Games
                ,11652 # Stronghold Games
                ,19 # Steve Jackson Games
                ,13 # Wizards of the Coast
                ,12024 # Cryptozoic Entertainment
                ,10754 # Plaid Hat Games
                ,21608 # CMON Global Limited
                ,108 # Gamewright
                ,221 # WizKids
                ,171 # (Public Domain)
                ,93 # Mattel, Inc
                ,25842 # Space Cowboys
                ,23202 # Stonemaier
                ,34188 # Plan  B
                ,30958 # Capstone Games
                ,22593 # Chip Theory Games
                ,17917 # Ares Games
                ,17543 # Greater Than Games
                ,28072 # Renegade Games
                ,34846 # Restoration Games
                ,29313 # Osprey Games
                ,21765 # Roxley
                ,7345 # Czech Games Edition
                ,29412 # Awaken Realms
                ,3929 # Compass Games
                ,26991 # Button Shy
                ,2456 # The Game Crafter
                ,12 # Cheapass Games
                ,9 # alea
                ,2164 # NorthStar Game Studio 
                ,5774 # Bézier Games
                ,18617 #Red Raven Games 
                ,102 #Decision Games (I)
                , 489# 3W (World Wide Wargames) 
        )
        
        # table
        publisher_allow_table = 
                data %>%
                select(publishers) %>%
                unnest(publishers) %>%
                filter(id %in% publisher_allow_ids) %>%
                distinct()
        
        # pull names
        publisher_allow_table %>%
                pull(value)
        
}



# not run
# foo = games_nested %>%
#         preprocess_games()





