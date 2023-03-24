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
                                          filter(value %in% publisher_allow_names) %>%
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
                                          filter(!(value %in% families_filter_names)) %>%
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

#' Applies all previous functions to nested table
#' 
preprocess_games= function(data) {
        
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
                       description,
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
                         description))
        
}

# not run
# foo = games_nested %>%
#         preprocess_games()





