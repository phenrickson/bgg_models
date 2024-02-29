# helper function to find top n neighbors 
top_n_neighbors = function(neighbors,
                           top_n,
                           ...) {
        
        assignee_neighbors %>%
                unnest(neighbors) %>%
                slice_min(order_by = dist,
                          n = n_neighbors + 1,
                          with_ties = F,
                          ...)
        
}

find_fuzzy_game = function(data, 
                           search,
                           top_n = 5,
                           ...) {
        
        data %>%
                select(name, game_id) %>%
                fuzzyjoin::stringdist_inner_join(
                        tibble(name = search),
                        by = c("name"),
                        distance_col = 'dist',
                        ...
                ) %>%
                arrange(dist) %>%
                head(top_n)
        
}

return_names = function(data) {
        
        names = 
                data %>%
                mutate(id = paste(name, ' (', yearpublished, ')', " - ", game_id, sep = "")) %>%
                pull(id)
        
        message(
                paste(names,
                      collapse = "\n"),
                '\n'
        )
        
        names
        
}

find_game_id = function(data,
                        search,
                        yearpublished = NULL,
                        ...) {
        
        search_yearpublished = yearpublished
        
        temp = 
                data %>%
                filter(name == search)
        
        if (nrow(temp) < 1 ) {
                
                temp = 
                        data %>%
                        filter(grepl(paste0("^", search), name))
        }
        
        if (nrow(temp) > 1) {
                
                message("games found:")
                
                temp %>%
                        return_names()
                
                message("returning id for first game found:")
                
                out = 
                        temp
                
        } else if (nrow(temp) == 0)
                
        {
                message("no games found; expanding search...")
                
                fuzzy_games = 
                        find_fuzzy_game(
                                data,
                                search,
                                method = 'jw',
                                ...
                        ) %>%
                        mutate(name = name.x,
                               game_id,
                               .keep = 'none') %>%
                        left_join(.,
                                  data,
                                  by = join_by(game_id, name))
                
                if (nrow(fuzzy_games) < 1) {
                        
                        stop("no games found")
                        
                } else {
                        
                        
                        message("closest games:")
                        
                        top_matches = 
                                fuzzy_games %>%
                                return_names()
                        
                        message("returning id for closest game:")
                        
                        out = 
                                fuzzy_games %>%
                                head(1)
                }
                
                
        }
        
        else {
                
                message("game found:")
                
                # temp %>%
                #         return_names()
                
                out = 
                        temp
        }
        
        if (!is.null(search_yearpublished)) {
                
                out = 
                        temp %>%
                        filter(yearpublished >= search_yearpublished)
                
        }
        
        
        out %>%
                head(1) %>%
                return_names()
        
        out %>%
                head(1) %>%
                pull(game_id)
        
}

# games_imputed %>%
#         find_game_id(
#                 'Carcassssone'
#         )

find_game_neighbors = function(
                data,
                game,
                game_id = NULL,
                ids = c("name", "game_id", "yearpublished"),
                metric = 'hamming',
                unnest = T,
                ...) {
        
        if (is.null(game_id)) {
                
                search = game
                
                search_id = 
                        data %>%
                        find_game_id(
                                search = game,
                                ...)
                
                neighbors =
                        data %>%
                        find_neighbors(
                                find_game_id = search_id,
                                ids,
                                metric,
                                ...
                        )
                
        }
        
        else {
                neighbors =
                        data %>%
                        find_neighbors(
                                find_game_id = game_id,
                                ids,
                                metric
                        )
        }
        
        if (unnest == T) {
                
                neighbors %>%
                        unnest(neighbors)
        }
}

# function to calc distance between x and y and return n neighbors given distance metric
get_neighbors = function(x,
                         y,
                         ids = c("game_id", "name", "yearpublished"),
                         metric = 'angular',
                         n) {
        
        require(rdist)
        
        # number of closest neighbors
        if (missing(n)) {n = nrow(y)}
        
        # if x is only one row...
        if(nrow(x)==1) {
                
                # ensure only numeric
                x = x %>%
                        select(-any_of(ids)) %>%
                        select(where(is.numeric)) %>%
                        as.matrix
                
                y = y %>%
                        select(-any_of(ids)) %>%
                        select(where(is.numeric)) %>%
                        as.matrix
                
                # get distance
                dist_mat = 
                        rdist::cdist(x, y, metric = metric)
                
                # order
                neighbors = 
                        order(dist_mat)[1:n]
                
                ordered =
                        tibble(.row_neighbor = neighbors,
                               .metric = metric,
                               dist = dist_mat[neighbors])
                
        }   
        
        return(ordered)
        
}

# function to return neighbors for assignees given pca estimates
find_neighbors = function(comparison_data,
                          find_game_id,
                          ids = c('game_id', 'name', 'yearpublished'),
                          metric = 'hamming',
                          ...) {
        
        # game_name = 
        #         comparison_data %>%
        #         filter(game_id == find_game_id) %>%
        #         mutate(id = paste(name, game_id, yearpublished, sep = " - ")) %>%
        #         head(1) %>%
        #         pull(id)
        # 
        # message("finding neighbors for ", game_name)
        # 
        # # find neighbors with get_neighbors for each id
        neighbors =
                comparison_data %>%
                filter(game_id == find_game_id) %>%
                # nest at the assignee level
                nest(data = -any_of(ids)) %>%
                # compute neighbors at the assignee level
                mutate(neighbors = map(data,
                                       ~ get_neighbors(.x,
                                                       y=comparison_data,
                                                       ids,
                                                       metric = metric)  %>%
                                               # join with original pca_estimates to find neighbor ids
                                               left_join(.,
                                                         comparison_data %>%
                                                                 transmute(.row_neighbor = row_number(),
                                                                           neighbor_id = game_id,
                                                                           neighbor_name = name),
                                                         by = c(".row_neighbor")))) %>%
                select(any_of(ids), neighbors)
        
        records = 
                neighbors %>%
                pluck("neighbors", 1) %>%
                nrow()
        
        if (!(records > 1)) {
                stop("no records returned")
        }
        
        message(records, " neighbors returned")
        
        return(neighbors)
        
}
