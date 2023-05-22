# function to pull user collection from bgg and join it with all games
assemble_user_collection = function(username,
                                    games) {
        
        # pull collection and rename ts
        get_collection = function(username) {
                
                # load user collection data
                bggUtils::get_user_collection(username) %>%
                        rename(user_load_ts = load_ts)
                
        }
        
        # join up games with collection
        join_games = function(collection,
                              games) {
                
                games %>%
                        left_join(.,
                                  collection,
                                  by = c("game_id", "name")
                        )
        }
        
        # function to create outcomes and set factors for user variables
        prep_collection = function(collection) {
                
                collection %>%
                        mutate(ever_owned = case_when(own == 1 | prevowned == 1 ~ 'yes',
                                                      TRUE ~ 'no'),
                               own = case_when(own == 1 ~ 'yes',
                                               TRUE ~ 'no'),
                               rated = case_when(own == 1 ~ 'yes',
                                                 TRUE ~ 'no'),
                               highly_rated = case_when(rating >= 8 ~ 'yes',
                                                        TRUE ~ 'no')
                        ) %>%
                        mutate_at(vars(own, ever_owned, rated),
                                  factor, levels = c("no", "yes"))
                
        }
        
        # pull collection
        collection = 
                get_collection(username)
        
        # 
        collection %>%
                join_games(.,
                           games) %>%
                prep_collection()
        
}

# split user collection and make resamples for model tuning
make_user_split_and_resamples = function(collection,
                                         games,
                                         end_train_year,
                                         valid_window = 2,
                                         outcome,
                                         min_users) {
        
        # helper function for splitting collection based on year
        split_collection = function(data,
                                    end_train_year,
                                    valid_window,
                                    min_users,
                                    filter = F) {
                
                train_collection = 
                        data %>%
                        filter(usersrated >=min_users) %>%
                        filter(yearpublished <= end_train_year)
                
                
                if (filter == T) {
                        valid_collection = 
                                data %>%
                                filter(usersrated >=min_users) %>%
                                filter(yearpublished > end_train_year & yearpublished <= end_train_year + valid_window)
                } else {
                        valid_collection = 
                                data %>%
                                filter(yearpublished > end_train_year & yearpublished <= end_train_year + valid_window)
                        
                }
                
                return(list("train" = train_collection,
                            "valid" = valid_collection))
                
        }
        
        # creates manual rsample split given previously created split object
        split_rsample = function(split) {
                
                make_splits(
                        list(analysis =
                                     seq(nrow(split$train)),
                             assessment =
                                     nrow(split$train) + seq(nrow(split$valid))),
                        bind_rows(split$train,
                                  split$valid)
                )
                
        }
        
        # split collection
        user_split = 
                split_collection(collection,
                                 end_train_year,
                                 valid_window = 2,
                                 min_users = min_users)
        
        # make split with rsample
        user_rsample_split = 
                split_rsample(user_split)
        
        # create resamples
        set.seed(1999)
        user_train_resamples = 
                vfold_cv(user_split$train,
                         v = 5,
                         strata = all_of(outcome))
        
        return(list("user_collection" = user_collection,
                    "user_split" = user_split,
                    "user_rsample_split" = user_rsample_split,
                    "user_train_resamples" = user_train_resamples,
                    "outcome" = outcome))
        
}