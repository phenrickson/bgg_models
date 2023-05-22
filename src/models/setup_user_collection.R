# functions and setup for user model training

# function to pull user collection from bgg and join it with all games
assemble_user_collection = function(username,
                                    games) {
        
        # required packages
        require(tidyverse)
        require(bggUtils)
        
        # pull collection and rename ts
        get_collection = function(username) {
                
                # load user collection data
                bggUtils::get_user_collection(username) |>
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


# models ------------------------------------------------------------------

# recipes for users
source(here::here("src", "features", "recipes_user.R"))

# specifications for classification models
source(here::here("src", "models", "class_model_specs.R"))

# tuning control
source(here::here("src", "models", "tune_ctrls.R"))

# class metrics
source(here::here("src", "models", "class_metrics.R"))

# modeling functions

# select only necessary from results
trim_results = function(res) {
        
        res %>%
                select(wflow_id, .row, !!outcome, starts_with(".pred"), any_of(c("game_id", "name", "yearpublished")))
        
}

# check that results finished
check_results = function(res) {
        
        res %>%
                mutate(check = purrr::map_lgl(result, ~identical(.x, list()))) %>%
                filter(check == F) %>%
                select(-check)
}

# finalize fits
finalize_fits = function(res,
                         metric) {
        
        res %>%
                mutate(best_tune = 
                               map(result,
                                   ~ .x %>% 
                                           select_best(metric = metric))) %>%
                mutate(last_fit = map2(result,
                                       best_tune,
                                       ~ .x %>%
                                               extract_workflow() %>%
                                               finalize_workflow(parameters = .y) %>%
                                               last_fit(user_train_setup$user_rsample_split,
                                                        metrics = prob_metrics,
                                                        control = ctrl_last_fit)))
}

# get individual predictions with ids from last fits
get_predictions = function(last_fits) {
        
        last_fits %>%
                select(wflow_id, last_fit) %>% 
                unnest(last_fit) %>%
                select(wflow_id, splits, .predictions) %>% 
                mutate(assess = map(splits,~ .x %>% 
                                            assessment %>% 
                                            select(game_id, name, yearpublished))) %>% 
                select(-splits) %>% 
                unnest(everything()) %>%
                trim_results() %>%
                arrange(desc(.pred_yes))
        
}

# get metrics from last fits
get_metrics = function(last_fits) {
        
        last_fits %>%
                select(wflow_id, last_fit) %>%
                mutate(metrics = map(last_fit,
                                     collect_metrics)) %>%
                select(wflow_id, metrics) %>%
                unnest(metrics) %>%
                arrange(.metric, .estimate)
        
}


# bundle ------------------------------------------------------------------






