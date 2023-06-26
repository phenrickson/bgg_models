# what: functions for training user models

# load user colleciton
load_user_collection = function(username) {
        
        message("loading user collection...")
        
        # get user collection
        user_collection = get_user_collection(username) %>%
                rename(user_load_ts = load_ts)
        
        # if less than 30 games ever owned, stop
        if (nrow(user_collection) < 30) {
                warning("not enough games in collection for reliable modeling")
        } else if (nrow(user_collection) < 50) {
                warning("relatively few games in collection; results may not be reliable")
        }
        
        user_collection
        
}

# function to create outcomes and set factors for user variables
prep_collection = function(collection) {
        
        collection %>%
                mutate(ever_owned = case_when(own == 1 | prevowned == 1 ~ 'yes',
                                              TRUE ~ 'no'),
                       own = case_when(own == 1 ~ 'yes',
                                       TRUE ~ 'no'),
                       rated = case_when(!is.na(rating) ~ 'yes',
                                         TRUE ~ 'no'),
                       highly_rated = case_when(rating >= 8 ~ 'yes',
                                                TRUE ~ 'no')
                ) %>%
                mutate_at(vars(own, ever_owned, rated),
                          factor, levels = c("no", "yes"))
        
}

# join collection with bgg games
join_bgg_games = function(collection,
                          games) {
        
        # required packages
        require(tidyverse)
        
        message("joining with bgg games")
        
        # join up bgg games with collection
        join_games = function(collection,
                              games) {
                
                games %>%
                        left_join(.,
                                  collection,
                                  by = c("game_id", "name")
                        )
        }
        
        # join games with collection
        collection_and_games = collection %>%
                # join 
                join_games(., games) %>%
                # prep
                prep_collection()
        
        # how many games in bgg?
        message(paste(nrow(collection_and_games), "games from bgg"))
        
        # how many games joined for modeling?
        message(paste(nrow(collection_and_games %>% 
                                   filter(!is.na(user_load_ts))), "games in user collection"))
        
        collection_and_games
        
        
}

# filter training set to only include games with minimum number of users
filter_min_users = function(data,
                            min_users = 30) {
        
        data %>%
                filter(usersrated >=min_users) 
        
}

# create training/test splits and resamples
# helper function for splitting collection based on year
make_user_split = function(data,
                           end_train_year,
                           valid_window,
                           min_users = 30,
                           filter_min_users = F) {
        
        train = 
                data %>%
                filter(usersrated >=min_users) %>%
                filter(yearpublished <= end_train_year)
        
        
        if (filter_min_users == T) {
                valid = 
                        data %>%
                        filter_min_users() %>%
                        filter(yearpublished > end_train_year & yearpublished <= end_train_year + valid_window)
        } else {
                valid = 
                        data %>%
                        filter(yearpublished > end_train_year & yearpublished <= end_train_year + valid_window)
                
        }
        
        make_splits(
                list(analysis =
                             seq(nrow(train)),
                     assessment =
                             nrow(train) + seq(nrow(valid))),
                bind_rows(train,
                          valid)
        )
        
}

find_max_year = function(split) {
        
        bind_rows(analysis(split),
                  assessment(split)) %>%
                summarize(max_year = max(yearpublished, na.rm=T)) %>%
                pull(max_year)
        
}

# make resamples from user train split
make_user_train_resamples = function(train_split,
                                     outcome,
                                     v = 5) {
        
        outcome = enquo(outcome)
        
        # create resamples via cross validation
        vfold_cv(analysis(train_split),
                 v = v,
                 strata = !!outcome)
        
}

# tune user wflows
tune_user_wflows = function(wflows,
                            ids,
                            resamples,
                            metrics,
                            control) {
        
        message("tuning user workflows...")
        
        tictoc::tic("workflow tuning:")
        tuned_wflows = 
                wflows %>%
                filter(wflow_id %in% ids) %>%
                workflow_map(
                        fn = 'tune_grid',
                        resamples = resamples,
                        metrics = metrics,
                        control = control
                )
        tictoc::toc()
        
        tuned_wflows
        
}

# select only necessary from results
trim_predictions = function(res) {
        
        res %>%
                select(wflow_id, .row, where(is.factor), .pred_yes, any_of(c("game_id", "name", "yearpublished")))
        
}

# update workflow set with pre defined tuning grid
add_tuning_grid = function(wflows,
                           wflow_models) {
        
        wflows %>%
                option_add(id = wflows %>% 
                                   filter(grepl(wflow_models, wflow_id)) %>% 
                                   pull(wflow_id),
                           grid = build_tuning_grid(
                                   build_model_spec(wflow_models)
                           )
                )
}

# get best tune for workflows
add_best_tune = function(wflow_results, metric) {
        
        wflow_results %>%
                mutate(best_tune = map(result,
                                       ~ .x %>%
                                               select_best(metric = metric)))
}

# add last fit for workflows
add_last_fit = function(wflow_results,
                        split = user_train_split,
                        metrics) {
        
        wflow_results %>%
                mutate(last_fit = map2(result,
                                       best_tune,
                                       ~ .x %>%
                                               extract_workflow() %>%
                                               finalize_workflow(parameters = .y) %>%
                                               last_fit(split = split,
                                                        metrics = metrics,
                                                        control = ctrl_last_fit())))
        
}

# add hurdle
add_pred_hurdle= function(games,
                          preds) {
        
        preds %>%
                left_join(.,
                          games %>%
                                  select(game_id, name, pred_hurdle),
                          by = c("game_id", "name")
                )
}

# get individual predictions with ids from last fits
collect_last_fit_predictions = function(last_fits) {
        
        last_fits %>%
                unnest(last_fit) %>%
                select(wflow_id, splits, .predictions) %>% 
                mutate(assess = map(splits,~ .x %>% 
                                            assessment %>% 
                                            select(game_id, name, yearpublished))) %>% 
                select(-splits) %>% 
                unnest(everything()) %>%
                trim_predictions() %>%
                arrange(desc(.pred_yes))
        
}

# collect metrics from last fits
collect_last_fit_metrics = function(last_fits) {
        
        last_fits %>%
                unnest(last_fit) %>%
                select(wflow_id, .metrics) %>%
                unnest(.metrics) %>%
                arrange(.metric, .estimate)
        
}

# get game ids
add_game_ids = function(preds, games) {
        
        preds %>%
                left_join(.,
                          games %>%
                                  mutate(.row = row_number()) %>%
                                  select(.row, game_id, name, yearpublished),
                          by = c(".row")) %>%
                trim_predictions()
}

# pre defined model specifications 
build_model_spec = function(model_engine,
                            engine_mode = 'classification') {
        
        if (model_engine == "glmnet" & engine_mode == 'classification') {
                
                logistic_reg(penalty = tune::tune(),
                             mixture = tune::tune()) %>%
                        set_engine("glmnet")
                
        } else if (model_engine == 'glmnet' & engine_mode == 'regression') {
                
                linear_reg(penalty = tune::tune(),
                           mixture = tune::tune()) %>%
                        set_engine("glmnet")
                
        } else if (model_engine == "lightgbm") {
                
                boost_tree(
                        mode = engine_mode,
                        trees = tune(),
                        min_n = tune(),
                        tree_depth = tune()) %>%
                        set_engine("lightgbm", 
                                   objective = "binary")
                
        } else if (model_engine == 'xgboost' & engine_mode == 'classification') {
                
                boost_tree(
                        trees = tune(),
                        min_n = tune(),
                        sample_size = tune(),
                        learn_rate = tune(),
                        tree_depth = tune(),
                        stop_iter = 50
                ) %>%
                        set_mode(engine_mode) %>%
                        set_engine("xgboost",
                                   eval_metric = 'logloss')
                
        } else if (model_engine == 'xgboost' & engine_mode == 'regression') {
                
                boost_tree(
                        trees = tune(),
                        min_n = tune(),
                        sample_size = tune(),
                        learn_rate = tune(),
                        tree_depth = tune(),
                        stop_iter = 50
                ) %>%
                        set_mode(engine_mode) %>%
                        set_engine("xgboost",
                                   eval_metric = 'rmse')
                
        }
        
        else if (model_engine == 'ranger') {
                
                rand_forest(trees = 500,
                            mtry = tune()) %>%
                        set_mode(engine_mode) %>%
                        set_engine("ranger")
                
        } else if (model_engine == 'glm' & engine_mode == 'classification') {
                
                logistic_reg()
                
        } else if (model_engine == 'glm' & engine_mode == 'regression') {
                
                linear_reg()
        }
        
}

build_model_spec_list = function(model_engine,
                             engine_mode = 'classification') {
        
       specs = map(model_engine,
           ~ build_model_spec(.x))
       
       names(specs) = model_engine
       
       specs
        
}


# tuning grids for classification models
build_tuning_grid = function(model_spec,
                        tuning_size = 5) {
        
        if (model_spec$engine == 'lightgbm') {
                
                grid_max_entropy(
                        x = dials::parameters(
                                trees(range = c(250, 500)),
                                min_n(), # 2nd important
                                tree_depth() # 3rd most important
                        ),
                        size = tuning_size
                )
                
        } else if (model_spec$engine == 'glmnet') {
                
                expand.grid(
                        penalty = 10 ^ seq(-3, -1, length = 10),
                        mixture = c(0)
                )
                
        } else {
                grid_max_entropy(
                        extract_parameter_set_dials(model_spec),
                        size = tuning_size
                )
        } 
}

# classification tuning grids
ctrl_grid = function() {
        control_grid(
                save_pred = TRUE,
                allow_par = T,
                parallel_over = "everything",
                verbose = TRUE,
                save_workflow = T,
                event_level = 'second'
        )
}

# control for last_fit
ctrl_last_fit = function() {
        
        control_last_fit(verbose = T,
                         event_level = 'second',
                         allow_par = T)
        
}

# metrics
class_metrics = function() {
        
        metric_set(yardstick::mcc,
                   yardstick::kap,
                   yardstick::precision,
                   yardstick::recall,
                   yardstick::j_index,
                   yardstick::bal_accuracy)
}

# tune over prob metrics
prob_metrics = function() {
        metric_set(yardstick::mn_log_loss,
                   yardstick::roc_auc)
}


# add tuning grid to workflow