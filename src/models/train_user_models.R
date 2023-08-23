# function to run everything
train_user_model = function(user_collection,
                            outcome = 'own',
                            bgg_games = games,
                            end_train_year = 2021,
                            valid_window = 2,
                            retrain_window = 0,
                            model_specs = c('glmnet'),
                            tune_metric = 'mn_log_loss',
                            save_workflow = F,
                            ...) {
        
        # get user and games
        user_collection_and_games = 
                user_collection %>%
                join_bgg_games(bgg_games)
        
        # create splits for train/validtion
        user_train_split =
                user_collection_and_games %>%
                make_user_split(end_train_year = end_train_year,
                                valid_window = valid_window)
        
        # make user resamples
        # set seed here
        set.seed(1999)
        user_train_resamples = 
                user_train_split %>%
                make_user_train_resamples(outcome = any_of(outcome))
        
        # make user recipes
        user_recipes = 
                user_train_split %>%
                analysis() %>%
                make_user_recipes_list(outcome = any_of(outcome))
        
        # make user wflows
        user_wflows = 
                workflow_set(
                        preproc =        
                                user_recipes,
                        models =
                                build_model_spec_list(model_specs),
                        cross = T
                )
        
        # add tuning grids by model
        user_wflows = 
                user_wflows %>%
                # add tuning grids by model
                add_tuning_grid(wflows = .,
                                wflow_models = model_specs)
        
        # select ids to tune
        wflow_ids =  user_wflows %>% 
                filter(grepl("all_splines_glmnet|all_trees_lightgbm|all_trees_cart", wflow_id)) %>%
                pull(wflow_id)
        
        message(paste("workflows:", "\n", paste(wflow_ids, collapse = "\n"), sep = ""), sep = "")
        
        # tune user wflows
        set.seed(1999)
        user_wflows_results = 
                user_wflows %>%
                tune_user_wflows(ids = wflow_ids,
                                 resamples = user_train_resamples,
                                 metrics = prob_metrics(),
                                 control = ctrl_grid())
        
        # collect predictions
        training_predictions = 
                user_wflows_results %>%
                collect_predictions(select_best = T,
                                    metric = tune_metric) %>%
                add_game_ids(games = analysis(user_train_split))
        
        # collect metrics
        training_metrics =
                user_wflows_results %>%
                rank_results(select_best = T,
                             rank_metric = tune_metric) %>%
                arrange(.metric, rank)
        
        # fit on train and assess on valid
        tictoc::tic("fitting models on training set...")
        training_fits = 
                user_wflows_results %>%
                add_best_tune(metric = tune_metric) %>%
                add_last_fit(split = user_train_split,
                             metrics = prob_metrics())
        tictoc::toc()
        
        # get predictions for validation set
        valid_predictions =
                training_fits %>%
                collect_last_fit_predictions() %>%
                add_pred_hurdle(games = bgg_games)
        
        # get metrics for validation set
        valid_metrics =
                training_fits %>%
                collect_last_fit_metrics()
        
        # get thresholds for classification
        tictoc::tic("fitting final models...")
        # final split
        user_final_split =
                user_collection_and_games %>%
                make_user_split(end_train_year = end_train_year+retrain_window,
                                valid_window = 10)
        tictoc::toc()
        
        # finalize
        user_final_fits =
                user_wflows_results %>%
                add_best_tune(metric = tune_metric) %>%
                add_last_fit(split = user_final_split,
                             metrics = prob_metrics())
        
        # get workflows
        user_workflows =
                user_final_fits %>%
                select(wflow_id, last_fit) %>%
                unnest(last_fit) %>%
                select(wflow_id, .workflow)
        
        # upcoming predictions
        upcoming_predictions =
                user_final_fits %>%
                collect_last_fit_predictions() %>%
                add_pred_hurdle(games = bgg_games)
        
        # output results
        results =
                list("outcome" =
                             outcome,
                     "end_train_year" =
                             end_train_year,
                     "valid_window" =
                             valid_window,
                     "user_collection" =
                             user_collection,
                     "training_predictions" =
                             training_predictions %>%
                             mutate(type = 'resamples'),
                     "training_metrics" =
                             training_metrics %>%
                             mutate(type = 'resamples'),
                     "valid_predictions" =
                             valid_predictions %>%
                             mutate(type = 'valid'),
                     "valid_metrics" =
                             valid_metrics %>%
                             mutate(type = 'valid'),
                     "upcoming_predictions" =
                             upcoming_predictions %>%
                             mutate(type = 'upcoming'),
                     "workflows" =
                             user_workflows,
                     "games_load_ts" =
                             bgg_games$load_ts[1]
                )
        
        if (save_workflow == T) {
                
                c(results,
                  list("workflows" = user_workflows)
                )
        }
        
        # # workflow results
        if ("glmnet" %in% model_specs) {
                tictoc::tic("extracting glmnet results...")
                glmnet_results =
                        list(
                                "glmnet_coefs" =
                                        user_workflows %>%
                                        get_workflow(model = "glmnet") %>%
                                        extract_fit_parsnip() %>%
                                        tidy(),
                                "glmnet_coefs_path" =
                                        user_workflows %>%
                                        get_workflow(model = "glmnet") %>%
                                        extract_fit_engine() %>%
                                        tidy(return_zeroes = T)
                        )
                tictoc::toc()
        } else {
                glmnet_results = vector()
        }


        if ("lightgbm" %in% model_specs) {

                tictoc::tic("extracting lightgbm results...")

                lightgbm_results =
                        list(
                                "lightgbm_vip" =
                                        # variable importance
                                        user_workflows %>%
                                        get_workflow(model = "lightgbm") %>%
                                        lightgbm_vip(),
                                "lightgbm_shap" =
                                        get_game_data(
                                                game_data =
                                                        user_collection %>%
                                                        join_bgg_games(bgg_games),
                                                id =
                                                        map_df(
                                                                list(training_predictions,
                                                                     valid_predictions,
                                                                     upcoming_predictions),
                                                                ~ .x %>%
                                                                        filter(grepl("lightgbm", wflow_id)) %>%
                                                                        arrange(desc(.pred_yes)) %>%
                                                                        head(15)
                                                        ) %>%
                                                        pull(game_id),
                                        ) %>%
                                        lightgbm_interpret(workflow = user_workflows %>%
                                                                   get_workflow(model = 'lightgbm'),
                                                           game_data = .,
                                                           outcome = outcome)
                        )
                tictoc::toc()
        } else {
                lightgbm_results = vector()
        }


        out = c(results,
                    glmnet_results,
                    lightgbm_results)

        out

}

tune_user_models = function(user_collection,
                            outcome = 'own',
                            games = games,
                            end_train_year = 2021,
                            valid_window = 2,
                            model_specs = c('glmnet'),
                            tune_wflow_ids = c("all_splines_glmnet|all_trees_lightgbm"),
                            tune_metric = 'mn_log_loss',
                            ...) {
        
        # get user and games
        user_collection_and_games = 
                user_collection %>%
                join_bgg_games(games = games)
        
        # create splits for train/validation
        user_train_split =
                user_collection_and_games %>%
                make_user_split(end_train_year = end_train_year,
                                valid_window = valid_window)
        
        # make user resamples
        # set seed here
        set.seed(1999)
        user_train_resamples =
                user_train_split %>%
                make_user_train_resamples(outcome = any_of(outcome))
        
        # make user recipes
        user_recipes =
                user_train_split %>%
                analysis() %>%
                make_user_recipes_list(outcome = any_of(outcome))
        
        # make user wflows
        user_wflows =
                workflow_set(
                        preproc =
                                user_recipes,
                        models =
                                build_model_spec_list(model_specs),
                        cross = T
                ) %>%
                # add tuning grids by model
                add_tuning_grid(wflows = .,
                                wflow_models = model_specs) %>%
                # filter to only wflows specified for tuning
                filter(grepl(tune_wflow_ids, wflow_id))
        
        # select ids to tune
        wflow_ids =  user_wflows %>%
                pull(wflow_id)
        
        message(paste("workflows:", "\n", paste(wflow_ids, collapse = "\n"), sep = ""), sep = "")
        
        # tune user wflows
        user_wflows_results =
                user_wflows %>%
                tune_user_wflows(ids = wflow_ids,
                                 resamples = user_train_resamples,
                                 metrics = prob_metrics(),
                                 control = ctrl_grid())
        
        # collect predictions
        training_predictions =
                user_wflows_results %>%
                collect_predictions(select_best = T,
                                    metric = tune_metric) %>%
                add_game_ids(games = analysis(user_train_split))
        
        # collect metrics
        training_metrics =
                user_wflows_results %>%
                rank_results(select_best = T,
                             rank_metric = tune_metric) %>%
                arrange(.metric, rank)
        
        # fit on train and assess on valid
        training_fits = 
                user_wflows_results %>%
                add_best_tune(metric = tune_metric) %>%
                add_last_fit(split = user_train_split,
                             metrics = prob_metrics())
        
        # # get predictions for validation set
        valid_predictions =
                training_fits %>%
                collect_last_fit_predictions() %>%
                add_pred_hurdle(games = games)
        
        # # get metrics for validation set
        valid_metrics =
                training_fits %>%
                collect_last_fit_metrics()
        
        list("tuning_results" = user_wflows_results,
             "training_predictions" =
                     training_predictions %>%
                     mutate(type = 'resamples'),
             "training_metrics" =
                     training_metrics %>%
                     mutate(type = 'resamples'),
             "valid_predictions" =
                     valid_predictions %>%
                     mutate(type = 'valid'),
             "valid_metrics" =
                     valid_metrics %>%
                     mutate(type = 'valid'),
             "user_collection" = user_collection)
        
}


extract_user_results = function(tuning_results,
                                retrain_window = 1,
                                ...) {
        
        # extract objects
        tuned_user_wflows = tuning_results$tuning_results
        user_collection = tuning_results$user_collection
        
        # user collection and games
        user_collection_and_games = 
                user_collection %>% 
                join_bgg_games(games = games)
        
        # extract template for tuning
        template = 
                tuned_user_wflows %>%
                pluck("info", 1) %>%
                pluck("workflow", 1) %>% 
                extract_preprocessor() %$% 
                template
        
        # last year used in training
        end_train_year = 
                max(template$yearpublished, na.rm = T)
        
        # collect predictions
        training_predictions = 
                tuned_user_wflows %>%
                collect_predictions(select_best = T,
                                    metric = tune_metric) %>%
                add_game_ids(games = template)   
        
        # collect metrics
        training_metrics =
                tuned_user_wflows %>%
                rank_results(select_best = T,
                             rank_metric = tune_metric) %>%
                arrange(.metric, rank)
        
        # fit on train and assess on valid
        training_fits = 
                tuned_user_wflows %>%
                add_best_tune(metric = tune_metric) %>%
                add_last_fit(split = user_train_split,
                             metrics = prob_metrics())
        
        # get predictions for validation set
        valid_predictions = 
                training_fits %>%
                collect_last_fit_predictions() %>%
                add_pred_hurdle(games = games)
        
        # get metrics for validation set
        valid_metrics = 
                training_fits %>%
                collect_last_fit_metrics()
        
        # final split
        user_final_split = 
                user_collection_and_games %>%
                make_user_split(end_train_year = end_train_year+retrain_window,
                                valid_window = 10)
        
        # finalize models
        final_fits = 
                tuned_user_wflows %>%
                add_best_tune(metric = tune_metric) %>%
                add_last_fit(split = user_final_split,
                             metrics = prob_metrics())
        
        # get workflows
        final_workflows = 
                final_fits %>%
                select(wflow_id, last_fit) %>%
                unnest(last_fit) %>%
                select(wflow_id, .workflow)
        
        # upcoming predictions
        upcoming_predictions =
                final_fits %>%
                collect_last_fit_predictions() %>%
                add_pred_hurdle(games = games)
        
        results =
                list("end_train_year" = 
                             end_train_year,
                     "valid_window" = 
                             valid_window,
                     "user_collection" =
                             user_collection,
                     "training_predictions" =
                             training_predictions %>%
                             mutate(type = 'resamples'),
                     "training_metrics" =
                             training_metrics %>%
                             mutate(type = 'resamples'),
                     "valid_predictions" =
                             valid_predictions %>%
                             mutate(type = 'valid'),
                     "valid_metrics" =
                             valid_metrics %>%
                             mutate(type = 'valid'),
                     "upcoming_predictions" = 
                             upcoming_predictions)
        
}

#         
#         # # workflow plot
#         # workflow_plot = 
#         #         user_wflows_results %>%
#         #         autoplot()+
#         #         theme_minimal()
#         # 
#         # # collect tuning parameters
#         # tuning_plots = 
#         #         user_wflows_results %>%
#         #         mutate(tuning_plot = map2(result,
#         #                                  wflow_id,
#         #                                  ~ autoplot(.x)+
#         #                                          ggtitle(.y))) %>%
#         #         pull(tuning_plot)
#         
#         # collect predictions
#         training_predictions = 
#                 user_wflows_results %>%
#                 collect_predictions(select_best = T,
#                                     metric = tune_metric) %>%
#                 add_game_ids(games = analysis(user_train_split))
#         
#         # collect metrics
#         training_metrics =
#                 user_wflows_results %>%
#                 rank_results(select_best = T,
#                              rank_metric = tune_metric) %>%
#                 arrange(.metric, rank)
#         
#         # fit on train and assess on valid
#         training_fits = 
#                 user_wflows_results %>%
#                 add_best_tune(metric = tune_metric) %>%
#                 add_last_fit(split = user_train_split,
#                              metrics = prob_metrics())
#         
#         # get predictions for validation set
#         valid_predictions = 
#                 training_fits %>%
#                 collect_last_fit_predictions() %>%
#                 add_pred_hurdle(games = bgg_games)
#         
#         # get metrics for validation set
#         valid_metrics = 
#                 training_fits %>%
#                 collect_last_fit_metrics()
#         
#         # get thresholds for classification
#         
#         # final split
#         user_final_split = 
#                 user_collection_and_games %>%
#                 make_user_split(end_train_year = end_train_year+retrain_window,
#                                 valid_window = 5)
#         
#         # finalize
#         user_final_fits = 
#                 user_wflows_results %>%
#                 add_best_tune(metric = tune_metric) %>%
#                 add_last_fit(split = user_final_split,
#                              metrics = prob_metrics())
#         
#         # get workflows
#         user_workflows = 
#                 user_final_fits %>%
#                 select(wflow_id, last_fit) %>%
#                 unnest(last_fit) %>%
#                 select(wflow_id, .workflow)
#         
#         # upcoming predictions
#         upcoming_predictions =
#                 user_final_fits %>%
#                 collect_last_fit_predictions() %>%
#                 add_pred_hurdle(games = bgg_games)
#         
#         # output results
#         results =
#                 list("outcome" = 
#                              outcome,
#                      "end_train_year" = 
#                              end_train_year,
#                      "valid_window" = 
#                              valid_window,
#                      "user_collection" =
#                              user_collection,
#                      "training_predictions" =
#                              training_predictions %>%
#                              mutate(type = 'resamples'),
#                      "training_metrics" =
#                              training_metrics %>%
#                              mutate(type = 'resamples'),
#                      "valid_predictions" =
#                              valid_predictions %>%
#                              mutate(type = 'valid'),
#                      "valid_metrics" =
#                              valid_metrics %>%
#                              mutate(type = 'valid'))
#         
#         # # workflow results
#         # if ("glmnet" %in% model_specs) {
#         #         
#         #         train_coefs = 
#         #         
#         #         user_output$workflows %>%
#         #                 get_workflow(model = "glmnet") 
#         # }
#         #      
#         #      "workflows" =
#         #              user_workflows,
#         #      "upcoming_predictions" =
#         #              upcoming_predictions,
#         #      "games_load_ts" = 
#         #              bgg_games$load_ts[1],
#         #      "train_coefs" = 
#         
# }


# load user colleciton
load_user_collection = function(username) {
        
        message("loading user collection...")
        
        # get user collection
        user_collection = get_user_collection(username) %>%
                mutate(load_ts = as.Date(load_ts)) %>%
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
        
        for(i in 1:length(wflow_models)) {
                
                wflows = wflows %>%
                        option_add(id = wflows %>% 
                                           filter(grepl(wflow_models[i], wflow_id)) %>% 
                                           pull(wflow_id),
                                   grid = build_tuning_grid(
                                           build_model_spec(wflow_models[i])
                                   )
                        )
        }
        
        wflows
        
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
        tune::control_grid(
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
        
        tune::control_last_fit(verbose = T,
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