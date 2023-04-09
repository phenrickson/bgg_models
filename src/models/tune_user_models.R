# run setup
source(here::here("src", "models", "setup_user_model.R"))

# parameters
username = 'mrbananagrabber'
end_train_year = 2019
outcome = 'ever_owned'

message("fetching user collection...")

# create user objects for training
user_collection = 
        assemble_user_collection(username,
                                 games_imputed)

message(paste("setting up user models for ", '`', outcome,  '`', "...", sep=""))

# create objects for training with user
user_setup = 
        make_user_split_and_resamples(user_collection,
                                      games = games_imputed,
                                      end_train_year = 2019,
                                      valid_window = 2,
                                      outcome = outcome,
                                      min_users = 100)

message("creating user recipes...")

# create user recipes
user_recipes = 
        make_user_recipes(data = user_setup$user_split$train,
                          outcome = user_setup$outcome)

# model specs to tune over
models_list = mget(grep("_class_spec",
              names(.GlobalEnv),
              value=TRUE)) %>%
        # subnames
        set_names(., gsub("_class_spec", "", names(.)))
        
# user wflows
user_wflows = 
        workflow_set(
                preproc =        
                        list(
                        minimal_impute = user_recipes$minimal_impute,
                   #     all_impute = user_recipes$all_impute,
                        all_splines = user_recipes$all_impute_splines,
                        pca = user_recipes$all_impute_splines %>%
                                step_pca(all_numeric_predictors(),
                                         threshold = 0.75,
                                         id = 'pca'),
                        corr = user_recipes$all_impute_splines %>%
                                step_corr(all_numeric_predictors()),
                        trees = user_recipes$all_trees
                ),
                models =
                        models_list,
                cross = T
        ) %>%
        # keep only specified wflows
        filter(wflow_id %in% 
                       c("minimal_impute_glmnet",
                         "minimal_splines_glmnet",
                         "all_impute_glmnet",
                         "all_impute_mars",
                         "all_splines_glmnet",
                     #    "pca_glmnet",
                      #   "pca_knn",
                         "corr_glm",
                         "trees_cart",
                         "trees_xgb",
                         "trees_lightgbm")
        )



# tune --------------------------------------------------------------------

# # penalized regressions

tictoc::tic("tuning glmnet...")

# tune glmnet
glmnet_res =
        user_wflows %>%
        filter(grepl("glmnet", wflow_id)) %>%
        workflow_map(
                fn = 'tune_grid',
                resamples = user_setup$user_train_resamples,
                metrics = prob_metrics,
                control = ctrl_grid,
                grid = glmnet_grid
        )

tictoc::toc()


# tuning results
glmnet_res %>%
        rank_results(rank_metric = 'mn_log_loss',
                     select_best = T)

# tuning predictions
glmnet_res %>%
        collect_predictions(select_best = T,
                                    metric = 'mn_log_loss') %>%
        get_trim_results() %>%
        left_join(., 
                  user_setup$user_split$train %>%
                          mutate(.row = row_number()) %>%
                          select(.row, game_id, name, yearpublished)) %>%
        arrange(desc(.pred_yes)) %>%
        filter(wflow_id == 'all_splines_glmnet') %>%
        print(n = 25)


# fits
glmnet_fits =
        glmnet_res %>%
        mutate(best_tune = map(result,
                               ~ .x %>% select_best(metric = 'mn_log_loss'))) %>%
        mutate(last_fit = map2(result,
                               best_tune,
                               ~ .x %>%
                                       extract_workflow() %>%
                                       finalize_workflow(parameters = .y) %>%
                                       last_fit(user_setup$valid_split,
                                                metrics = prob_metrics,
                                                control = ctrl_last_fit))
        )

# coef plot
library(tidytext)
glmnet_fits %>% 
        select(wflow_id, last_fit) %>%
        mutate(tidied = map(last_fit,
                            ~ .x %>% 
                                    extract_fit_parsnip %>%
                                    tidy))  %>%
        select(wflow_id, tidied) %>%
        unnest(tidied) %>%
        filter(estimate != 0) %>%
        filter(term != '(Intercept)') %>% 
        group_by(wflow_id) %>%
        slice_max(abs(estimate), n = 40) %>% 
        ggplot(aes(x=estimate, y = reorder_within(term, estimate, wflow_id)))+
        geom_point()+
        facet_wrap(wflow_id ~.,
                   scales = "free_y")+
        scale_y_reordered()+
        theme(axis.text.y = element_text(size = 6))
        


# get fits

glmnet_fits %>% 
        select(wflow_id, last_fit) %>%
        mutate(tidied = map(last_fit,
                            ~ .x %>% 
                                    extract_fit_parsnip %>%
                                    tidy))  %>%
        select(wflow_id, tidied) %>%
        unnest(tidied) %>%
        filter(estimate != 0) %>%
        filter(term != '(Intercept)') %>% 
        group_by(wflow_id) %>%
        slice_max(abs(estimate), n = 40) %>% 
        ggplot(aes(x=estimate, y = reorder_within(term, estimate, wflow_id)))+
        geom_point()+
        facet_wrap(wflow_id ~.,
                   scales = "free_y")+
        scale_y_reordered()+
        theme(axis.text.y = element_text(size = 6))


# 
# # k nearest neighbor
# knn_res = 
#         user_wflow_set %>%
#         filter(grepl("knn", wflow_id)) %>%
#         workflow_map(
#                 fn = 'tune_grid',
#                 resamples = user_train_resamples,
#                 metrics = tune_metrics,
#                 control = tune_ctrl,
#                 grid = knn_grid
#         )
# 
# # xgboost
# xgb_res = 
#         glmnet_res = 
#         user_wflow_set %>%
#         filter(grepl("xgb", wflow_id)) %>%
#         workflow_map(
#                 fn = 'tune_grid',
#                 resamples = user_train_resamples,
#                 metrics = tune_metrics,
#                 control = tune_ctrl
#         )
# 
# # light gbm
# lightgbm_res = 
#         glmnet_res = 
#         user_wflow_set %>%
#         filter(grepl("lightgbm", wflow_id)) %>%
#         workflow_map(
#                 fn = 'tune_grid',
#                 resamples = user_train_resamples,
#                 metrics = tune_metrics,
#                 control = tune_ctrl
#         )
# 
# # combine
# rbind(glmnet_res,
#       xgb_res,
#       knn_res,
#       lightgbm_res) %>%
#         rank_results(rank_metric = 'mn_log_loss',
#                      select_best = T)
# 
# 
# # get final fits
# train_fits =
#         rbind(xgb_res,
#               lightgbm_res,
#               knn_res,
#               glmnet_res) %>%
#         mutate(best_tune = map(result,
#                                ~ .x %>% select_best(metric = 'mn_log_loss'))) %>%
#         mutate(last_fit = map2(result,
#                                best_tune,
#                                ~ .x %>%
#                                        extract_workflow() %>%
#                                        finalize_workflow(parameters = .y) %>%
#                                        last_fit(valid_split,
#                                                 metrics = tune_metrics,
#                                                 event_level = 'second'))
#         )
# 
# # get results on validation set
# train_fits %>%
#         select(wflow_id, last_fit) %>%
#         unnest(last_fit) %>%
#         select(wflow_id, .metrics) %>%
#         unnest(.metrics) %>%
#         arrange(.metric, .estimate)
# 
# # look at predictions
# train_fits %>%
#         unnest(last_fit) %>%
#         select(wflow_id, .predictions) %>%
#         unnest(.predictions) %>%
#         group_by(wflow_id) %>%
#         cal_plot_breaks(truth = own,
#                         num_breaks = 5,
#                         estimate = .pred_yes,
#                         event_level = 'second')+
#         facet_wrap(wflow_id ~.)
# left_join(., games_split$valid %>%
#                   mutate(.row = nrow(games_split$train) + row_number()) %>%
#                   select(.row, game_id, name, yearpublished)) %>%
#         select(wflow_id, .pred_yes, own, game_id, name, yearpublished)
# arrange(desc(.pred_yes))
# 
# 
# # get best tunes
# glmnet_best_tunes = 
#         glmnet_res %>%
#         mutate(best_tune = map(result,
#                                ~ .x %>% select_best(metric = 'mn_log_loss'))) %>%
#         mutate(.predictions = map2(result,
#                                    best_tune,
#                                    ~ .x %>% collect_predictions(parameters = .y)))
# 
# # calibration
# library(probably)
# 
# 
# # get fits
# glmnet_fits = 
#         glmnet_res %>%
#         mutate(best_tune = map(result,
#                                ~ .x %>% select_best(metric = 'mn_log_loss'))) %>%
#         mutate(last_fit = map2(result,
#                                best_tune,
#                                ~ .x %>% 
#                                        extract_workflow() %>%
#                                        finalize_workflow(parameters = .y) %>%
#                                        last_fit(valid_split,
#                                                 metrics = tune_metrics,
#                                                 event_level = 'second'))
#         )
# 
# 
# glmnet_fits %>% 
#         select(wflow_id, last_fit) %>%
#         unnest(last_fit) %>%
#         unnest(.metrics) %>%
#         select(wflow_id, id, .metric, .estimate) %>%
#         arrange()
# collect_metrics(select_best = T)
# 
# 
# 
# fit(games)
# glmnet_res %>%
#         
#         mutate(best_tune = map(result,
#                                ~ .x %>% select_best(metric = 'mn_log_loss'))) %>%
#         select(wflow_id, best_tune) %>%
#         unnest(best_tune)
# 
# autoplot(glmnet_res) +
#         facet_wrap(.metric ~ wflow_id,
#                    scales = "free_y")
# 
# glmnet_res %>%
#         collect_predictions(parameters = glmnet_best_tunes)
# 
# 
# # logistic regression
# glm_res = 
#         user_wflow_set %>%
#         filter(grepl("_glm$", wflow_id)) %>%
#         workflow_map(
#                 fn = 'tune_grid',
#                 resamples = user_train_resamples,
#                 metrics = tune_metrics,
#                 control = tune_ctrl)
# 
# 
# glm_fit = glm_res %>%
#         mutate(last_fit = map(result,
#                               ~ .x %>%
#                                       extract_workflow() %>%
#                                       last_fit(valid_split,
#                                                event_level = 'second',
#                                                metrics = tune_metrics))
#         )
# 
# 
# 
# # knn
# 
# 
# 
# 
# # mars
# 
# 
# # glmnet
# glmnet_res = 
#         user_wflow_set %>%
#         extract_workflow(id = 'impute_glmnet') %>%
#         tune_grid(
#                 resamples = user_train_resamples,
#                 control = tune_ctrl,
#                 metrics = tune_metrics,
#                 grid = glmnet_grid)
# 
# # tuning plot
# autoplot(glmnet_res)
# 
# # get best tune
# glmnet_best_tune = 
#         glmnet_res %>%
#         select_best(metric = 'mn_log_loss')
# 
# # get fit
# glmnet_fit = 
#         glmnet_res %>%
#         extract_workflow() %>%
#         finalize_workflow(parameters = glmnet_best_tune) %>%
#         fit(games_split$train)
# 
# # plot
# glmnet_fit %>% 
#         extract_fit_parsnip() %>% 
#         tidy() %>% 
#         filter(estimate !=0 & term!='(Intercept)') %>%
#         ggplot(aes(x=estimate, y=reorder(term, estimate)))+
#         geom_point()
# 
# # get predictions
# glmnet_preds = 
#         glmnet_res %>%
#         collect_predictions(parameters = glmnet_best_tune) %>%
#         arrange(.row) %>%
#         bind_cols(., games_split$train %>%
#                           select(name, game_id, yearpublished)) %>%
#         select(id, .config, penalty, .pred_yes, own, name, game_id, yearpublished)
# 
# # roc
# glmnet_res %>%
#         collect_predictions(parameters = glmnet_best_tune) %>%
#         roc_curve(truth = own,
#                   estimate = .pred_yes,
#                   event_level = 'second') %>%
#         autoplot()
# 
# glmnet_res %>%
#         collect_predictions(parameters = glmnet_best_tune) %>%
#         group_by(prob = plyr::round_any(.pred_yes, .05),
#                  own) %>%
#         count() %>%
#         group_by(prob) %>%
#         mutate(prop = n / sum(n)) %>%
#         mutate(total = sum(n)) %>%
#         filter(own == 'yes') %>%
#         ggplot(aes(x=prob,
#                    size = total,
#                    y = prop))+
#         geom_point()+
#         geom_line(lwd = 0.5)+
#         coord_obs_pred()+
#         geom_abline()+
#         theme_minimal()
# 
# # logistic regression
# glmn_res = 
#         user_wflow_set %>%
#         extract_workflow(id = 'corr_glm') %>%
#         tune_grid(
#                 resamples = user_train_resamples,
#                 control = tune_ctrl,
#                 metrics = tune_metrics)
# 
# # estimate averageweight
# 
# # load user data
# 
# 
# # # extract from pca
# # pca = glmnet_res %>%
# #         extract_workflow(id = 'pca_glmnet') %>%
# #         extract_preprocessor() %>%
# #         prep()
# # 
# # # examine
# # components = 
# #         pca %>%
# #         bake(new_data = NULL) %>%
# #         select(game_id, name, own, starts_with("PC")) %>%
# #         set_names(., str_replace(names(.), "PC0", "PC") %>%
# #                           str_replace("PC0", "PC"))
# # 
# # 
# # 
# # 
# # 
# # 
# # components %>%
# #         ggplot(aes(x=PC1,
# #                    y=PC18))+
# #         geom_point()
# # select(game_id, 
# #        
