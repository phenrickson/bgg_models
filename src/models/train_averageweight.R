train_outcome_model = function(data,
                               outcome,
                               models = linear_models(),
                               metrics = reg_metrics(),
                               remove_wflow_ids = "^trees_",
                               tune_method = "tune_race_anova",
                               end_train_year = 2020,
                               valid_window = 2,
                               predictors = predictor_vars(),
                               ids = id_vars(),
                               splines = spline_vars(),
                               ...) {
        
        # prep data given type of outdcome being trained
        processed_data = 
                data %>%
                preprocess_outcome(outcome = {{outcome}})
        
        # create split
        split = 
                processed_data %>%
                make_train_valid_split(
                        end_train_year = end_train_year,
                        valid_window = valid_window
                )
        
        # extract training and validation
        # analysis
        train = 
                analysis(split)
        
        # assessment
        valid = 
                assessment(split)
        
        # other
        other = 
                processed_data %>%
                filter(!(game_id %in% (bind_rows(train, valid) %>% pull(game_id))))

        # resamples for tuning on training set
        resamples =
                train %>%
                make_resamples(
                        outcome = averageweight
                )
        
        # build basic recipe
        recipe = 
                train %>%
                make_recipe(
                        outcome = {{outcome}},
                        predictors = predictors,
                ) %>%
                add_bgg_dummies() %>%
                add_preprocessing()
        
        # build other recipes
        recipes = 
                make_outcome_recipes(recipe,
                                     spline_vars = splines)
        
        # make workflows
        workflows = 
                workflow_set(
                        preproc = 
                                recipes,
                        models = 
                                models,
                ) %>%
                # remove ids based on pattern
                filter(!grepl(remove_wflow_ids, wflow_id))
        
        # tune
        tuning_results = 
                workflows %>%
                tune_workflows(
                        method = 'tune_race_anova',
                        resamples = resamples,
                )
        
        # get tuning metrics across all
        tuning_metrics = 
                tuning_results %>%
                rank_results(select_best = T) 
        
        # get best fit
        train_fit = 
                tuning_results %>%
                fit_best()
        
        # predict validation
        valid_preds = train_fit %>%
                augment(valid)
        
        # assess 
        valid_metrics =
                valid_preds %>%
                metrics(
                        truth = averageweight,
                        estimate = .pred)
        
        # predict other
        other_preds = 
                train_fit %>%
                augment(other)
        
        # get final fit
        final_fit = 
                train_fit %>%
                fit(bind_rows(train,
                              valid))
        
        # return results
        list(
                "tuning_results" = tuning_results,
                "tuning_metrics" = tuning_metrics,
                "valid_preds" = valid_preds,
                "valid_metrics" = valid_metrics,
                "other_preds" = other_preds,
                "train_fit" = train_fit,
                "final_fit" = final_fit
        )
        
}

# 
# train_outcome_model(outcome = averageweight,
#                     data = games_processed,
#                     end_train_year = 2019,
#                     valid_window = 2,
#                     predictors = predictor_vars())
# 
# # make workflows
# workflows = 
#         workflow_set(
#                 preproc = 
#                         recipes,
#                 models = 
#                         models,
#         ) %>%
#         filter(!(wflow_id) %in% wflow_ids)
# 
# # tune
# tuning_results = 
#         workflows %>%
#         tune_workflows(
#                 method = 'tune_race_anova',
#                 resamples = resamples,
#         )
# 
# # get tuning metrics across all
# tuning_metrics = 
#         tuning_results %>%
#         rank_results(select_best = T) 
# 
# # get best fit
# train_fit = 
#         tuning_results %>%
#         fit_best()
# 
# # predict validation
# valid_preds = train_fit %>%
#         augment(valid)
# 
# # assess 
# valid_metrics =
#         valid_preds %>%
#         metrics(
#                 truth = averageweight,
#                 estimate = .pred)
# 
# recipe = 
#         train %>%
#         make_recipe(
#                 outcome = averageweight
#         ) %>%
#         add_bgg_dummies() %>%
#         add_preprocessing()
# 
# }
# 
# # train valid split
# split = 
#         games_processed %>%
#         prep_averageweight() %>%
#         make_train_valid_split(
#                 end_train_year = end_train_year,
#                 valid_window = valid_window
#         )
# 
# # extract training and validation
# # analysis
# train = 
#         analysis(split)
# 
# # assessment
# valid = 
#         assessment(split)
# 
# # other
# other = games_processed %>%
#         filter(!(game_id %in% (bind_rows(train, valid) %>% pull(game_id))))
# 
# # resamples for tuning on training set
# resamples = 
#         train %>%
#         make_resamples(
#                 outcome = averageweight
#         )
# 
# # recipes
# recipe = 
#         train %>%
#         make_recipe(
#                 outcome = averageweight
#         ) %>%
#         add_bgg_dummies() %>%
#         add_preprocessing()
# 
# # expand recipes
# recipes = 
#         list(
#                 "trees" = 
#                         recipe,
#                 "impute_normalize" = 
#                         recipe %>%
#                         add_imputation() %>%
#                         add_zv() %>%
#                         add_normalize(),
#                 "impute_splines" = 
#                         recipe %>%
#                         add_imputation() %>%
#                         add_splines() %>%
#                         add_zv() %>%
#                         add_normalize(),
#                 # "impute_discrete" =
#                 #         recipe %>%
#                 #         add_imputation() %>%
#                 #         add_discrete(outcome = averageweight) %>%
#                 #         add_normalize(),
#                 "impute_pca" = 
#                         recipe %>%
#                         add_imputation() %>%
#                         add_splines() %>%
#                         add_zv() %>%
#                         add_normalize() %>%
#                         add_pca()
#         )
# 
# linear_models = 
#         list(
#                 "lm" = 
#                         linear_reg(mode = "regression") %>%
#                         set_engine("lm"),
#                 "glmnet" = 
#                         linear_reg(mode = "regression",
#                                    penalty = tune::tune(),
#                                    mixture = 0) %>%
#                         set_engine("glmnet"),
#                 "pls" = 
#                         pls(
#                                 mode = "regression",
#                                 predictor_prop = tune::tune(),
#                                 num_comp = tune::tune(),
#                                 engine = "mixOmics"
#                         )
#         )
# 
# tree_models = 
#         list(
#                 "xgbTree" = 
#                         boost_tree(
#                                 trees = 500,
#                                 min_n = tune(),
#                                 sample_size = tune(),
#                                 learn_rate = tune(),
#                                 tree_depth = tune(),
#                                 stop_iter = 50
#                         ) %>%
#                         set_mode("regression") %>%
#                         set_engine("xgboost",
#                                    eval_metric = 'rmse'),
#                 "lightgbm" = 
#                         parsnip::boost_tree(
#                                 mode = "regression",
#                                 trees = 500,
#                                 min_n = tune(),
#                                 tree_depth = tune(),
#                         ) %>%
#                         set_engine("lightgbm")
#         )
# 
# models = c(linear_models,
#            tree_models)
# 
# workflows = 
#         workflow_set(
#                 preproc = 
#                         recipes,
#                 models = 
#                         models,
#         ) %>%
#         # remove workflows with tree recipes and linear models
#         filter(!(wflow_id %in% paste0("trees_", names(linear_models)))) %>%
#         # remove impute with trees
#         filter(!(grepl("^impute_", wflow_id) & grepl(paste(names(tree_models), collapse = "|"), wflow_id))) %>%
#         # remove pca and pls
#         filter(!(grepl("_pca_", wflow_id) & grepl("_pls", wflow_id)))
# 
# 
# # tune
# tuning_results = 
#         workflows %>%
#         tune_workflows(
#                 method = 'tune_race_anova',
#                 resamples = resamples,
#         )
# 
# # save tuning results
# 
# # # results
# # tuning_results %>%
# #         autoplot()+
# #         theme_bgg()
# # 
# # # table of tuning results
# tuning_results %>%
#         rank_results(select_best = T) %>%
#         select(wflow_id,
#                .metric, mean) %>%
#         pivot_wider(id_cols = c("wflow_id"),
#                     names_from = c(".metric"),
#                     values_from = c("mean")) %>%
#         arrange(rmse)
# 
# # ggplot(aes(y=mean,
# #            color = model,
# #            label = wflow_id,
# #            x=rank,
# #            tooltip = paste(wflow_id,
# #                            paste("metric:", .metric),
# #                            paste("estimate: ", round(mean, 2)),
# #                            paste("rank:", rank),
# #                            sep = "\n")))+
# # ggiraph::geom_point_interactive()+
# # geom_text(angle = 90,
# #           size = 2.5,
# #            hjust = -0.1)+
# # facet_wrap(.metric ~.,
# #            ncol = 2,
# #            scales = "free")+
# # theme_bgg()
# 
# # # plot tuning results
# # ggiraph::girafe(ggobj = plot_tuning_results,
# #                 width = 9)
# 
# # # autoplot specific workflow
# # tuning_results %>%
# #         pluck("result", 1) %>%
# #         autoplot()
# 
# # get best fit
# train_fit = 
#         tuning_results %>%
#         fit_best()
# 
# # predict validation
# valid_preds = train_fit %>%
#         augment(valid)
# 
# valid_metrics =
#         valid_preds %>%
#         metrics(
#                 truth = averageweight,
#                 estimate = .pred)
# 
# # predict other
# other_preds = train_fit %>%
#         augment(other)
# 
# # get final fit
# final_fit = 
#         train_fit %>%
#         fit(bind_rows(train,
#                       valid))
# 
# # return results
# list(
#         "tuning_results" = tuning_results,
#         "tuning_metrics" = tuning_metrics,
#         "valid_preds" = valid_preds,
#         "valid_metrics" = valid_metrics,
#         "other_preds" = other_preds,
#         "train_fit" = train_fit,
#         "final_fit" = final_fit
# )
# 
# }
# 
# # # get component scores
# # fit %>%
# #         tidy() %>%
# #         filter(type != 'outcomes') %>%
# #         group_by(component) %>%
# #         slice_max(n = 25,
# #                   order_by = abs(value),
# #                   with_ties = F) %>%
# #         ungroup() %>%
# #         mutate(term = present_text(term)) %>%
# #         ggplot(aes(y=tidytext::reorder_within(term, value, component),
# #                    x = value))+
# #         geom_col()+
# #         facet_wrap(paste("Component", component) ~.,
# #                    scales ="free")+
# #         tidytext::scale_y_reordered()+
# #         theme_bgg()+
# #         ylab("")
# 
# # # get loadings
# # fit %>%
# #         extract_recipe() %>%
# #         tidy()
# # 
# # best_workflow = 
# #         tuning_results %>%
# #         rank_results(rank_metric = 'rmse', select_best = T) %>%
# #         filter(rank == 1) %>%
# #         pull(wflow_id) %>%
# #         head(1)
# # 
# # best_tune = 
# #         tuning_results %>%
# #         mutate(best_tune = map(result,
# #                                ~ .x %>% select_best(metric = 'rmse')))
# # 
# # workflow =
# #         tuning_results %>%
# #         extract_workflow(id = best_workflow) %>%
# #         finalize_workflow(parameters = best_tune %>%
# #                                   pluck("best_tune", 1))
# 
# # fit %>%
# #         predict_sim(
# #                 new_data = valid,
# #                 nsims = 100
# #         ) %>%
# #         mutate(across(everything(),
# #                ~ case_when(.x > 5 ~ 5,
# #                            .x < 1 ~ 1,
# #                            TRUE ~ .x))) %>%
# #         apply(1, quantile, probs = c(0.05, 0.5, 0.95), names = F) %>%
# #         t() %>%
# #         set_colnames(., c(".pred_low", ".pred_mid", ".pred_high")) %>%
# #         bind_cols(valid %>%
# #                           select(game_id, name,
# #                                  averageweight,
# #                                  numweights),
# #                   .) %>%
# #         filter(game_id %in% (valid %>% sample_n(50) %>% pull(game_id))) %>%
# #         mutate(name = abbreviate(name, minlength = 40)) %>%
# #         ggplot(aes(ymin = .pred_low,
# #                    y = averageweight,
# #                    ymax = .pred_high,
# #                    x = reorder(name, .pred_mid)))+
# #         geom_pointrange()+
# #         geom_point(aes(color = numweights),
# #                    size = 3)+
# #         coord_flip()+
# #         xlab("")+
# #         theme_bgg()+
# #         scale_color_gradient(
# #                 limits = c(0, 100),
# #                              oob = scales::squish)+
# #         guides(color = guide_colorbar(barwidth = 10,
# #                                       barheight = 0.5))
# # 
# # fit %>%
# #         predict_sim(
# #                 new_data = valid,
# #                 nsims = 1000
# #         ) %>%
# #         mutate(across(everything(),
# #                       ~ case_when(.x > 5 ~ 5,
# #                                   .x < 1 ~ 1,
# #                                   TRUE ~ .x))) %>%
# #         select(any_of(paste0("sim_", sample(1:1000, 50)))) %>%
# #         mutate(.row = row_number()) %>%
# #         pivot_longer(cols = -c(.row)) %>%
# #         ggplot(aes(x=value))+
# #         geom_density(aes(group = name),
# #                      color = 'deepskyblue1')+
# #         geom_density(data = valid,
# #                      aes(x=averageweight))+
# #         theme_bgg()
# # 
# # ggplot()+
# # geom_density(data = valid,
# #              aes(x=averageweight))
# # 
# # 
# # 
# # 
# # 
# #         
# # 
# # 
# # 
# # prepped_fit =
# #         workflow %>%
# #         prep_interval(
# #                 fit_data = train,
# #                 n_boot = 2
# #         )
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # plot = foo %>%
# #        augment(averageweight_valid) %>%
# #         ggplot(aes(x=averageweight,
# #                    alpha = log(numweights),
# #                    y=.pred,
# #                    tooltip = name,
# #                    ))+
# #         ggiraph::geom_point_interactive()
# # 
# # ggiraph::girafe(ggobj = plot,
# #                 width = 8)
# # foo %>%
# #         extract_fit_parsnip() %>%
# #         tidy() %>%
# #         arrange(desc(estimate))
# # 
# # averageweight_results
# #         autoplot()
# #         pluck("result", 1) %>%
# #         select(id, .metrics) %>% 
# #         unnest(.metrics) %>%
# #         ggplot(aes(x=penalty,
# #                    y=.estimate, 
# #                    group = id,
# #                    color = as.factor(mixture)))+
# #         geom_line()+
# #         facet_wrap(~.metric, 
# #                    scales = "free")
# 
