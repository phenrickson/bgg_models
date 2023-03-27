# # interpret results from lightgbm
# 
# # interpret
# lightgbm_interpret = function(lightgbm_workflow) {
#         
#         
# }
# mod = 
# 
# 
# # variable importance
# vip = hurdle_last_fit %>%
#         pluck(".workflow", 1) %>%
#         extract_fit_engine() %>%
#         lightgbm::lgb.importance() %>%
#         mutate_if(is.numeric, round, 3) %>%
#         pivot_longer(cols = -c(Feature),
#                      names_to = 'type',
#                      values_to = 'value') %>%
#         group_by(type) %>%
#         slice_max(n = 30,
#                   order_by = value,
#                   with_ties = T)
# 
# # variable importance
# suppressWarnings({
#         vip %>%
#                 mutate(type = factor(type, levels = c("Gain", "Cover", "Frequency"))) %>%
#                 mutate(Feature = abbreviate(str_to_title(gsub("_", " ", Feature)), 25)) %>%
#                 ggplot(aes(x=value,
#                            y=reorder_within(Feature, value, type)))+
#                 geom_col()+
#                 facet_wrap(type ~.,
#                            scales = "free",
#                            ncol = 2)+
#                 scale_y_reordered()+
#                 theme_minimal()+
#                 theme(axis.text.y = element_text(size = 5))+
#                 ylab("Feature")
# })
# 
# # interpret using light gbm
# mod = hurdle_last_fit %>%
#         pluck(".workflow", 1) %>%
#         extract_fit_engine()
# 
# data = hurdle_last_fit %>%
#         pluck(".workflow", 1) %>%
#         extract_mold() %$%
#         predictors %>%
#         as.matrix()
# 
# 
# 
# tune = race_res %>%
#         mutate(best_tune = map(result,
#                                select_best, metric = 'roc_auc')) %>%
#         select(wflow_id, best_tune) %>%
#         filter(wflow_id == 'all_impute_xgb') %>%
#         pluck("best_tune", 1)
# 
# wflow =  race_res %>%
#         extract_workflow(id = 'all_impute_xgb') %>%
#         finalize_workflow(parameters = tune) %>%
#         fit(train)
#         
#         # extract mold
#         mold = wflow %>%
#         extract_mold()
# 
# # extract model
# mod = wflow %>%
#         extract_fit_engine()
# 
# # extract recipe template
# template = wflow %>%
#         extract_preprocessor() %$%
#         template
# 
# # extract training set
# orig =
#         bind_cols(
#                 # outcome
#                 wflow %>%
#                         extract_mold() %$%
#                         outcomes,
#                 # predictors
#                 wflow %>%
#                         extract_mold() %$%
#                         predictors,
#                 # ids
#                 wflow %>%
#                         extract_mold() %$%
#                         extras %$%
#                         roles %$%
#                         id) %>%
#         select(names(template))
# 
# # get outcome var
# outcome =   wflow %>%
#         extract_mold() %$%
#         outcomes
# 
# # get list of features used in model
# features =
#         mod %$%
#         feature_names
# 
# # matrix of features used in model
# mat = wflow %>%
#         extract_mold() %$%
#         predictors %>%
#         as.matrix()
# 
# # bake new
# new =
#         wflow %>%
#         extract_preprocessor() %>%
#         prep(strings_as_factor = F) %>%
#         bake(new_data = valid)
# 
# # run shap
# shap = fastshap::explain(mod,
#                          exact = T,
#                          newdata = new %>%
#                                  select(one_of(features)) %>%
#                                  as.matrix) %>%
#         as_tibble()
# 
# shap[323,] %>%
#         pivot_longer(cols = everything()) %>%
#         mutate(value = -value) %>%
#         mutate(sign = case_when(value > 0 ~ 'positive',
#                                 value < 0 ~ 'negative')) %>%
#         filter(value !=0) %>%
#         group_by(sign) %>%
#         slice_max(order_by = abs(value), n = 10) %>%
#         ggplot(aes(x=value,
#                    fill = value,
#                    y = reorder(name, value)))+
#         geom_col()+
#         geom_vline(xintercept = 0)+
#         ggthemes::scale_fill_gradient2_tableau()
# 
