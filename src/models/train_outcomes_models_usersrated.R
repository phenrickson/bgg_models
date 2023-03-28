# train usersrated  -----------------------------------------------------------


# # filters to only games with at least 25 user ratings and do not have missingness on usersrated weight
usersrated_train_and_resamples =
        create_train_and_resamples(data = train_imputed %>%
                                           filter(usersrated>=25),
                                   outcome = log_usersrated)

# get train
usersrated_train = 
        usersrated_train_and_resamples$training

# get resamples
usersrated_resamples =
        usersrated_train_and_resamples$resamples

# builds all recipes for outcome
usersrated_recipes = 
        create_outcome_recipes_conditional(data = usersrated_train,
                                           outcome = log_usersrated)

# create workflow sets

# # linear models
# usersrated_linear_wflows = 
#         create_workflow_set(data = usersrated_train,
#                             # select recipes
#                             recipes = list("base_impute" = usersrated_recipes$base_impute,
#                                            "all_impute" = usersrated_recipes$all_impute,
#                                            "all_impute_splines" = usersrated_recipes$all_impute_splines,
#                                            "all_impute_pca" = usersrated_recipes$all_impute %>%
#                                                    step_pca(all_predictors(),
#                                                             id = "pca",
#                                                             threshold = 0.75)
#                             ),
#                             # select models
#                             models = list("glmnet" = models$glmnet,
#                                           "lm" = models$lm)
#         )
# 
# # tune linear
# usersrated_linear_res = 
#         usersrated_linear_wflows %>%
#         tune_grid_wflows(wflows = .,
#                          resamples = usersrated_resamples)
# 
# 
# # create workflows for decision trees
# usersrated_cart_wflows =
#         create_workflow_set(data = usersrated_train,
#                             # select recipes
#                             recipes = list("base_trees" = usersrated_recipes$base_trees,
#                                            "all_trees" = usersrated_recipes$all_trees),
#                             # select models
#                             models = list("cart" = cart_spec)
#         )
# 
# # tune via race
# usersrated_cart_res =
#         usersrated_cart_wflows %>%
#         tune_race_wflows(.,
#                          resamples = usersrated_resamples)
# 
# # make boosted workflows
# usersrated_boost_wflows = 
#         create_workflow_set(data = usersrated_train,
#                             # select recipes
#                             recipes = list("all_trees" = usersrated_recipes$all_trees),
#                             # select models
#                             models = list("xgb" = xgb_spec)
#         )
# 
# # tune xgb without parallel
# # ctrl_xgb = ctrl
# # ctrl_xgb$allow_par=F
# 
# # tune via race
# usersrated_boost_res =
#         usersrated_boost_wflows %>%
#         tune_race_wflows(.,
#                          resamples = usersrated_resamples)
# 
# # collect results
# usersrated_res = 
#         bind_rows(usersrated_linear_res,
#                   usersrated_cart_res,
#                   usersrated_boost_res)
# 
# # pin results locally
# usersrated_res %>%
#         pin_write(board = pins::board_folder(here::here("models", "results", "usersrated")),
#                   name  = "usersrated_res",
#                   description = paste("trained through", end_train_year)
#         )

# pin read
usersrsated_res =
        pin_read(board = pins::board_folder(here::here("models", "results", "usersrated")),
                 name = "usersrated_res")

# get best mod given training set and finalize fit
# select best model based on log loss
usersrated_best_mod = 
        usersrated_res %>%
        rank_results(select_best = T, rank_metric = 'rmse') %>%
        filter(.metric == 'rmse') %>%
        slice_min(rank, n =1, with_ties = F) %>%
        pull(wflow_id) %>%
        unique

# get tuning parameters
usersrated_best_tune = 
        usersrated_res %>%
        filter(wflow_id == usersrated_best_mod) %>%
        mutate(best_tune = map(result,
                               select_best, metric = 'rmse')) %>%
        select(wflow_id, best_tune) %>%
        pluck("best_tune", 1)

# predict validation set
usersrated_last_fit = 
        usersrated_res %>%
        extract_workflow(id = usersrated_best_mod) %>% 
        finalize_workflow(parameters = usersrated_best_tune) %>%
        fit(usersrated_train)



# # fit on train
# usersrated_train_fit = 
#         usersrated_res %>%
#         extract_workflow(id = usersrated_best_mod) %>%
#         finalize_workflow(parameters = usersrated_best_tune) %>%
#         fit(usersrated_train)
# 
# # save workflow to local board
# usersrated_train_fit %>%
#         pin_write(board = pins::board_folder(here::here("models", "models", "usersrated")),
#                   name = "usersrated_fit",
#                   description = paste("trained through", end_train_year))
# 
# 
# 

# # deploy with vetiver
# # vetiver version (for active use)
# usersrated_vetiver =
#         vetiver::vetiver_model(model = usersrated_train_fit,
#                                model_name = "usersrated_vetiver",
#                                description = paste("xgboost classifiation model trained through",
#                                                    end_train_year , 
#                                                    "to predict usersrated"))
# 
# # test that it works
# testthat::test_that("vetiver model does not error due to package",
#                     testthat::expect_no_error(
#                             usersrated_vetiver %>%
#                                     predict(train %>%
#                                                     sample_n(10)))
# )
# 
# # connect to gcs again
# source(here::here("src", "data", "connect_to_gcs.R"))
# 
# # pin to gcs
# vetiver::vetiver_pin_write(board = deployed_board,
#                            usersrated_vetiver)
