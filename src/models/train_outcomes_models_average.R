# train average  -----------------------------------------------------------

# # filters to only games with at least 25 user ratings and do not have missingness on average weight
average_train_and_resamples =
        create_train_and_resamples(data = train_imputed %>%
                                           filter(!is.na(average)) %>%
                                           filter(usersrated >= 25),
                                   outcome = average)

# get train
average_train = 
        average_train_and_resamples$training

# get resamples
average_resamples =
        average_train_and_resamples$resamples

# builds all recipes for outcome
average_recipes = 
        create_outcome_recipes(data = average_train,
                               outcome = average)

# create workflow sets

# linear models
average_linear_wflows = 
        create_workflow_set(data = average_train,
                            # select recipes
                            recipes = list("base_impute" = average_recipes$base_impute,
                                           "all_impute" = average_recipes$all_impute,
                                           "all_impute_splines" = average_recipes$all_impute_splines,
                                           "all_impute_pca" = average_recipes$all_impute %>%
                                                   step_pca(all_predictors(),
                                                            id = "pca",
                                                            threshold = 0.75)
                            ),
                            # select models
                            models = list("glmnet" = models$glmnet,
                                          "lm" = models$lm)
        )

# tune linear
average_linear_res = 
        average_linear_wflows %>%
        tune_grid_wflows(wflows = .,
                         resamples = average_resamples)

# create workflows for decision trees
average_cart_wflows =
        create_workflow_set(data = average_train,
                            # select recipes
                            recipes = list("base_trees" = average_recipes$base_trees,
                                           "all_trees" = average_recipes$all_trees),
                            # select models
                            models = list("cart" = cart_spec)
        )

# tune via race
average_cart_res =
        average_cart_wflows %>%
        tune_race_wflows(.,
                         resamples = average_resamples)

# make boosted workflows
average_boost_wflows = 
        create_workflow_set(data = average_train,
                            # select recipes
                            recipes = list("all_trees" = average_recipes$all_trees),
                            # select models
                            models = list("xgb" = xgb_spec)
        )

# tune xgb without parallel
# ctrl_xgb = ctrl
# ctrl_xgb$allow_par=F

# tune via race
average_boost_res =
        average_boost_wflows %>%
        tune_race_wflows(.,
                         resamples = average_resamples)

# collect results
average_res = 
        bind_rows(average_linear_res,
                  average_cart_res,
                  average_boost_res)

# pin results locally
average_res %>%
        pin_write(board = pins::board_folder(here::here("models", "results", end_train_year)),
                  name  = "average_res",
                  description = paste("trained through", end_train_year)
        )

# get best mod given training set and finalize fit
# select best model based on log loss
average_best_mod = 
        average_res %>%
        rank_results(select_best = T, rank_metric = 'rmse') %>%
        filter(.metric == 'rmse') %>%
        slice_min(rank, n =1, with_ties = F) %>%
        pull(wflow_id) %>%
        unique

# get tuning parameters
average_best_tune = 
        average_res %>%
        filter(wflow_id == average_best_mod) %>%
        mutate(best_tune = map(result,
                               select_best, metric = 'rmse')) %>%
        select(wflow_id, best_tune) %>%
        pluck("best_tune", 1)

# fit on train
average_train_fit = 
        average_res %>%
        extract_workflow(id = average_best_mod) %>%
        finalize_workflow(parameters = average_best_tune) %>%
        fit(average_train)

# save trained workflow to models board
# prefix with training year in gcs
temp_models_board = 
        # models board
        pins::board_gcs(
                bucket = my_bucket,
                prefix = paste("models/", end_train_year, "/", sep=""),
                versioned = T)

# pin
average_train_fit %>%
        pin_write(board = temp_models_board,
                  name = "average_fit",
                  description = paste("model trained to predict average; trained through", end_train_year))

# # deploy with vetiver
# # vetiver version (for active use)
# average_vetiver =