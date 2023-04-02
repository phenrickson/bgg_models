# train averageweight  -----------------------------------------------------------


# # filters to only games with at least 25 user ratings and do not have missingness on average weight
averageweight_train_and_resamples =
        create_train_and_resamples(data = train %>%
                                           filter(!is.na(averageweight)) %>%
                                           filter(usersrated >= 25) %>%
                                           filter(numweights > 5),
                                   outcome = averageweight)

# get train
averageweight_train = 
        averageweight_train_and_resamples$training

# get resamples
averageweight_resamples =
        averageweight_train_and_resamples$resamples

# builds all recipes for outcome
averageweight_recipes = 
        create_outcome_recipes(data = averageweight_train,
                               outcome = averageweight)

# create workflow sets

# linear models
averageweight_linear_wflows = 
        create_workflow_set(data = averageweight_train,
                            # select recipes
                            recipes = list("base_impute" = averageweight_recipes$base_impute,
                                           "all_impute" = averageweight_recipes$all_impute,
                                           "all_impute_splines" = averageweight_recipes$all_impute_splines,
                                           "all_impute_pca" = averageweight_recipes$all_impute %>%
                                                   step_pca(all_predictors(),
                                                            id = "pca",
                                                            threshold = 0.75)
                            ),
                            # select models
                            models = list("glmnet" = models$glmnet,
                                          "lm" = models$lm)
        )

# tune linear
averageweight_linear_res = 
        averageweight_linear_wflows %>%
        tune_grid_wflows(wflows = .,
                         resamples = averageweight_resamples)


# create workflows for decision trees
averageweight_cart_wflows =
        create_workflow_set(data = averageweight_train,
                            # select recipes
                            recipes = list("base_trees" = averageweight_recipes$base_trees,
                                           "all_trees" = averageweight_recipes$all_trees),
                            # select models
                            models = list("cart" = cart_spec)
        )

# tune via race
averageweight_cart_res =
        averageweight_cart_wflows %>%
        tune_race_wflows(.,
                         resamples = averageweight_resamples)

# make boosted workflows
averageweight_boost_wflows = 
        create_workflow_set(data = averageweight_train,
                            # select recipes
                            recipes = list("all_trees" = averageweight_recipes$all_trees),
                            # select models
                            models = list("xgb" = xgb_spec)
        )

# tune xgb without parallel
# ctrl_xgb = ctrl
# ctrl_xgb$allow_par=F

# tune via race
averageweight_boost_res =
        averageweight_boost_wflows %>%
        tune_race_wflows(.,
                         resamples = averageweight_resamples)

# collect results
averageweight_res = 
        bind_rows(averageweight_linear_res,
                  averageweight_cart_res,
                  averageweight_boost_res)

# pin results locally
averageweight_res %>%
        pin_write(board = pins::board_folder(here::here("models", "results", end_train_year)),
                  name  = "averageweight_res",
                  description = paste("trained through", end_train_year)
        )


# get best mod given training set and finalize fit
# select best model based on log loss
averageweight_best_mod = 
        averageweight_res %>%
        rank_results(select_best = T, rank_metric = 'rmse') %>%
        filter(.metric == 'rmse') %>%
        slice_min(rank, n =1, with_ties = F) %>%
        pull(wflow_id) %>%
        unique

# get tuning parameters
averageweight_best_tune = 
        averageweight_res %>%
        filter(wflow_id == averageweight_best_mod) %>%
        mutate(best_tune = map(result,
                               select_best, metric = 'rmse')) %>%
        select(wflow_id, best_tune) %>%
        pluck("best_tune", 1)

# fit on train
averageweight_train_fit = 
        averageweight_res %>%
        extract_workflow(id = averageweight_best_mod) %>%
        finalize_workflow(parameters = averageweight_best_tune) %>%
        fit(averageweight_train)

# save trained workflow to models board
# prefix with training year in gcs
temp_models_board = 
        # models board
        pins::board_gcs(
                bucket = my_bucket,
                prefix = paste("models/", end_train_year, "/", sep=""),
                versioned = T)
        
averageweight_train_fit %>%
        pin_write(board = temp_models_board,
                  name = "averageweight_fit",
                  description = paste("model trained to predict averageweight; trained through", end_train_year))
