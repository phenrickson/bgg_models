# what: train a hurdle model to predict whether a game will receive a geek rating 
# aka, predict whether a game is expected to get at least 30 user ratings

# dependencies:
# load analysis tables (locally) or run query

# packages ----------------------------------------------------------------

# load core packages needed for modeling
suppressPackageStartupMessages({
        
        # tidyverse/modeling
        library(tidyverse)
        library(tidymodels)
        library(tidyselect)
        
        # tidymodels preferences
        tidymodels_prefer()
        
        # recipes and workflows
        library(recipes)
        library(textrecipes)
        library(workflows)
        library(workflowsets)
        
        # additional
        library(magrittr)
        library(broom.mixed)
        library(data.table)
        library(tidytext)
        library(conflicted)
        library(lubridate)
        
        # ggplot
        library(ggforce)
        
        # pins
        library(pins)
        
        # vetiver
        library(vetiver)
        
        # get my own package
        #devtools::install_github("phenrickson/bggUtils")
        library(bggUtils)
        
}
)

# conflicts
suppressMessages({
        conflict_prefer("year", "lubridate")
        conflict_prefer("quarter", "lubridate")
        conflict_prefer("set_names", "magrittr")
        conflict_prefer("flatten", "purrr")
        conflict_prefer("tune", "tune")
        conflict_prefer("plotly", "layout")
})


# data --------------------------------------------------------------------

# load tables used in modeling
# local version
load(here::here("data", "processed", "games_nested.Rdata"))

# # query from gcp (and update local)
# source(here::here("pull_analysis_games_tables.R"))

# publisher white list
processed_board = board_folder(here::here("data", "processed"),
                               versioned = T)

# read in publisher allow list
publisher_allow_names = processed_board %>%
        pin_read("publisher_allow_names")

# read in publisher allow list
families_filter_names = processed_board %>%
        pin_read("families_filter_names")


# preprocessing -----------------------------------------------------------


message("preprocessing games for modeling...")

# script with functions for standardized preprocessing
source(here::here("src", "data", "preprocess_games.R"))


# use function to preprocess nested data
games_processed = games_nested %>%
        # apply preprocessing from fuction 
        preprocess_games()

# functions for modeling specific outcomes


# data splitting -----------------------------------------------------------


# set end train year
end_train_year = 2019

message(paste("splitting games before and after", end_train_year))

# training set for hurdle model
train =
        games_processed %>%
        filter(yearpublished <= end_train_year)

# valid set for hurdle model
valid=
        games_processed %>%
        filter(yearpublished > end_train_year & yearpublished < end_train_year +2)

# create custom split
# make a split for validating on a specific set of years
valid_split = make_splits(
        list(analysis =
                     seq(nrow(train)),
             assessment =
                     nrow(train) + seq(nrow(valid))),
        bind_rows(train,
                  valid)
)


# models ------------------------------------------------------------------


# base logit
glm_class_spec = 
        logistic_reg()

# penalized logistic regression via glmnet
glmnet_class_spec = 
        logistic_reg(penalty = tune::tune(),
                     mixture = 0.5) %>%
        set_engine("glmnet")

# regularization
library(glmnet)
glmnet_grid = 
        grid_regular(
                penalty(range = c(-5, -.5)),
                levels = 10
        )

# partial least squares discriminant analysis
# library(plsmod)
# pls_class_spec =
#         pls() %>%
#         set_mode("classification")

# cart for classification
cart_class_spec <-
        decision_tree(
                cost_complexity = tune(),
                tree_depth = tune(),
                min_n = 30
        ) %>%
        set_mode("classification") %>%
        set_engine("rpart")

# xgb for class
library(xgboost)
xgb_class_spec <-
        boost_tree(
                trees = 500,
                min_n = tune(),
                sample_size = tune(),
                learn_rate = tune(),
                tree_depth = tune(),
                stop_iter = 50
        ) %>%
        set_mode("classification") %>%
        set_engine("xgboost",
                   eval_metric = 'logloss')

# random forest
library(ranger)
rf_class_spec = 
        rand_forest(trees = 500,
                    mtry = tune()) %>%
        set_mode("classification") %>%
        set_engine("ranger")

# lightgbm
library(bonsai)
lightgbm_class_spec <-
        parsnip::boost_tree(
                mode = "classification",
                trees = 500,
                min_n = tune(),
                tree_depth = tune(),
        ) %>%
        set_engine("lightgbm", objective = "binary")


# metrics and resamples ---------------------------------------------------

# classification metrics
# probabilities
prob_metrics = metric_set(yardstick::mn_log_loss,
                          yardstick::roc_auc)

# class
class_metrics = metric_set(yardstick::accuracy,
                           yardstick::bal_accuracy,
                           yardstick::mcc,
                           yardstick::kap,
                           yardstick::j_index,
                           yardstick::f_meas,
                           yardstick::precision,
                           yardstick::recall,
                           yardstick::ppv)

# create resamples with 5 fold repeated cross validation
set.seed(1999)
train_folds = vfold_cv(train,
                       v = 5,
                       strata = users_threshold)

# create recipes ----------------------------------------------------------

# load script with recipes
source(here::here("src", "features", "recipes_outcomes.R"))

# set levels of dummy extract for hurdle model
dummy_recipe_func = function(recipe) {
        
        recipe %>%
                # include all mechanics
                dummy_extract_variable(mechanics,
                                       threshold = 1) %>%
                # include all categories
                dummy_extract_variable(categories,
                                       threshold = 1) %>%
                # families at min 100
                dummy_extract_variable(families,
                                       threshold = 100) %>%
                # publishers at 50
                dummy_extract_variable(publishers,
                                       threshold = 50) %>%
                # designers at 25
                dummy_extract_variable(designers,
                                       threshold = 25) %>%
                # artists min 50
                dummy_extract_variable(artists,
                                       threshold = 50)
}


# basic recipe without publishers/artists/designers
# tokenize mechanics and categories
# impute missigness
# preprocess
base_recipe = 
        train %>%
        # remove selected variables
        select(-families, -publishers, -artists, -designers) %>%
        # make base recipe %>%
        base_recipe_func(outcome = users_threshold) %>%
        # dummy extract categories and mechanics
        dummy_extract_variable(c(mechanics, categories),
                               threshold = 1) %>%
        # impute missingness
        impute_recipe_func() %>%
        # basic preprocessing
        preproc_recipe_func() %>%
        # check missing
        check_missing(all_predictors())

# recipe with all features
# dummy extract
# preprocessing
# imptuation
impute_recipe = 
        train %>%
        # make base recipe %>%
        base_recipe_func(outcome = users_threshold) %>%
        # dummy extract categorical
        dummy_recipe_func() %>%
        # impute missingness
        impute_recipe_func() %>%
        # basic preprocessing
        preproc_recipe_func() %>%
        # check missing
        check_missing(all_predictors())

# recipe with all features
# for trees, so less preprocessing
trees_recipe = 
        train %>%
        # make base recipe %>%
        base_recipe_func(outcome = users_threshold) %>%
        # dummy extract categorical
        dummy_recipe_func() %>%
        # basic preprocessing
        preproc_recipe_func()


# workflows ---------------------------------------------------------------

# set parallelization
all_cores <- parallel::detectCores(logical = FALSE)-1
doMC::registerDoMC(cores = all_cores)

# control for racing
race_ctrl <-
        finetune::control_race(
                save_pred = TRUE,
                parallel_over = "resamples",
                event_level = 'second',
                verbose = TRUE,
                verbose_elim = TRUE,
                save_workflow = TRUE
        )

# create function for tuning a workflow set via race
tune_race_wflows = function(wflows) {
        
        wflows %>%
                workflow_map(
                        "tune_race_anova",
                        seed = 1999,
                        resamples = train_folds,
                        control = race_ctrl,
                        metrics = prob_metrics
                )
}

# # pin results
# pin_results = function(results) {
#         
#         results %>%
#                 pins::pin_write(board = results_board,
#                                 x = .,
#                                 name = paste(deparse(quote(results)), end_train_year, sep ="_"))
# }
#         

# tuning ------------------------------------------------------------------

# board for storing results
results_board = pins::board_folder(here::here("models", "results", "hurdle", end_train_year),
                                   versioned = T)

# single decision tree
cart_wflows = 
        workflow_set(
                # recipe with preprocessing/imputation
                preproc = 
                        list( 
                                # no publishers/artists/designers, imputation
                                base = base_recipe,
                                # all features, for trees
                                all = trees_recipe),
                # model specifications
                models =
                        list(
                                cart = cart_class_spec
                        ),
                cross = T)

# tune cart
cart_res = 
        cart_wflows %>%
        tune_race_wflows()

# save results
cart_res %>%
        unnest(result) %>%
        select(wflow_id, id, .order, .metrics) %>%
        unnest(.metrics) %>%
        pin_write(.,
                  name = "cart_res",
                  board = results_board)

# linear models
linear_wflows = 
        workflow_set(
                # recipe with preprocessing/imputation
                preproc = 
                        list( 
                                # no publishers/artists/designers; imputation, normalization
                                base_norm = base_recipe %>%
                                        step_normalize(all_predictors()),
                                # no publishers/artists/designers; imputation, normalization, correlation  and nzv filter
                                base_norm_filt = base_recipe %>%
                                        step_normalize(all_predictors()) %>%
                                        step_nzv(all_predictors()) %>%
                                        step_corr(all_predictors(),
                                                  threshold = 0.9),
                                # all features; imputation, with normalization
                                all_norm = impute_recipe %>%
                                        step_normalize(all_predictors()),
                                # all features; imputation, normalized, correlation and nzv filter
                                all_norm_filt= impute_recipe %>%
                                        step_normalize(all_predictors()) %>%
                                        step_nzv(all_predictors()) %>%
                                        step_corr(all_predictors(),
                                                  threshold = 0.9)
                        ),
                # model specifications
                models =
                        list(
                                glm = glm_class_spec,
                                glmnet = glmnet_class_spec
                        ),
                cross = T) %>%
        # select wflows
        filter(wflow_id %in% 
                       c("base_norm_glmnet",
                         "base_norm_filt_glm",
                         "all_norm_glmnet",
                         "all_norm_filt_glm")
        )

# tune
linear_res = 
        linear_wflows %>%
        tune_race_wflows()

# save results from the fold level
linear_res %>%
        unnest(result) %>%
        select(wflow_id, id, .order, .metrics) %>%
        unnest(.metrics) %>%
        pin_write(.,
                  name = "linear_res",
                  board = results_board)

# xgb wflows
boost_wflows = 
        workflow_set(
                # recipe with preprocessing/imputation
                preproc = 
                        list( 
                                # all features, for trees
                                all_impute = trees_recipe),
                # model specifications
                models =
                        list(
                                xgb = xgb_class_spec,
                                lightgbm = lightgbm_class_spec
                        ),
                cross = T)

# tune
boost_res = 
        boost_wflows %>%
        tune_race_wflows()

# pin results
boost_res %>%
        unnest(result) %>%
        select(wflow_id, id, .order, .metrics) %>%
        unnest(.metrics) %>%
        pin_write(.,
                  name = "boost_res",
                  board = results_board)

# # rf wflows
# rf_wflows = 
#         workflow_set(
#                 # recipe with preprocessing/imputation
#                 preproc = 
#                         list( 
#                                 # all features, for trees
#                                 all = trees_recipe %>%
#                                         impute_recipe_func()
#                         ),
#                 # model specifications
#                 models =
#                         list(
#                                 rf = rf_class_spec
#                         ),
#                 cross = T)
# 
# # tune
# rf_res = 
#         rf_wflows %>%
#         tune_race_wflows()
# 
# # pin results
# rf_res %>%
#         unnest(result) %>%
#         select(wflow_id, id, .order, .metrics) %>%
#         unnest(.metrics) %>%
#         pin_write(.,
#                   name = "rf_res",
#                   board = results_board)

# ### partial least squares discriminant analysis
# pls_wflows = 
#         workflow_set(
#                 # recipe with preprocessing/imputation
#                 preproc = 
#                         list( 
#                                 # no publishers/artists/designers, imputation
#                                 all_norm = impute_recipe %>%
#                                         step_normalize(all_predictors())),
#                 # model specifications
#                 models =
#                         list(
#                                 pls = pls_class_spec
#                         ),
#                 cross = T)
# 
# # tune
# pls_res = 
#         pls_wflows %>%
#         tune_race_wflows()


# get results of tuning
race_res =
        bind_rows(cart_res,
                  linear_res,
                  boost_res)

# pin results

# collect metrics
race_metrics = race_res %>%
        rank_results(select_best = T, rank_metric = 'mn_log_loss')

# pin metrics
race_metrics %>%
        pin_write(board = results_board,
                  name = "race_metrics")

# select best model based on log loss
best_mod = race_metrics %>%
        filter(.metric == 'mn_log_loss') %>%
        slice_min(rank, n =1, with_ties = F) %>%
        pull(wflow_id) %>%
        unique

# gather predictions
race_preds = race_res %>%
        collect_predictions(select_best = T, metric = 'mn_log_loss')

# assess cutpoint for classification metrics
assess_cutpoint = 
        map(seq(0.1, 0.9, 0.01),
            ~ race_preds %>%
                    mutate(.pred_class = factor(case_when(.pred_yes > .x ~ 'yes',
                                                          TRUE ~ 'no'),
                                                levels = c("no", "yes"))) %>%
                    mutate(cutpoint = .x) %>%
                    group_by(wflow_id, cutpoint) %>%
                    class_metrics(truth = users_threshold,
                                  estimate = .pred_class)) %>%
        bind_rows

# examine estimates across cutpoint
assess_cutpoint %>%
        mutate(method = stringr::str_match(wflow_id, '(.*)_(.*)')[,3]) %>%
        mutate(wflow_id = factor(wflow_id,
                                 levels = race_metrics %>% 
                                         filter(.metric == 'mn_log_loss') %>%
                                         pull(wflow_id))) %>%
        ggplot(aes(x=cutpoint,
                   color = wflow_id,
                   y=.estimate))+
        geom_line(lwd = 1.02)+
        facet_wrap(.metric ~.,
                   scales = "free_y")+
        theme_minimal()+
        scale_color_viridis_d()

# apply a custom cost function
# predicting that game will not meet the users threshold when it does is far more costly than the opposite
# in this case, i'll consider the cost of a false negative to be 10 times more worse than the cost of a false positive
assess_costs = 
        map(seq(0, 0.9, 0.01),
            ~ race_preds %>%
                    mutate(.pred_class = factor(case_when(.pred_yes > .x ~ 'yes',
                                                          TRUE ~ 'no'),
                                                levels = c("no", "yes"))) %>%
                    mutate(cutpoint = .x) %>%
                    mutate(cost = case_when(users_threshold == .pred_class ~ 0,
                                            users_threshold == 'yes' & .pred_class == 'no' ~ 10,
                                            users_threshold == 'no' & .pred_class == 'yes' ~ 1)) %>%
                    group_by(wflow_id, cutpoint) %>%
                    summarize(cost = sum(cost),
                              .groups = 'drop'))%>%
        bind_rows %>%
        mutate(wflow_id = factor(wflow_id,
                                 levels = race_metrics %>% 
                                         filter(.metric == 'mn_log_loss') %>%
                                         pull(wflow_id)))

# plot cutpoint
assess_costs %>% {
        
        ggplot(.,
               aes(x=cutpoint,
                   color = wflow_id,
                   y = cost))+
                geom_line(lwd = 1.04)+
                theme_minimal()+
                scale_color_viridis_d()+
                geom_vline(xintercept = 
                                   slice_min(., cost, n= 1, with_ties = F) %>%
                                   pull(cutpoint),
                           linetype = 'dashed',
                           alpha = 0.6)+
                scale_y_continuous()
}


# get cutpoint
cutpoint = assess_costs %>%
        group_by(wflow_id) %>%
        slice_min(cost, n = 1, with_ties = F) %>%
        filter(wflow_id == best_mod) %>%
        pull(cutpoint)

# confusion matrix
race_preds %>%
        filter(wflow_id == best_mod) %>%
        mutate(.pred_class = factor(case_when(.pred_yes > cutpoint ~ 'yes',
                                              TRUE ~ 'no'),
                                    levels = c("no", "yes"))) %>%
        yardstick::conf_mat(users_threshold,
                            estimate = .pred_class) %>%
        autoplot(type = 'heatmap')

# gather metrics
# metrics = race_res %>%
#         collect_metrics(select_best=T, metric = 'roc_auc')

# get best tune
best_tune = race_res %>%
        mutate(best_tune = map(result,
                               select_best, metric = 'roc_auc')) %>%
        select(wflow_id, best_tune) %>%
        filter(wflow_id == best_mod) %>%
        pluck("best_tune", 1)

# finalize mod
hurdle_last_fit = 
        race_res %>%
        extract_workflow(id = best_mod) %>%
        finalize_workflow(parameters = best_tune) %>%
        last_fit(valid_split,
                 metrics = prob_metrics)

# performance on validation
hurdle_last_fit %>%
        collect_metrics()

# get valid preds
valid_preds = hurdle_last_fit %>%
        collect_predictions() %>%
        left_join(.,
                  valid %>%
                          select(game_id, name, yearpublished) %>%
                          mutate(.row = nrow(train) + row_number()),
                  by = c(".row")) %>%
        select(.row, game_id, name, yearpublished, users_threshold, .pred_yes) %>%
        mutate(.pred_class = factor(case_when(.pred_yes >= cutpoint ~ 'yes',
                                              TRUE ~ 'no'),
                                    levels = c("no", "yes")))

valid_preds %>%
        ggplot(aes(x=.pred_yes,
                   fill = users_threshold))+
        geom_density(alpha = 0.8)+
        theme_minimal()

# prob
valid_preds %>%
        prob_metrics(truth = users_threshold,
                     estimate = .pred_class,
                     .pred_yes,
                     event_level = 'second')

# class
valid_preds %>%
        class_metrics(truth = users_threshold,
                      estimate = .pred_class,
                      .pred_yes,
                      event_level = 'second')

# lift
suppressWarnings({
        valid_preds %>%
                yardstick::gain_curve(truth = users_threshold,
                                      estimate = .pred_yes,
                                      event_level = 'second')
}) %>%
        autoplot()

# pr curve
suppressWarnings({
        valid_preds %>%
                yardstick::pr_curve(truth = users_threshold,
                                    estimate = .pred_yes,
                                    event_level = 'second')
}) %>%
        autoplot()

# variable importance
vip = hurdle_last_fit %>%
        pluck(".workflow", 1) %>%
        extract_fit_engine() %>%
        lightgbm::lgb.importance() %>%
        mutate_if(is.numeric, round, 3) %>%
        pivot_longer(cols = -c(Feature),
                     names_to = 'type',
                     values_to = 'value') %>%
        group_by(type) %>%
        slice_max(n = 30,
                  order_by = value,
                  with_ties = T)

# variable importance
suppressWarnings({
        vip %>%
                mutate(type = factor(type, levels = c("Gain", "Cover", "Frequency"))) %>%
                mutate(Feature = abbreviate(str_to_title(gsub("_", " ", Feature)), 25)) %>%
                ggplot(aes(x=value,
                           y=reorder_within(Feature, value, type)))+
                geom_col()+
                facet_wrap(type ~.,
                           scales = "free",
                           ncol = 2)+
                scale_y_reordered()+
                theme_minimal()+
                theme(axis.text.y = element_text(size = 6))+
                ylab("Feature")
})

# interpret using light gbm
mod = hurdle_last_fit %>%
        pluck(".workflow", 1) %>%
        extract_fit_engine()

data = hurdle_last_fit %>%
        pluck(".workflow", 1) %>%
        extract_mold() %$%
        predictors %>%
        as.matrix()

# 
# interpret = lgb.interprete(mod,
#                          data,
#                          47816)[[1]] %>%
#         mutate_if(is.numeric, round, 3)
#                          
# interpret %>%
#         filter(Contribution > 0) %>%
#         slice_max(abs(Contribution), n = 40) %>%
#         ggplot(aes(x=Contribution,
#                    y = reorder(Feature, Contribution)))+
#         geom_col()+
#         theme_minimal()

# save model with vetiver
model_board = pins::board_folder(here::here("models", "board"))
vetiver_hurdle = vetiver::vetiver_model(model = hurdle_fit,
                                        model_name = "hurdle_model",
                                        description = paste("classifiation model trained through", end_train_year, "to predict whether games will hit user ratings threshold"))

# test that it predicts
testthat::test_that("vetiver model does not error due to package",
                    testthat::expect_no_error(
                            vetiver_hurdle %>% 
                                    predict(hurdle_train %>% 
                                                    sample_n(10)))
)

# store
vetiver::vetiver_pin_write(model_board, vetiver_hurdle)

# deploy



# library(probably)
# hurdle_preds %>%
#         filter(wflow_id == 'base_xgb') %>%
#         cal_plot_breaks(users_threshold, .pred_yes,
#                         event_level = 'second',
#                         num_breaks = 25)

# # confusion matrix
# last_fit %>% 
#         collect_predictions() %>%
#         mutate(.pred_class = factor(case_when(.pred_yes > cutpoint ~ 'yes',
#                                               TRUE ~ 'no'),
#                                     levels = c("no", "yes"))) %>%
#         yardstick::conf_mat(truth = users_threshold,
#                             estimate = .pred_class) %>%
#         autoplot(type = 'heatmap')+
#         ggtitle("Confusion Matrix for Validation Set",
#                 subtitle = paste('xgboost with cutpoint at', cutpoint))

# get preds for validation set
# valid_preds = last_fit %>% 
#         collect_predictions() %>%
#         mutate(.pred_class = factor(case_when(.pred_yes > cutpoint ~ 'yes',
#                                               TRUE ~ 'no'),
#                                     levels = c("no", "yes")))

# # examine
# valid_preds %>%
#         select(.pred_yes, .row, .pred_class, users_threshold) %>%
#         bind_cols(., valid %>% 
#                           select(game_id, name, usersrated)) %>%
#         arrange(.pred_yes) %>% 
#         mutate_if(is.numeric, round, 3)  %>%
#         filter(.pred_class == 'no') %>%
#         arrange(desc(usersrated))
# 
# # plot probs
# valid_preds %>%
#         ggplot(aes(x=.pred_yes,
#                    fill = users_threshold))+
#         geom_density(alpha = 0.8)+
#         facet_wrap(users_threshold ~., ncol = 1)+
#         theme_phil()+
#         theme(legend.title = element_text())

# # get results on validation set
# hurdle_wflow = last_fit %>% 
#         extract_workflow() %>%
#         finalize_workflow(parameters = best_tune) %>%
#         fit(hurdle_train)

# pin workflow


## shapley

# extract mold
mold = wflow %>%
        extract_mold()

# extract model
mod = wflow %>%
        extract_fit_engine()

# extract recipe template
template = wflow %>%
        extract_preprocessor() %$%
        template

# extract training set
orig =
        bind_cols(
                # outcome
                wflow %>%
                        extract_mold() %$%
                        outcomes,
                # predictors
                wflow %>%
                        extract_mold() %$%
                        predictors,
                # ids
                wflow %>%
                        extract_mold() %$%
                        extras %$%
                        roles %$%
                        id) %>%
        select(names(template))

# get outcome var
outcome =   wflow %>%
        extract_mold() %$%
        outcomes

# get list of features used in model
features =
        mod %$%
        feature_names

# matrix of features used in model
mat = wflow %>%
        extract_mold() %$%
        predictors %>%
        as.matrix()

# bake new
new =
        wflow %>%
        extract_preprocessor() %>%
        prep(strings_as_factor = F) %>%
        bake(new_data = valid)

# run shap
shap = fastshap::explain(mod,
                         exact = T,
                         newdata = new %>%
                                 select(one_of(features)) %>%
                                 as.matrix) %>%
        as_tibble()

shap[323,] %>%
        pivot_longer(cols = everything()) %>%
        mutate(value = -value) %>%
        mutate(sign = case_when(value > 0 ~ 'positive',
                                value < 0 ~ 'negative')) %>%
        filter(value !=0) %>%
        group_by(sign) %>%
        slice_max(order_by = abs(value), n = 10) %>%
        ggplot(aes(x=value,
                   fill = value,
                   y = reorder(name, value)))+
        geom_col()+
        geom_vline(xintercept = 0)+
        ggthemes::scale_fill_gradient2_tableau()

