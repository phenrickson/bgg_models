# what: train a series of models to predict outcomes on bgg

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
        
        # background jobs
        library(job)
        
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


# run script to load games
source(here::here("src","data","load_games_data.R"))

message("preprocessing games for modeling...")

# script with functions for standardized preprocessing
source(here::here("src", "data", "preprocess_games.R"))

# use function to preprocess nested data
games_processed = games_nested %>%
        # apply preprocessing from fuction 
        preprocess_games() %>%
        # create outcome variable for hurdle model
        mutate(users_threshold = factor(case_when(!is.na(bayesaverage) ~ 'yes',
                                                  is.na(bayesaverage) ~ 'no'),
                                        levels = c('no', 'yes')))


# data splitting -----------------------------------------------------------


# set end train year
end_train_year = 2019

message(paste("splitting games before and after", end_train_year))

# training set for hurdle model
train =
        games_processed %>%
        filter(yearpublished <= end_train_year)

# valid set for hurdle model
valid =
        games_processed %>%
        filter(yearpublished > end_train_year & yearpublished <= end_train_year +2)

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


# linear regression
lm_spec <-
        linear_reg(mode = "regression") %>%
        set_engine("lm")

# penalized logistic regression
glmnet_spec <- 
        linear_reg(mode = "regression",
                   penalty = tune::tune(),
                   mixture = 0.5) %>%
        set_engine("glmnet")

# specify grid for tuning glmnet
glmnet_grid <- 
        grid_regular(
                penalty(range = c(-5, -.5)),
                levels = 10
        )

# cart
cart_spec <-
        decision_tree() %>%
        set_mode("regression")

# cart grid
cart_grid <- 
        grid_regular(
                cost_complexity(), 
                min_n(), 
                levels = c(cost_complexity = 3, 
                           min_n = 6)
        )

# xgbTree
library(xgboost)
xgb_spec <-
        boost_tree(
                trees = 500,
                min_n = tune(),
                sample_size = tune(),
                learn_rate = tune(),
                tree_depth = tune(),
                stop_iter = 50
        ) %>%
        set_mode("regression") %>%
        set_engine("xgboost",
                   eval_metric = 'rmse')

# # set up grid for tuning
xgb_grid = 
        grid_latin_hypercube(
                tree_depth = tree_depth(range = c(2L, 9L)),
                min_n(),
                sample_size = sample_prop(),
                #  mtry = mtry_prop(c(0.25, 1)),
                learn_rate(),
                size = 30
        )

library(bonsai)
lightgbm_spec <-
        parsnip::boost_tree(
                mode = "regression",
                trees = 500,
                min_n = tune(),
                tree_depth = tune(),
        ) %>%
        set_engine("lightgbm")

# combine all into one list
models =   list(
        cart = cart_spec,
        lm = lm_spec,
        glmnet = glmnet_spec,
        xgb = xgb_spec,
        lightgbm = lightgbm_spec
)

# metrics and controls ----------------------------------------------------


# # specify regression metrics
reg_metrics<-metric_set(yardstick::rmse,
                        yardstick::mae,
                        yardstick::mape,
                        yardstick::rsq)

# control for resamples
ctrl <- control_resamples(save_pred = TRUE, 
                          save_workflow = T,
                          allow_par = T,
                          verbose = T,
                          parallel_over = "resamples")

# control for racing
race_ctrl = 
        finetune::control_race(
                save_pred = TRUE,
                parallel_over = "resamples",
                verbose = TRUE,
                verbose_elim = TRUE,
                save_workflow = T
        )

# set parallelization
all_cores <- parallel::detectCores(logical = FALSE)-1
doMC::registerDoMC(cores = all_cores)

# control for standard tuning
ctrl <-
        control_grid(save_pred = T,
                    parallel_over = 'everything',
                    event_level = 'second',
                    save_workflow = T)

# control for racing
race_ctrl <-
        finetune::control_race(
                save_pred = TRUE,
                parallel_over = "everything",
                event_level = 'second',
                verbose = TRUE,
                verbose_elim = TRUE,
                save_workflow = TRUE
        )

# create function for tuning a workflow set via race
tune_grid_wflows = function(wflows,
                            resamples) {
        
        wflows %>%
                workflow_map(
                        "tune_grid",
                        grid = 10,
                        control = ctrl,
                        resamples = resamples,
                        metrics = reg_metrics
                )
}

# create function for tuning a workflow set via race
tune_race_wflows = function(wflows,
                            resamples) {
        
        wflows %>%
                workflow_map(
                        "tune_race_anova",
                        seed = 1999,
                        resamples = resamples,
                        control = race_ctrl,
                        metrics = reg_metrics
                )
}



# recipes -----------------------------------------------------------------

# load script with generic recipes
source(here::here("src", "features", "recipes_outcomes.R"))

# set levels of dummy extract for outcome models
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

# add splines for nonlinear effects for linear models
splines_recipe_func = function(recipe) {
        
        recipe %>%
                #add spline for truncated yearpublished
                step_ns(year,
                        deg_free = 5) %>%
                # spline for number mechanics
                step_ns(number_mechanics,
                        deg_free = 5) %>%
                # spline for number categories
                step_ns(number_categories,
                        deg_free = 5)
}


# functions for creating workflows/workflowsets  ---------------------------


# creates custom training set and resamples given data and outcome
create_train_and_resamples = function(data, outcome, seed = 1999) {
        
        outcome = enquo(outcome)
        
        # string
        outcome_str = rlang::as_name(outcome)
        
        # check for missingness
        if (colSums(is.na(data[,outcome_str]) > 0)) {
                
                stop(paste("missingness in", outcome_str))
                
        }
        
        # otherwise, create folds
        resamples = vfold_cv(data,
                             v = 5,
                             strata = !!outcome)
        
        message(paste("creating training and resamples for", outcome_str, "with",  nrow(data), "obs"))
        
        return(list("training" = data,
                    "resamples" = resamples))
        
}

# create standard set of recipes for an outcome given data
create_outcome_recipes = function(data,
                          outcome) {
        
        outcome = enquo(outcome)
        
        # basic recipe without publishers/artists/designers
        # tokenize mechanics and categories
        # impute missigness
        # preprocess
        base_impute_recipe = 
                data %>%
                # remove selected variables
                select(-families, -publishers, -artists, -designers) %>%
                # make base recipe %>%
                base_recipe_func(outcome = !!outcome) %>%
                # dummy extract categories and mechanics
                dummy_extract_variable(c(mechanics, categories),
                                       threshold = 1) %>%
                # impute missingness
                impute_recipe_func() %>%
                # basic preprocessing
                preproc_recipe_func() %>%
                # normalize
                step_normalize(all_numeric_predictors()) %>%
                # check missing
                check_missing(all_predictors())
        
        # recipe with all features
        # dummy extract
        # preprocessing
        # imputation
        all_impute_recipe = 
                data %>%
                # make base recipe %>%
                base_recipe_func(outcome = !!outcome) %>%
                # dummy extract categorical
                dummy_recipe_func() %>%
                # impute missingness
                impute_recipe_func() %>%
                # basic preprocessing
                preproc_recipe_func() %>%
                # normalize
                step_normalize(all_numeric_predictors()) %>%
                # check missing
                check_missing(all_predictors())
        
        # splines
        all_impute_splines_recipe =
                data %>%
                # make base recipe %>%
                base_recipe_func(outcome = !!outcome) %>%
                # dummy extract categorical
                dummy_recipe_func() %>%
                # impute missingness
                impute_recipe_func() %>%
                # basic preprocessing
                preproc_recipe_func() %>%
                # splines
                splines_recipe_func() %>%
                # normalize
                step_normalize(all_numeric_predictors()) %>%
                # check missing
                check_missing(all_predictors())
        
        # base with trees
        base_trees_recipe = 
                data %>%
                # remove selected variables
                select(-families, -publishers, -artists, -designers) %>%
                # make base recipe %>%
                base_recipe_func(outcome = !!outcome) %>%
                # dummy extract categories and mechanics
                dummy_extract_variable(c(mechanics, categories),
                                       threshold = 1) %>%
                # basic preprocessing
                preproc_recipe_func()
        
        
        # recipe with all features
        # for trees, so less preprocessing
        all_trees_recipe = 
                data %>%
                # make base recipe %>%
                base_recipe_func(outcome = !!outcome) %>%
                # dummy extract categorical
                dummy_recipe_func() %>%
                # basic preprocessing
                preproc_recipe_func()
        
        recipes = list("base_impute" = base_impute_recipe,
                       "base_trees" = base_trees_recipe,
                       "all_impute" = all_impute_recipe,
                       "all_impute_splines" = all_impute_splines_recipe,
                       "all_trees" = all_trees_recipe)
        
        return(recipes)
        
}
                
# create workflow sets given data, recipes, outcome, and models
create_workflow_set = function(data,
                               recipes, 
                               models) {
        
        # create workflow set
        workflow_sets =
                workflow_set(
                        preproc = recipes,
                        models = models,
                        cross = T
                )
        
        return(workflow_sets)
        
}

# train and impute averageweight -----------------------------------------------------

# # run script to train averageweight model
# source(here::here("src", "models", "train_outcome_models_averageweight.R"))

# read in vetiver model
averageweight_fit = 
        vetiver_pin_read(deployed_board,
         "averageweight_vetiver")

# impute missing averageweight in training and validation sets with this model
impute_averageweight = function(averageweight_fit,
                                data) {
        
        averageweight_fit %>%
                augment(data) %>%
                mutate(averageweight = case_when(is.na(averageweight) ~ .pred,
                                                 TRUE ~ averageweight)) %>%
                # truncate
                mutate(averageweight = case_when(averageweight > 5 ~ 5,
                                                 averageweight < 1 ~ 1,
                                                 TRUE ~ averageweight)) %>%
                select(-.pred)
        
}

# impute averageweight in train
train_imputed = 
        train %>%
        # impute
        impute_averageweight(averageweight_fit,
                             .) 

# impute averageweight in valid
valid_imputed = 
        valid %>%
        # impute
        impute_averageweight(averageweight_fit,
                             .)


# train average, usersrated, bayesaverage ---------------------------------

# update base recipe to include averageweight as a predictor
base_recipe_func_conditional = function(data,
                                          outcome) {
        
        outcome = enquo(outcome)
        
        recipe(x=data) %>%
                # set bgg id variables
                update_role(
                        one_of("game_id",
                               "name",
                               "yearpublished",
                               "image",
                               "thumbnail",
                               "description",
                               # "load_ts",
                               "average",
                               "usersrated",
                               "log_usersrated",
                               "stddev",
                               "numcomments",
                               "numweights",
                               "owned",
                               "trading",
                               "wanting",
                               "wishing",
                               "users_threshold",
                               "averageweight",
                               "bayesaverage"),
                        new_role = "id") %>%
                # set averageweight as predictor
                update_role(one_of("averageweight"),
                            new_role = "predictor") %>%
                # set outcome varable
                update_role(!!outcome,
                            new_role = "outcome") %>%
                # set all others as predictors
                update_role(-has_role("id"),
                            -has_role("outcome"),
                            new_role = "predictor") %>%
                # indicate missingness in numeric features
                step_indicate_na(all_numeric_predictors(),
                                 prefix = "missing") %>%
                # make time per player variable
                step_mutate(time_per_player = playingtime/ maxplayers) %>% 
                # remove zero variance predictors
                step_zv(all_predictors())
        
}

# update recipes to use base recipe with averageweight as a predictor
create_outcome_recipes_conditional = function(data,
                                  outcome) {
        
        outcome = enquo(outcome)
        
        # basic recipe without publishers/artists/designers
        # tokenize mechanics and categories
        # impute missigness
        # preprocess
        base_impute_recipe = 
                data %>%
                # remove selected variables
                select(-families, -publishers, -artists, -designers) %>%
                # make base recipe %>%
                base_recipe_func_conditional(outcome = !!outcome) %>%
                # dummy extract categories and mechanics
                dummy_extract_variable(c(mechanics, categories),
                                       threshold = 1) %>%
                # impute missingness
                impute_recipe_func() %>%
                # basic preprocessing
                preproc_recipe_func() %>%
                # normalize
                step_normalize(all_numeric_predictors()) %>%
                # check missing
                check_missing(all_predictors())
        
        # recipe with all features
        # dummy extract
        # preprocessing
        # imputation
        all_impute_recipe = 
                data %>%
                # make base recipe %>%
                base_recipe_func_conditional(outcome = !!outcome) %>%
                # dummy extract categorical
                dummy_recipe_func() %>%
                # impute missingness
                impute_recipe_func() %>%
                # basic preprocessing
                preproc_recipe_func() %>%
                # normalize
                step_normalize(all_numeric_predictors()) %>%
                # check missing
                check_missing(all_predictors())
        
        # splines
        all_impute_splines_recipe =
                data %>%
                # make base recipe %>%
                base_recipe_func_conditional(outcome = !!outcome) %>%
                # dummy extract categorical
                dummy_recipe_func() %>%
                # impute missingness
                impute_recipe_func() %>%
                # basic preprocessing
                preproc_recipe_func() %>%
                # splines
                splines_recipe_func() %>%
                # normalize
                step_normalize(all_numeric_predictors()) %>%
                # check missing
                check_missing(all_predictors())
        
        # base with trees
        base_trees_recipe = 
                data %>%
                # remove selected variables
                select(-families, -publishers, -artists, -designers) %>%
                # make base recipe %>%
                base_recipe_func_conditional(outcome = !!outcome) %>%
                # dummy extract categories and mechanics
                dummy_extract_variable(c(mechanics, categories),
                                       threshold = 1) %>%
                # basic preprocessing
                preproc_recipe_func()
        
        
        # recipe with all features
        # for trees, so less preprocessing
        all_trees_recipe = 
                data %>%
                # make base recipe %>%
                base_recipe_func_conditional(outcome = !!outcome) %>%
                # dummy extract categorical
                dummy_recipe_func() %>%
                # basic preprocessing
                preproc_recipe_func()
        
        recipes = list("base_impute" = base_impute_recipe,
                       "base_trees" = base_trees_recipe,
                       "all_impute" = all_impute_recipe,
                       "all_impute_splines" = all_impute_splines_recipe,
                       "all_trees" = all_trees_recipe)
        
        return(recipes)
        
}

# # implement previous functions in one go
# build_tune_and_collect_workflows = function(data, outcome, recipes,
#                                             models) {
# 
#         # build
#         workflow_objs =
#                 build_workflow_sets(
#                         data = data,
#                         models = models
#                 )
# 
#         # get workflow sets
#         workflow_sets =
#                 workflow_objs$workflow_sets %>%
#                 filter(wflow_id %in% c('preproc_xgb',
#                                        'preproc_cart',
#                                        'corr_lm',
#                                        'norm_glmnet')) %>%
#                 # add tuning options
#                 option_add(grid = cart_grid, id = c("preproc_cart")) %>%
#                 # glmnet grid
#                 option_add(grid = glmnet_grid, id = c("norm_glmnet")) %>%
#                 # xgb grid
#                 option_add(grid = xgb_grid, id = c("preproc_xgb"))
# 
#         # tune
#         workflows_res =
#                 tune_workflow_sets(
#                         workflow_sets = workflow_sets,
#                         method = 'tune_race_anova',
#                         resamples = workflow_objs$resamples)
# 
#         # collect
#         message("collecting metrics")
# 
#         # metrics
#         metrics = workflows_res %>%
#                 rank_results(select_best = T)
# 
#         # predictions
#         message("collecting predictions")
# 
#         predictions = workflows_res %>%
#                 collect_predictions(select_best = T, metric = 'rmse') %>%
#                 # join with games used in training
#                 left_join(.,
#                           workflow_objs$training %>%
#                                   mutate(.row = row_number()) %>%
#                                   select(.row, game_id, name, yearpublished),
#                           by = c(".row"))
# 
#         # best tune
#         message('selecting best tune')
#         workflows_res = workflows_res %>%
#                 mutate(best_tune = map(result, select_best, n = 1, metric = 'rmse'))
# 
#         return(list(
#                 "workflows_res" = workflows_res,
#                 "metrics" = metrics,
#                 "predictions" = predictions,
#                 "training" = workflow_objs$training,
#                 "resamples" = workflow_objs$resamples)
#         )
# 
# }

# train average, bayesaverage, and usersrated -----------------------------


# train average
source(here::here("src", "models", "train_outcomes_models_average.R"))

# train usersrated
source(here::here("src", "models", "train_outcomes_models_usersrated.R"))


# validate ----------------------------------------------------------------

# load hurdke vetiver
hurdle_fit =
        vetiver_pin_read(deployed_board,
                         name = "hurdle_vetiver")

# load averageweight
averageweight_last_fit =
        pin_read(board = pins::board_folder(here::here("models", "models", "averageweight")),
                  name = "averageweight_fit")

# function to predict
predict_bgg_outcomes = function(data,
                                averageweight_mod,
                                average_mod,
                                usersrated_mod,
                                pivot = T,
                                ratings = 2000) {
        
        
        # predict with hurdle model
        preds = 
                data %>%
                # augment(hurdle_mod,
                #         new_data = .) %>%
                # select(-.pred_no,
                #        -.pred_class) %>%
                # predict with averageweight
                augment(averageweight_mod,
                        new_data = .) %>%
                # rename
                rename(.pred_averageweight = .pred,
                       .actual_averageweight = averageweight) %>%
                # truncate and replace
                mutate(.pred_averageweight = case_when(.pred_averageweight > 5 ~5,
                                                       .pred_averageweight < 1 ~ 1,
                                                       TRUE ~ .pred_averageweight),
                       averageweight = .pred_averageweight) %>%
                # predict with average
                augment(average_mod,
                        new_data = .) %>%
                rename(.pred_average = .pred) %>%
                mutate(.actual_average = average) %>%
                # predict with usersrated
                augment(usersrated_mod,
                        new_data = .) %>%
                rename(.pred_usersrated = .pred) %>%
                mutate(.pred_usersrated = exp(.pred_usersrated)) %>%
                # round
                mutate(.pred_usersrated = plyr::round_any(.pred_usersrated, 50)) %>%
                mutate(.actual_usersrated = usersrated) %>%
                # estimate bayesaverage putting these together
                mutate(.pred_bayesaverage =
                               # numerator
                               ((ratings * 5.5) + (.pred_average * .pred_usersrated)) /
                               # denominator
                               (ratings + .pred_usersrated)
                ) %>%
                # get actual
                rename(.actual_bayesaverage = bayesaverage)
        
        if (pivot == T) {

                # get predictions
                preds = 
                        preds %>%
                        select(yearpublished,
                               game_id,
                               name,
                               starts_with(".pred")) %>%
                        pivot_longer(cols = -c(yearpublished,
                                               game_id,
                                               name),
                                     names_to = c("outcome"),
                                     values_to = c(".pred")) %>%
                        mutate(outcome = gsub(".pred_", "", outcome)) %>%
                        left_join(.,
                                  preds %>%
                                          select(yearpublished,
                                                 game_id,
                                                 name,
                                                 starts_with(".actual")) %>%
                                          pivot_longer(cols = -c(yearpublished,
                                                                 game_id,
                                                                 name),
                                                       names_to = c("outcome"),
                                                       values_to = c(".actual")) %>%
                                          mutate(outcome = gsub(".actual_", "", outcome)))
        }

}

# predict valid with function
valid_preds =
        valid %>%
        predict_bgg_outcomes(data = .,
                             averageweight_last_fit,
                             average_last_fit,
                             usersrated_last_fit)

valid_preds %>%
        left_join(.,
                  augment(hurdle_fit,
                          valid) %>%
                          select(game_id, name, .pred_yes, usersrated)) %>%
        mutate(.actual = case_when(outcome == 'bayesaverage' & is.na(.actual) ~ 5.5,
                                   outcome == 'usersrated' ~ log1p(.actual),
                                   TRUE ~ .actual),
               .pred = case_when(outcome == 'usersrated' ~ log1p(.pred),
                                 TRUE ~ .pred)) %>%
        mutate(.pred_class = case_when(.pred_yes > .2 ~ 'yes',
                                       TRUE ~ 'no')) %>%
        group_by(outcome, .pred_class) %>%
        reg_metrics(truth = .actual,
                    estimate = .pred) %>%
        arrange(outcome) %>%
        spread(.pred_class,)


valid_preds %>%
        reg_metrics(truth = .actual,
                    estimate = .pred)


# predict 
valid_preds %>%
        left_join(.,
                  augment(hurdle_fit,
                          valid) %>%
                          select(game_id, name, usersrated, .pred_yes)) %>%
        filter(.pred_yes > .25 ) %>%
        mutate(.actual = case_when(outcome == 'bayesaverage' & is.na(.actual) ~ 5.5,
                                  outcome == 'usersrated' ~ log1p(.actual),
                                  TRUE ~ .actual),
               .pred = case_when(outcome == 'usersrated' ~ log1p(.pred),
                                TRUE ~ .pred))  %>%
        ggplot(aes(x=.pred,
                   y=.actual,
                   color = .pred_yes))+
        geom_point(alpha = 0.5)+
        facet_wrap(outcome ~.,
                   scales = "free")+
        scale_color_gradient(low = 'red',
                              high = 'dodgerblue2',
                             limits = c(0, 0.25),
                             oob = scales::squish)+
        geom_abline()


# predict new with function
upcoming_preds =
        games_processed %>%
        filter(yearpublished > 2021) %>%
        predict_bgg_outcomes(data = .,
                             averageweight_last_fit,
                             average_last_fit,
                             usersrated_last_fit) %>%
        left_join(.,
                  augment(hurdle_fit,
                          games_processed %>%
                                  filter(yearpublished > 2021)) %>%
                          select(game_id, name, usersrated, .pred_yes))

upcoming_preds %>%
        filter(.pred_yes > .25) %>%
        filter(yearpublished == 2024) %>% 
        filter(outcome == 'bayesaverage') %>% 
        arrange(desc(.pred)) %>%
        mutate(.actual = replace_na(.actual, 5.5))

# refit outcome models to train plus additional year
averageweight_fit = 
        averageweight_last_fit %>%
        fit(bind_rows(train_imputed,
                      valid_imputed) %>%
                    filter(yearpublished <= end_train_year+1) %>%
                    filter(usersrated >=25))

# average
average_fit = 
        average_last_fit %>%
        fit(bind_rows(train_imputed,
                      valid_imputed) %>%
                    filter(yearpublished <= end_train_year+1) %>%
                    filter(usersrated >=25))

# usersrated
usersrated_fit = 
        usersrated_last_fit %>%
        fit(bind_rows(train_imputed,
                      valid_imputed) %>%
                    filter(yearpublished <= end_train_year+1) %>%
                    filter(usersrated >=25))

# predict 2021-2023
upcoming_preds =
        games_processed %>%
        filter(yearpublished > end_train_year +1) %>%
        predict_bgg_outcomes(data = .,
                             averageweight_last_fit,
                             average_fit,
                             usersrated_fit) %>%
        left_join(.,
                  augment(hurdle_fit,
                          games_processed %>%
                                  filter(yearpublished > end_train_year+1)) %>%
                          select(game_id, name, usersrated, .pred_yes))

# 
# valid_preds %>%
#         select(yearpublished,
#                game_id,
#                name,
#                starts_with(".pred"),
#                starts_with(".actual")
#         )
#                       
#         
#         # predict with hurdle model
#         valid_hurdle = 
#                 hurdle_fit %>%
#                 augment(valid,
#                         type = 'prob')
#         
#         # predict with averageweight model
#         valid_imputed = 
#                 hurdle_fit %>%
#                 augment(valid_hurdle,
#                         type = 'prob')
#         
#         
#         
#         
#         
#         
# }
# 
# 
# # predict with averageweight and hurdle
# averageweight_preds =
#         averageweight_last_fit %>%
#         augment(
#                 hurdle_fit %>%
#                         augment(valid_imputed,
#                                 type = 'prob')) %>%
#         rename(.pred_hurdle = .pred_yes) %>%
#         select(game_id,
#                name,
#                yearpublished,
#                averageweight,
#                .pred,
#                averageweight,
#                .pred_hurdle) %>%
#         rename(actual = averageweight) %>%
#         transmute(outcome = 'averageweight',
#                   yearpublished,
#                   game_id,
#                   name,
#                   .pred,
#                   actual,
#                   averageweight,
#                   .pred_hurdle)
# 
# # predict with usersrated and hurdle
# usersrated_preds =
#         usersrated_last_fit %>%
#         augment(
#                 hurdle_fit %>%
#                         augment(valid_imputed,
#                                 type = 'prob')) %>%
#         rename(.pred_hurdle = .pred_yes) %>%
#         select(game_id,
#                name,
#                yearpublished,
#                log_usersrated,
#                .pred,
#                .pred_hurdle) %>%
#         rename(actual = log_usersrated) %>%
#         transmute(outcome = 'usersrated',
#                   yearpublished,
#                   game_id,
#                   name,
#                   .pred,
#                   actual,
#                   .pred_hurdle)
# 
# # predict with average and hurdle
# # preds
# average_preds =
#         average_last_fit %>%
#         augment(
#                 hurdle_fit %>%
#                         augment(valid_imputed,
#                                 type = 'prob')) %>%
#         rename(.pred_hurdle = .pred_yes) %>%
#         select(game_id,
#                name, 
#                yearpublished, 
#                usersrated,
#                .pred, 
#                average,
#                .pred_hurdle) %>%
#         rename(actual = average) %>%
#         transmute(outcome = 'average',
#                   yearpublished,
#                   game_id,
#                   name,
#                   .pred,
#                   actual,
#                   usersrated,
#                   .pred_hurdle)
# 
# # make bayesaverage preds
# bayesaverage_preds = 
#         bind_rows(average_preds,
#                   usersrated_preds)
#         
# bind_rows
# 
# 
# average_preds %>% 
#         ggplot(aes(x=.pred, 
#                    size = usersrated,
#                    y=actual))+
#         geom_point(alpha = 0.5)+
#         coord_obs_pred()
# 
# usersrated_preds %>%
#         ggplot(aes(x=.pred,
#                    size = usersrated,
#                    y=actual))+
#         geom_point(alpha = 0.5)+
#         coord_obs_pred()
