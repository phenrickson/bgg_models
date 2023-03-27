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


# preprocessing -----------------------------------------------------------

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
                            resamples,
                            ctrl = ctrl) {
        
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
                            resamples,
                            ctrl = race_ctrl) {
        
        wflows %>%
                workflow_map(
                        "tune_race_anova",
                        seed = 1999,
                        resamples = resamples,
                        control = ctrl,
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


