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

# training set: games published before end train year
train =
        games_processed %>%
        filter(yearpublished <= end_train_year)

# valid set: games that have been out for at least 2 years post end train year
valid =
        games_processed %>%
        filter(yearpublished > end_train_year & yearpublished <= end_train_year +2)

# create custom split
# make a split for validating on a specific set of years
valid_split = 
        make_splits(
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
# not run
# source(here::here("src", "models", "train_outcome_models_averageweight.R"))

# # load in averageweight trained through end train year
averageweight_train_fit =
        pin_read(models_board,
                 name = paste(end_train_year, "averageweight_fit", sep="/"))

# run xgb.Booster.complete(.)
averageweight_train_fit %>%
        extract_fit_engine() %>%
        xgb.Booster.complete(.)

# get template for data used in averageweight training
averageweight_template = 
        averageweight_train_fit %>%
        extract_preprocessor %$%
        template
 
# get resampled predictions for averageweight in training set;
# that is, the outcome models will be trained on estimated rather than observed averageweight
set.seed(1999)
averageweight_oos =
        averageweight_train_fit %>%
        # fit model to resamples
        fit_resamples(
                # get resamples via cv
                resamples = 
                        vfold_cv(data = averageweight_template,
                                 v = 5,
                                 strata = 'averageweight'),
                # keep predictions
                control = control_resamples(allow_par = T,
                                            save_pred = T)
        ) %>%
        # collect predictions
        collect_predictions() %>%
        arrange(.row) %>%
        # add game id, name, and yearpublished from template
        left_join(.,
                  averageweight_template %>%
                          select(game_id, name, yearpublished) %>%
                          mutate(.row = row_number()),
                  by = c(".row")
        ) %>%
        transmute(yearpublished, 
                  game_id, 
                  name,
                  est_averageweight = .pred)

# now impute games that do not have averageweight
averageweight_impute = 
        averageweight_train_fit %>%
        # augment
        augment(train %>%
                        filter(!(game_id %in% averageweight_template$game_id))
        ) %>%
        transmute(yearpublished, 
                  game_id, 
                  name,
                  est_averageweight = .pred)
        

# impute missing averageweight in new data
impute_averageweight = function(averageweight_fit,
                                data) {

        averageweight_train_fit %>%
                augment(data) %>%
                rename(est_averageweight = .pred)

}

# estimate averageweight out of sample on training set
# # filters to only games with at least n user ratings and do not have missingness on average weight

# impute averageweight in train
train_imputed = 
        train %>%
        # bind in est_averageweight
        left_join(.,
                  bind_rows(averageweight_oos,
                            averageweight_impute),
                  by = c("game_id", "name", "yearpublished"))

# predict with averageweight model
valid_imputed = 
        valid %>%
        impute_averageweight(averageweight_train_fit,
                             data = .)

# train average, bayesaverage, and usersrated -----------------------------

# # train average
source(here::here("src", "models", "train_outcomes_models_average.R"))

# train usersrated
source(here::here("src", "models", "train_outcomes_models_usersrated.R"))

# load in trained models
# load in average trained through end train year
average_train_fit =
        pin_read(models_board,
                 name = paste(end_train_year, "average_fit", sep="/"))

# run xgb.Booster.complete(.)
average_train_fit %>%
        extract_fit_engine() %>%
        xgb.Booster.complete(.)

# # load in average trained through end train year
usersrated_train_fit =
        pin_read(models_board,
                 name = paste(end_train_year, "usersrated_fit", sep="/"))

# run xgb.Booster.complete(.)
usersrated_train_fit %>%
        extract_fit_engine() %>%
        xgb.Booster.complete(.)


# predict on valid set ----------------------------------------------------------------

# load hurdle vetiver
hurdle_fit =
        vetiver_pin_read(deployed_board,
                         name = "hurdle_vetiver")


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
                # impute averageweight
                impute_averageweight(averageweight_mod,
                                     data =.) %>%
                mutate(.pred_averageweight = est_averageweight,
                       .actual_averageweight = averageweight) %>%
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
                                          mutate(outcome = gsub(".actual_", "", outcome)),
                                  by = c("yearpublished", "game_id", "name", "outcome")
                        )
        }

}

# predict valid with function
valid_preds =
        valid %>%
        predict_bgg_outcomes(data = .,
                             averageweight_train_fit,
                             average_train_fit,
                             usersrated_train_fit)

