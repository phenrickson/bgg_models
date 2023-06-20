# what: train user models

# packages ----------------------------------------------------------------

suppressPackageStartupMessages({
        
        # tidyverse packages
        library(tidyverse)
        library(tidymodels)
        
        # set preferences
        tidymodels_prefer()
        
        # specific modeling packages
        library(bonsai)
        
        # my package
        library(bggUtils)
        
})


# setup -------------------------------------------------------------------

# recipes for user models
source(here::here("src", "features", "recipes_user.R"))

# functions for training user models
source(here::here("src", "models", "train_user_models.R"))


# data --------------------------------------------------------------------

# load games with imputed averageweight data
games =
        pins::pin_read(
                board = pins::board_folder(here::here("data", "processed")),
                name = "games_imputed")


# parallel backend --------------------------------------------------------


# # set parallel backend
library(doParallel)
all_cores <- parallel::detectCores(logical = FALSE)
doMC::registerDoMC(cores = all_cores)


# inputs -------------------------------------------------------------------------

# parameters for user training
username = 'mrbananagrabber'
end_train_year = 2020
valid_window = 2
retrain_window = 1
outcome = 'ever_owned'
tune_metric = 'mn_log_loss'


# train_user_model = function(username,
#                             outcome,
#                             end_train_year,
#                             valid_window,
#                             retrain_window) {
#         
#         
#         
# }

# tune ---------------------------------------------------------------------

# get user and games
user_collection_and_games = 
        load_user_collection(username) %>%
        join_bgg_games(games)

# create splits for testing
user_train_split =
        user_collection_and_games %>%
        make_user_split(end_train_year = end_train_year,
                        valid_window = 4)

# make user resamples
# set seed here
set.seed(1999)
user_train_resamples = 
        user_train_split %>%
        make_user_train_resamples(outcome = ever_owned)

# make user recipes
user_recipes = 
        user_train_split %>%
        analysis() %>%
        make_user_recipes_list(outcome = ever_owned)

# make user wflows
user_wflows = 
        workflow_set(
                preproc =        
                        user_recipes,
                models =
                        build_model_specs(c("glmnet", "lightgbm")),
                cross = T
        )

# add tuning grids by model
user_wflows = 
        user_wflows %>%
        # add glmnet grids 
        add_tuning_grid(wflows = .,
                        wflow_models = "glmnet") %>%
        # add lightgbm grid
        add_tuning_grid(wflows = .,
                        wflow_models = "lightgbm")

# select ids to tune
wflow_ids =  user_wflows %>% 
        filter(grepl("all_splines_glmnet|all_trees_lightgbm|all_trees_cart", wflow_id)) %>%
        pull(wflow_id)

message(paste("workflows:", "\n", paste(wflow_ids, collapse = "\n"), sep = ""), sep = "")

# tune user wflows
set.seed(1999)
user_wflows_results = 
        user_wflows %>%
        tune_user_wflows(ids = wflow_ids,
                         resamples = user_train_resamples,
                         metrics = prob_metrics(),
                         control = ctrl_grid())

# # workflow plot
# workflow_plot = 
#         user_wflows_results %>%
#         autoplot()+
#         theme_minimal()
# 
# # collect tuning parameters
# tuning_plots = 
#         user_wflows_results %>%
#         mutate(tuning_plot = map2(result,
#                                  wflow_id,
#                                  ~ autoplot(.x)+
#                                          ggtitle(.y))) %>%
#         pull(tuning_plot)

# collect predictions
training_predictions = 
        user_wflows_results %>%
        collect_predictions(select_best = T,
                            metric = tune_metric) %>%
        add_game_ids(games = analysis(user_train_split))

# collect metrics
training_metrics =
        user_wflows_results %>%
        rank_results(select_best = T,
                     rank_metric = tune_metric) %>%
        arrange(.metric, rank)

# fit on train and assess on valid
training_fits = 
        user_wflows_results %>%
        add_best_tune(metric = tune_metric) %>%
        add_last_fit(split = user_train_split,
                     metrics = prob_metrics())

# assess on validation set ------------------------------------------------

# get predictions for validation set
valid_predictions = 
        training_fits %>%
        collect_last_fit_predictions()

# get metrics for validation set
valid_metrics = 
        training_fits %>%
        collect_last_fit_metrics()

# get thresholds for classification

# finalize model on train and validation -----------------------------------------

# final split
user_final_split = 
        user_collection_and_games %>%
        make_user_split(end_train_year = end_train_year+retrain_window,
                        valid_window = 5)

# finalize
user_final_fits = 
        user_wflows_results %>%
        add_best_tune(metric = tune_metric) %>%
        add_last_fit(split = user_final_split,
                     metrics = prob_metrics())

# get workflows
user_workflows = 
        user_final_fits %>%
        select(wflow_id, last_fit) %>%
        unnest(last_fit) %>%
        select(wflow_id, .workflow)

# upcoming predictions
upcoming_predictions =
        user_final_fits %>%
        collect_last_fit_predictions() %>%
        add_pred_hurdle()

# list of user results
user_output = 
        list("user_collection" = 
                     user_collection_and_games %>% 
                     filter(!is.na(user_load_ts)),
             "training_predictions" = 
                     training_predictions %>%
                     mutate(type = 'resamples'),
             "training_metrics" =
                     training_metrics %>%
                     mutate(type = 'resamples'),
             "valid_predictions" =
                     valid_predictions %>%
                     mutate(type = 'valid'),
             "valid_metrics" = 
                     valid_metrics %>%
                     mutate(type = 'valid'),
             "workflows" = 
                     user_workflows,
             "upcoming_predictions" =
                     upcoming_predictions)



workflows = user_output$user_workflows
results = user_output[-"user_workflows"]

# pin objects
user_output %>%
        pins::pin_write(
                board = pins::board_folder(here::here("models",
                                                      "users")),
                name = paste(username,end_train_year,outcome, sep="_"),
                versioned = T)


