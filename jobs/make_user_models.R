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
        
        # vetiver
        library(vetiver)
        
})


# setup -------------------------------------------------------------------

# recipes for user models
source(here::here("src", "features", "recipes_user.R"))

# functions for training user models
source(here::here("src", "models", "train_user_models.R"))

# functions for visualizing workflows
source(here::here("src", "visualizations", "viz_workflows.R"))

# functions used for preprocessing
source(here::here("src", "features", "preprocess_games.R"))


# data --------------------------------------------------------------------

# load games with imputed averageweight data for modeling
games =
        pins::pin_read(
                board = pins::board_folder(here::here("data", "processed")),
                name = "games_imputed")

# load bgg games with full information
games_info = 
        pins::pin_read(
                board = pins::board_folder(here::here("data", "processed")),
                name = "games"
        )

# parallel backend --------------------------------------------------------


# # set parallel backend
library(doParallel)
all_cores <- parallel::detectCores(logical = FALSE)
doMC::registerDoMC(cores = all_cores)


# inputs -------------------------------------------------------------------------

# parameters for user training
# username = 'GOBBluth89'
# end_train_year = 2020
# valid_window = 2
# retrain_window = 1
# outcome = 'ever_owned'
# tune_metric = 'mn_log_loss'

# function to run everything
train_user_model = function(user_collection,
                            outcome,
                            bgg_games = games,
                            end_train_year,
                            valid_window,
                            retrain_window,
                            tune_metric) {
        
        # get user and games
        user_collection_and_games = 
                user_collection %>%
                join_bgg_games(bgg_games)
        
        # create splits for train/validtion
        user_train_split =
                user_collection_and_games %>%
                make_user_split(end_train_year = end_train_year,
                                valid_window = valid_window)
        
        # make user resamples
        # set seed here
        set.seed(1999)
        user_train_resamples = 
                user_train_split %>%
                make_user_train_resamples(outcome = any_of(outcome))
        
        # make user recipes
        user_recipes = 
                user_train_split %>%
                analysis() %>%
                make_user_recipes_list(outcome = any_of(outcome))
        
        # make user wflows
        user_wflows = 
                workflow_set(
                        preproc =        
                                user_recipes,
                        models =
                                build_model_spec_list(c("glmnet", "lightgbm")),
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
        
        # get predictions for validation set
        valid_predictions = 
                training_fits %>%
                collect_last_fit_predictions() %>%
                add_pred_hurdle(games = bgg_games)
        
        
        # get metrics for validation set
        valid_metrics = 
                training_fits %>%
                collect_last_fit_metrics()
        
        # get thresholds for classification
        
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
                add_pred_hurdle(games = bgg_games)
        
        # output results
        list("outcome" = 
                     outcome,
             "end_train_year" = 
                     end_train_year,
             "user_collection" =
                     user_collection,
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
                     upcoming_predictions,
             "games_load_ts" = 
                     games$load_ts[1])
        
}


# run function for user
user_collection = 
        load_user_collection(username = 'mrbananagrabber')

set.seed(1999)
user_output = 
        train_user_model(user_collection = user_collection,
                         bgg_games = games,
                         outcome = 'own',
                         end_train_year = 2021,
                         valid_window = 2,
                         retrain_window = 0,
                         tune_metric = 'mn_log_loss')

# save results
user_workflows = user_output$workflows
user_results = user_output[-which(names(user_output) == "workflows")]

# build markdown report


# # pin results
# user_results %>%
#         pins::pin_write(
#                 board = pins::board_folder(here::here("models",
#                                                       "users")),
#                 name = paste(collection$username[1],
#                              user_results$end_train_year,
#                              user_results$outcome, sep="_"),
#                 versioned = T)

# # use vetiver to save glmnet workflow
# vetiver_glmnet = 
#         workflows %>%
#         filter(grepl("glmnet", wflow_id)) %>%
#         pluck(".workflow", 1) %>%
#         vetiver_model(
#                 model = .,
#                 model_name = paste(results$username, 
#                                    results$outcome, 
#                                    "glmnet", sep = "_"),
#                 metadata = list("training_metrics" =
#                                         results$training_metrics %>%
#                                         filter(grepl("glmnet", wflow_id)),
#                                 "valid_metrics" = 
#                                         results$valid_metrics %>%
#                                         filter(grepl("glmnet", wflow_id))),
#                 versioned = T)

# # pin workflow
# vetiver_glmnet %>%
# vetiver::vetiver_pin_write(
#         board = pins::board_folder(here::here("models", "users")))
# 
# 
# # save lightgm workflow via saveRDS
# lightgbm_wflow = workflows %>%
#         filter(grepl("lightghbm", wflow_id)) %>%
#         pluck(".workflow", 1)

# lightgbm::saveRDS.lgb.Booster()
# 
# # pin results
# results %>%
#         pins::pin_write(
#                 board = pins::board_folder(here::here("models",
#                                                       "users")),
#                 name = paste(results$username, 
#                              results$end_train_year,
#                              results$outcome, sep="_"),
#                 versioned = T)
# 
# # bundle workflows
# bundled_workflows = bundle::bundle(workflows)

# filepath
# filepath = paste0(
#         here::here("models/users",
#                    paste(results$username,
#                          results$end_train_year,
#                          results$outcome,
#                          "workflows", sep = "_")),
#         ".rds")

# # save with saveRDS
# tictoc::tic()
# saveRDS(bundled_workflows,
#         compress = F,
#         file = filepath)
# tictoc::toc()

# 
# # pin bundled workflows
# bundled_workflows %>%
#         pins::pin_write(
#                 board = pins::board_folder(here::here("models",
#                                                       "users")),
#                 name = paste(results$username, 
#                              results$outcome,
#                              "workflows", 
#                              sep="_"),
#                 versioned = T)
# 
# # # save workflows
# # bundled_workflows = bundle::bundle(workflows)
# # saveRDS(bundled_workflows,
# #         file = here::here("")
# 
# 
# # # save workflows with vetiver
# # library(vetiver)
# # 
# # v <- vetiver_model(
# #         crash_wf_model, 
# #         "traffic-crash-model", 
# #         metadata = list(metrics = crash_metrics %>% 
# #                                 dplyr::select(-.config),
# #                         predictions = 
# # )
# 
# # 
# # readr::write_rds(
# #         
# # )
# save(workflows,
#      file = 
#              paste0(here::here("models", 
#                                "users",
#                                paste(username,end_train_year, outcome, sep="_")),
#                     ".rds"))
# 
