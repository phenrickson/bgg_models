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
        
        # tables
        library(gt)
        library(gtExtras) 
        library(DT)
        
})


# setup -------------------------------------------------------------------

# functions used for preprocessing
source(here::here("src", "features", "preprocess_games.R"))

# recipes for user models
source(here::here("src", "features", "recipes_user.R"))

# functions for training user models
source(here::here("src", "models", "train_user_models.R"))

# functions for visualizing workflows
source(here::here("src", "visualizations", "viz_workflows.R"))

# functions for user report
source(here::here("src", "reports", "user_collection_report.R"))



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

# register parallel backend --------------------------------------------------------


# # set parallel backend
library(doParallel)
all_cores <- parallel::detectCores(logical = FALSE)
doMC::registerDoMC(cores = all_cores)


# functions ---------------------------------------------------------------

# function to run everything
train_user_model = function(user_collection,
                            outcome = 'own',
                            bgg_games = games,
                            end_train_year = 2021,
                            valid_window = 2,
                            retrain_window = 0,
                            tune_metric = 'mn_log_loss',
                            ...) {
        
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
             "valid_window" = 
                     valid_window,
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

# build user markdown report
build_user_report = function(user_output,
                             ...) {
        
        # get username
        username = get_username(user_output$user_collection)
        
        # make report name
        build_report_name = function(username,
                                     ...) {
                
                paste(username, user_output$outcome, user_output$end_train_year, sep = "_")
        }
        
        # assign output to global environment for markdown
        user_output <<- user_output
        
        # build markdown report
        rmarkdown::render(
                input = here::here("notebooks", "user_model_report.Rmd"),
                output_dir = here::here("reports", "users"),
                output_file = build_report_name(username),
                params = list(bgg_username = username)
        )
        
}


# run ---------------------------------------------------------------------
# 
# # run for one username
# 'mrbananagrabber' %>%
#         # load user collection from bgg api
#         load_user_collection(username = .) %>%
#         # train model for user
#         train_user_model(user_collection = .,
#                          bgg_games = games,
#                          outcome = 'own',
#                          end_train_year = 2021,
#                          valid_window = 2,
#                          retrain_window = 0,
#                          tune_metric = 'mn_log_loss') %>%
#         # build markdown report
#         build_user_report(user_output = .)

# run over multiple
usernames = c('mrbananagrabber',
              'GOBBluth89')

# via map
map(usernames,
    ~ .x %>%
            load_user_collection(username = .) %>%
            train_user_model(user_collection = .,
                             bgg_games = games,
                             outcome = 'own',
                             end_train_year = 2021,
                             valid_window = 2,
                             retrain_window = 0,
                             tune_metric = 'mn_log_loss') %>%
            build_user_report(user_output = .)
)
