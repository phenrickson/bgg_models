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

# script containing functions and setup for user collection modeling
#source(here::here("src", "models", "setup_user_models.R"))

# recipes for users
source(here::here("src", "features", "recipes_user.R"))


# boards ------------------------------------------------------------------

# connect to gcs boards
source(here::here("src", "data", "gcs_boards.R"))

# data --------------------------------------------------------------------

# load games with imputed averageweight data
games =
        pins::pin_read(
                board = pins::board_folder(here::here("data", "processed")),
                name = "games_imputed")

# parallel ----------------------------------------------------------------

# # set parallel backend
# library(doParallel)
# all_cores <- parallel::detectCores(logical = FALSE)
# doMC::registerDoMC(cores = all_cores)


# training setup ------------------------------------------------------------------


# list of models to train
models = 
        list(
                # logistic regression
                glm =  logistic_reg(),
                
                # penalized logistic regression via glmnet
                glmnet = 
                        logistic_reg(penalty = tune::tune(),
                                     mixture = tune::tune()) %>%
                        set_engine("glmnet"),
                
                # decision tree
                cart =             
                        decision_tree(
                                cost_complexity = tune(),
                                tree_depth = tune(),
                                min_n = tune()) %>%
                        set_mode("classification") %>%
                        set_engine("rpart"),
                
                # lightgbm
                lightgbm = 
                        parsnip::boost_tree(
                                mode = "classification",
                                trees = 500,
                                min_n = tune(),
                                tree_depth = tune()) %>%
                        set_engine("lightgbm", 
                                   objective = "binary")
                
        )

# grids for tuning
tuning_grids = 
        list(
                # penalized logistic regression
                glmnet = 
                        expand.grid(
                                penalty = 10 ^ seq(-3, -1, length = 10), 
                                mixture = c(0)
                        ),
                
                # decision tree
                cart = 
                        grid_max_entropy(
                                extract_parameter_set_dials(models$cart),
                                size = 10
                        ),
                
                # lightgbm
                lightgbm = 
                        grid_max_entropy(
                                x = dials::parameters(
                                        min_n(), # 2nd important
                                        tree_depth() # 3rd most important
                                ),
                                size = 20
                        )
        )

# tuning controls
ctrl_grid = 
        control_grid(
                save_pred = TRUE,
                allow_par = T,
                parallel_over = "everything",
                verbose = TRUE,
                save_workflow = T,
                event_level = 'second'
        )

# control for last_fit
ctrl_last_fit = 
        control_last_fit(verbose = T,
                         event_level = 'second',
                         allow_par = T)

# metrics
class_metrics = metric_set(yardstick::mcc,
                           yardstick::kap,
                           yardstick::precision,
                           yardstick::recall,
                           yardstick::j_index,
                           yardstick::bal_accuracy)

# tune over prob metrics
prob_metrics = metric_set(yardstick::mn_log_loss,
                          yardstick::roc_auc)



# functions ----------------------------------------------------------------

# load user colleciton
load_user_collection = function(username) {
        
        # get user collection
        user_collection = get_user_collection(username) %>%
                rename(user_load_ts = load_ts)
        
        # if less than 30 games ever owned, stop
        if (nrow(user_collection) < 30) {
                warning("not enough games in collection for reliable modeling")
        } else if (nrow(user_collection) < 50) {
                warning("relatively few games in collection; results may not be reliable")
        }
        
        user_collection
        
}

# join collection with bgg games
join_bgg_games = function(collection,
                          games) {
        
        # required packages
        require(tidyverse)
        
        message("joining with bgg games")
        
        # join up bgg games with collection
        join_games = function(collection,
                              games) {
                
                games %>%
                        left_join(.,
                                  collection,
                                  by = c("game_id", "name")
                        )
        }
        
        # function to create outcomes and set factors for user variables
        prep_collection = function(collection) {
                
                collection %>%
                        mutate(ever_owned = case_when(own == 1 | prevowned == 1 ~ 'yes',
                                                      TRUE ~ 'no'),
                               own = case_when(own == 1 ~ 'yes',
                                               TRUE ~ 'no'),
                               rated = case_when(own == 1 ~ 'yes',
                                                 TRUE ~ 'no'),
                               highly_rated = case_when(rating >= 8 ~ 'yes',
                                                        TRUE ~ 'no')
                        ) %>%
                        mutate_at(vars(own, ever_owned, rated),
                                  factor, levels = c("no", "yes"))
                
        }
        
        
        # join games with collection
        collection_and_games = collection %>%
                # join 
                join_games(., games) %>%
                # prep
                prep_collection()
        
        # how many games in bgg?
        message(paste(nrow(collection_and_games), "games from bgg"))
        
        # how many games joined for modeling?
        message(paste(nrow(collection_and_games %>% 
                                   filter(!is.na(user_load_ts))), "games in user collection"))
        
        collection_and_games
        
        
}

# filter training set to only include games with minimum number of users
filter_min_users = function(data,
                            min_users = 30) {
        
        data %>%
                filter(usersrated >=min_users) 
        
}

# create training/test splits and resamples
# helper function for splitting collection based on year
make_user_split = function(data,
                                 end_train_year,
                                 valid_window,
                                 min_users = 30,
                                 filter_min_users = F) {
        
        train = 
                data %>%
                filter(usersrated >=min_users) %>%
                filter(yearpublished <= end_train_year)
        
        
        if (filter_min_users == T) {
                valid = 
                        data %>%
                        filter_min_users() %>%
                        filter(yearpublished > end_train_year & yearpublished <= end_train_year + valid_window)
        } else {
                valid = 
                        data %>%
                        filter(yearpublished > end_train_year & yearpublished <= end_train_year + valid_window)
                
        }
        
        make_splits(
                list(analysis =
                             seq(nrow(train)),
                     assessment =
                             nrow(train) + seq(nrow(valid))),
                bind_rows(train,
                          valid)
        )
        
}

find_max_year = function(split) {
        
        bind_rows(analysis(split),
                  assessment(split)) %>%
                summarize(max_year = max(yearpublished, na.rm=T)) %>%
                pull(max_year)
        
}

# make resamples from user train split
make_user_train_resamples = function(train_split,
                                     outcome,
                                     v = 5) {
        
        outcome = enquo(outcome)
        
        # create resamples via cross validation
        vfold_cv(analysis(train_split),
                 v = v,
                 strata = !!outcome)
        
}

# tune user wflows
tune_user_wflows = function(wflows,
                            ids,
                            resamples,
                            metrics,
                            control) {
        
        message("tuning user workflows...")
        
        tictoc::tic("workflow tuning:")
        tuned_wflows = 
                wflows %>%
                filter(wflow_id %in% ids) %>%
                workflow_map(
                        fn = 'tune_grid',
                        resamples = resamples,
                        metrics = metrics,
                        control = control
                )
        tictoc::toc()
        
        tuned_wflows
        
}

# select only necessary from results
trim_predictions = function(res) {
        
        res %>%
                select(wflow_id, .row, where(is.factor), .pred_yes, any_of(c("game_id", "name", "yearpublished")))
        
}

# get best tune for workflows
add_best_tune = function(wflow_results, metric) {
        
        wflow_results %>%
                mutate(best_tune = map(result,
                                       ~ .x %>%
                                               select_best(metric = metric)))
}

# add last fit for workflows
add_last_fit = function(wflow_results,
                        split = user_train_split) {
        
        wflow_results %>%
                mutate(last_fit = map2(result,
                                       best_tune,
                                       ~ .x %>%
                                               extract_workflow() %>%
                                               finalize_workflow(parameters = .y) %>%
                                               last_fit(split = split,
                                                        metrics = prob_metrics,
                                                        control = ctrl_last_fit)))
        
}

# add hurdle
add_pred_hurdle= function(preds) {
        
        preds %>%
                left_join(.,
                          user_collection_and_games %>%
                                  select(game_id, name, pred_hurdle),
                          by = c("game_id", "name")
                          )
}

# get individual predictions with ids from last fits
collect_last_fit_predictions = function(last_fits) {
        
        last_fits %>%
                unnest(last_fit) %>%
                select(wflow_id, splits, .predictions) %>% 
                mutate(assess = map(splits,~ .x %>% 
                                            assessment %>% 
                                            select(game_id, name, yearpublished))) %>% 
                select(-splits) %>% 
                unnest(everything()) %>%
                trim_predictions() %>%
                arrange(desc(.pred_yes))
        
}

# collect metrics from last fits
collect_last_fit_metrics = function(last_fits) {
        
        last_fits %>%
                unnest(last_fit) %>%
                select(wflow_id, .metrics) %>%
                unnest(.metrics) %>%
                arrange(.metric, .estimate)
        
}

# get game ids
add_game_ids = function(preds,
                        games) {
        
        preds %>%
                left_join(.,
                          games %>%
                                  mutate(.row = row_number()) %>%
                                  select(.row, game_id, name, yearpublished),
                          by = c(".row")) %>%
                trim_predictions()
}


# run ---------------------------------------------------------------------


# # set parallel backend
library(doParallel)
all_cores <- parallel::detectCores(logical = FALSE)
doMC::registerDoMC(cores = all_cores)

# get user and games
user_collection_and_games = 
        load_user_collection('mrbananagrabber') %>%
        join_bgg_games(games)

# create splits for testing
user_train_split =
        user_collection_and_games %>%
        make_user_split(end_train_year = 2020,
                              valid_window = 2)

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
                        models,
                cross = T
        )

# add tuning grids by model
user_wflows = 
        user_wflows %>%
        # add glmnet grids 
        option_add(id = user_wflows %>% 
                           filter(grepl("glmnet", wflow_id)) %>% 
                           pull(wflow_id),
                   grid = tuning_grids$glmnet) %>%
        # add cart grids
        option_add(id = user_wflows %>% 
                           filter(grepl("cart", wflow_id)) %>% 
                           pull(wflow_id),
                   grid = tuning_grids$cart) %>%
        # add lightgbm grids
        option_add(id = user_wflows %>% 
                           filter(grepl("cart", wflow_id)) %>% 
                           pull(wflow_id),
                   grid = tuning_grids$cart)


# select ids to tune
ids =  user_wflows %>% 
        filter(grepl("splines_glmnet|all_trees_lightgbm|all_trees_cart", wflow_id)) %>%
        pull(wflow_id)

message(paste("workflows:", "\n", paste(ids, collapse = "\n"), sep = ""), sep = "")
        
# tune user wflows
set.seed(1999)
user_wflows_results = 
        user_wflows %>%
        tune_user_wflows(ids = ids,
                         resamples = user_train_resamples,
                         metrics = prob_metrics,
                         control = ctrl_grid)

# collect predictions
training_predictions = 
        user_wflows_results %>%
        collect_predictions(select_best = T,
                            metric = 'mn_log_loss') %>%
        add_game_ids(games = analysis(user_train_split))

# collect metrics
training_metrics =
        user_wflows_results %>%
        rank_results(select_best = T,
                     rank_metric = 'mn_log_loss') %>%
        arrange(.metric, rank)

# fit on train and assess on valid
training_fits = 
        user_wflows_results %>%
        add_best_tune(metric = 'mn_log_loss') %>%
        add_last_fit(split = user_train_split)

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
        make_user_split(end_train_year = 2021,
                        valid_window = 4)

# finalize
user_final_fits = 
        user_wflows_results %>%
        add_best_tune(metric = 'mn_log_loss') %>%
        add_last_fit(split = user_final_split)

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
        list(
                "user_collection" = 
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