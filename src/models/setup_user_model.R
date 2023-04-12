# functions and setup for user model training

# gcs ---------------------------------------------------------------------

source(here::here("src", "data", "connect_to_gcs.R"))


# packages ----------------------------------------------------------------

suppressPackageStartupMessages({
        library(tidyverse)
        library(tidymodels)
        library(bggUtils)
        
})

# data --------------------------------------------------------------------

### games
# load games with imputed averageweight data
games_imputed = pins::pin_read(board = board_folder(here::here("data", "processed")),
                               name = "games_imputed")

### collection
# function to pull user collection from bgg and join it with all games
assemble_user_collection = function(username,
                                    games) {
        
        # pull collection and rename ts
        get_collection = function(username) {
                
                # load user collection data
                bggUtils::get_user_collection(username) %>%
                        rename(user_load_ts = load_ts)
                
        }
        
        # join up games with collection
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
        
        # pull collection
        collection = 
                get_collection(username)
        
        # 
        collection %>%
                join_games(.,
                           games) %>%
                prep_collection()
        
}


# splits and resamples -------------------------------------------------------------------


# function to split collection into training and validation set
split_collection = function(data,
                            end_train_year,
                            valid_window,
                            min_users,
                            filter = F) {
        
        train_collection = 
                data %>%
                filter(usersrated >=min_users) %>%
                filter(yearpublished <= end_train_year)
        
        
        if (filter == T) {
                valid_collection = 
                        data %>%
                        filter(usersrated >=min_users) %>%
                        filter(yearpublished > end_train_year & yearpublished <= end_train_year + valid_window)
        } else {
                valid_collection = 
                        data %>%
                        filter(yearpublished > end_train_year & yearpublished <= end_train_year + valid_window)
                
        }
      
        return(list("train" = train_collection,
                    "valid" = valid_collection))
        
}

# creates manual rsample split given previously created split object
split_rsample = function(split) {
        
        make_splits(
                list(analysis =
                             seq(nrow(split$train)),
                     assessment =
                             nrow(split$train) + seq(nrow(split$valid))),
                bind_rows(split$train,
                          split$valid)
        )
        
}

# setup for user models
make_user_split_and_resamples = function(collection,
                                         games,
                                         end_train_year,
                                         valid_window = 2,
                                         outcome,
                                         min_users) {
        
        # split collection
        user_split = 
                split_collection(collection,
                                 end_train_year,
                                 valid_window = 2,
                                 min_users = min_users)
        
        # make split with rsample
        user_rsample_split = 
                split_rsample(user_split)
        
        # create resamples
        set.seed(1999)
        user_train_resamples = 
                vfold_cv(user_split$train,
                         v = 5,
                         strata = all_of(outcome))
        
        return(list("user_collection" = user_collection,
                    "user_split" = user_split,
                    "user_rsample_split" = user_rsample_split,
                    "user_train_resamples" = user_train_resamples,
                    "outcome" = outcome))
        
}


# recipes -----------------------------------------------------------------


# load user recipes
source(here::here("src", "features", "recipes_user.R"))

# model specifications ----------------------------------------------------

# get model specs
source(here::here("src", "models", "class_models.R"))


# tuning control --------------------------------------------------------


# tuning control for tune grid
ctrl_grid = 
        control_grid(
                save_pred = TRUE,
                allow_par = T,
                parallel_over = "everything",
                verbose = TRUE,
                save_workflow = T,
                event_level = 'second'
        )

# control for racing
ctrl_race = 
        finetune::control_race(
                save_pred = TRUE,
                parallel_over = "everything",
                allow_par = T,
                verbose = TRUE,
                verbose_elim = TRUE,
                save_workflow = F,
                event_level = 'second'
        )

# control for last_fit
ctrl_last_fit = 
        control_last_fit(verbose = T,
                         event_level = 'second',
                         allow_par = T)


# metrics -----------------------------------------------------------------


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



# results -----------------------------------------------------------------

# select only necessary from results
trim_results = function(res) {
        
        res %>%
                select(wflow_id, .row, !!outcome, .pred_yes, any_of(c("game_id", "name", "yearpublished")))
        
}

# check that results finished
check_results = function(res) {
        
        res %>%
                mutate(check = purrr::map_lgl(result, ~identical(.x, list()))) %>%
                filter(check == F) %>%
                select(-check)
}

# finalize fits
finalize_fits = function(res,
                         metric) {
        
        res %>%
                mutate(best_tune = 
                               map(result,
                                   ~ .x %>% 
                                           select_best(metric = metric))) %>%
                mutate(last_fit = map2(result,
                                       best_tune,
                                       ~ .x %>%
                                               extract_workflow() %>%
                                               finalize_workflow(parameters = .y) %>%
                                               last_fit(user_train_setup$user_rsample_split,
                                                        metrics = prob_metrics,
                                                        control = ctrl_last_fit)))
}

# get individual predictions with ids from last fits
get_predictions = function(last_fits) {
        
        last_fits %>%
                select(wflow_id, last_fit) %>% 
                unnest(last_fit) %>%
                select(wflow_id, splits, .predictions) %>% 
                mutate(assess = map(splits,~ .x %>% 
                                            assessment %>% 
                                            select(game_id, name, yearpublished))) %>% 
                select(-splits) %>% 
                unnest(everything()) %>%
                trim_results() %>%
                arrange(desc(.pred_yes))
        
}

# get metrics from last fits
get_metrics = function(last_fits) {
        
        last_fits %>%
                select(wflow_id, last_fit) %>%
                mutate(metrics = map(last_fit,
                                     collect_metrics)) %>%
                select(wflow_id, metrics) %>%
                unnest(metrics) %>%
                arrange(.metric, .estimate)
        
}


# bundle ------------------------------------------------------------------






