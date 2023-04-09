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
                            min_users) {
        
        train_collection = 
                data %>%
                filter(usersrated >=min_users) %>%
                filter(yearpublished <= end_train_year)
        
        valid_collection = 
                data %>%
                filter(usersrated >=min_users) %>%
                filter(yearpublished > end_train_year & yearpublished <= end_train_year + valid_window)
        
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
                                 2019,
                                 valid_window = 2,
                                 min_users = min_users)
        
        # make valid split
        valid_split = 
                split_rsample(user_split)
        
        # create resamples
        set.seed(1999)
        user_train_resamples = 
                vfold_cv(user_split$train,
                         v = 5,
                         strata = all_of(outcome))
        
        return(list("user_collection" = user_collection,
                    "user_split" = user_split,
                    "valid_split" = valid_split,
                    "user_train_resamples" = user_train_resamples,
                    "outcome" = outcome))
        
}

# recipes -----------------------------------------------------------------

# load recipes
source(here::here("src", "features", "recipes_user.R"))

# create standard recipes for users
make_user_recipes = function(data,
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
                # dummy extract categories
                dummy_extract_variable(c(categories),
                                       threshold = 1) %>%
                # dummy extract mechanics
                dummy_extract_variable(c(mechanics, categories),
                                       threshold = 50) %>%
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
        
        recipes = list("minimal_impute" = base_impute_recipe,
                       "minimal_trees" = base_trees_recipe,
                       "all_impute" = all_impute_recipe,
                       "all_impute_splines" = all_impute_splines_recipe,
                       "all_trees" = all_trees_recipe)
        
        return(recipes)
        
}

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
get_trim_results = function(res) {
        
        res %>%
                select(wflow_id, .row, !!outcome, .pred_yes)
        
}

# register parallel ----------------------------------------------------------------

# set parallel
library(doParallel)
all_cores <- parallel::detectCores(logical = FALSE)
doMC::registerDoMC(cores = all_cores)

