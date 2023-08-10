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

# run ---------------------------------------------------------------------
# 
# run for one username
user_output = 
        'GOBBluth89' %>%
        # load user collection from bgg api
        load_user_collection(username = .) %>%
        # train model for user
        train_user_model(user_collection = .,
                         bgg_games = games,
                         outcome = 'own',
                         end_train_year = 2021,
                         valid_window = 2,
                         retrain_window = 0,
                         model_specs = c('glmnet','lightgbm'),
                         tune_metric = 'mn_log_loss')

user_output %>%
        # build markdown report
        build_user_report(user_output = .)

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
