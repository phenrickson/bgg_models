# load games data

# connect to gcs 
source(here::here("src", "data", "connect_to_gcs.R"))


# packages ----------------------------------------------------------------

library(tidyverse)
library(tidymodels)
library(bggUtils)


# data --------------------------------------------------------------------


# load games with imputed averageweight data
games_imputed = pins::pin_read(board = board_folder(here::here("data", "processed")),
                               name = "games_imputed")


# get user collection ---------------------------------------------------


# load user collection data
collection = bggUtils::get_user_collection('GOBBluth89') %>%
        rename(user_laod_ts = load_ts)



# model user collection ---------------------------------------------------


# join
games_collection = 
        games_imputed %>%
        left_join(.,
                  collection,
                  by = c("game_id", "name")
        )


# recipes -----------------------------------------------------------------


# load recipes
source(here::here("src", "features", "recipes_user.R"))



# process

# estimate averageweight

# load user data
