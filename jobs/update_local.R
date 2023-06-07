# update local games data

# 1 load games from gcp and save to raw/processed
source(here::here("src", "data", "make_local_games_data.R"),
       echo = T,
       local = F)

