# what: make list of bgg families to be removed before modeling (due to issues of leakage)

# tidyverse package
library(tidyverse)

# load game families data from games nested
load(here::here("data", "processed", "games_nested.Rdata"))

# select game families
game_families = 
        games_nested %>%
        select(families) %>%
        unnest(families) %>%
        distinct(family_value, id, type, family_value, value)

# specific families being removed
families_filter_table = 
        bind_rows(
                # better description needed 
                game_families %>%
                        filter(grepl("Admin Better Description", value)),
                # digital versions (leakage)
                game_families %>%
                        filter(grepl("Digital Implementations", value)),
                # misc values (leakage)
                game_families %>%
                        filter(grepl("Misc", value)),
                # promotional (leakage)
                game_families %>%
                        filter(grepl("Promotional Implementations", value)),
                # upcoming
                game_families %>%
                        filter(grepl("Upcoming", value)),
                # unreleased
                game_families %>%
                        filter(grepl("Unreleased", value)),
                # components typically added after release
                game_families %>%
                        filter(grepl("Components Game Trayzinside", value)),
                # specifc type of crowdfunding
                game_families %>%
                        filter(grepl("Spieleschmiede|Verkami|Indiegogo", value))
        )
        
# pull values
families_filter_names = 
        families_filter_table %>%
        pull(value)

# pin to gcs
source(here::here("src", "data", "connect_to_gcs.R"))

# board on gcs
data_board =
        pins::board_gcs(
                bucket = my_bucket,
                prefix = "data/",
                versioned = T)

# write
data_board %>%
        pin_write(families_filter_names,
                  name = 'families_filter_names',
                  description = 'names of bgg family names to be removed from modeling')


