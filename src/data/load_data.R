# function to retrieve games stored in gcp bucket
load_games = function(object_name = "raw/objects/games",
                      bucket = "bgg_data",
                      generation = NULL,
                      ...) {
    
    bggUtils::get_games_from_gcp(
        bucket = bucket,
        object_name = object_name,
        generation = generation)
    
}

# function to preprocess games
prepare_games = function(data) {
    data |>
        # apply preprocessing via bggUtils function
        bggUtils::preprocess_bgg_games() |>
        # remove games missing yearpublished
        filter(!is.na(yearpublished))
}

# add hurdle
add_hurdle = function(data, ratings = 25) {
    
    data |>
        mutate(hurdle = case_when(usersrated >= ratings ~ 'yes',
                                  TRUE ~ 'no'),
               hurdle = factor(hurdle, levels = c("no", "yes")))
    
}
