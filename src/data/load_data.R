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
