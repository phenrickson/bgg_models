load_games = function(object_name = "raw/objects/games",
                      generation = "1708980495752949",
                      bucket = "bgg_data",
                      ...) {
    
    bggUtils::get_games_from_gcp(
        bucket = bucket,
        object_name = object_name,
        generation = generation)
    
}