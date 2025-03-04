# authenticate to gcs with scopes needed
authenticate_to_gcs = function(
    scope = c(
        "https://www.googleapis.com/auth/devstorage.full_control",
        "https://www.googleapis.com/auth/cloud-platform"
    ),
    auth_file = Sys.getenv("GCS_AUTH_FILE")
) {
    googleAuthR::gar_auth_service(
        json_file = auth_file,
        scope = scope
    )
}

# function to retrieve games stored in gcp bucket
load_games = function(
    object_name = "raw/objects/games",
    bucket = "bgg_data",
    generation = NULL,
    ...
) {
    bggUtils::get_games_from_gcp(
        bucket = bucket,
        object_name = object_name,
        generation = generation
    )
}

# customizing publishers allow
publishers_allow = function() {
    c(25624, bggUtils:::publishers_allow_list())
}

# function to preprocess games
prepare_games = function(
    data,
    publisher_allow = publishers_allow(),
    families_allow = bggUtils:::families_allow_list(),
    families_remove = bggUtils:::families_remove_list(),
    ...
) {
    data |>
        # apply preprocessing via bggUtils function
        bggUtils::preprocess_bgg_games(
            publisher_allow = publisher_allow,
            families_allow = families_allow,
            families_remove = families_remove
        ) |>
        # remove games missing yearpublished
        filter(!is.na(yearpublished))
}

# add hurdle
add_hurdle = function(data, ratings = 25) {
    data |>
        mutate(
            hurdle = case_when(usersrated >= ratings ~ 'yes', TRUE ~ 'no'),
            hurdle = factor(hurdle, levels = c("no", "yes"))
        )
}
