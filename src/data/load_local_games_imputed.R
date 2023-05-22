# load imputed games data

# load games with imputed averageweight data
games_imputed = pins::pin_read(board = board_folder(here::here("data", "processed")),
                               name = "games_imputed")