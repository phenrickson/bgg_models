preprocess_games = function(data,
                                publisher_allow = publisher_allow_list(),
                                families_allow = families_allow_list(),
                                families_remove = families_remove_list()
) {
    
    keep_links = paste0("boardgame", c("category", "mechanic", "designer", "artist", "publisher"))
    
    # outcomes =
    outcomes = data %>%
        select(game_id, statistics) %>%
        unnest(c(statistics), keep_empty = T) %>%
        select(game_id, averageweight, usersrated, average, bayesaverage, numweights) %>%
        mutate(across(
            c(averageweight,
              usersrated,
              average,
              bayesaverage),
            ~ ifelse(. == 0, NA, .))
        )
    
    # get info needed from games
    info = data %>%
        select(game_id, info) %>%
        unnest(c(info), keep_empty = T) %>%
        mutate(across(
            c(yearpublished,
              minplayers,
              maxplayers,
              playingtime,
              minplaytime,
              maxplaytime,
              minage),
            ~ ifelse(. == 0, NA, .))
        )
    
    # get primary name
    names =
        data %>%
        select(game_id, names) %>%
        unnest(c(names), keep_empty = T) %>%
        filter(type == 'primary') %>%
        select(game_id,
               name = value)
    
    # extract links
    links = data %>%
        select(game_id, links) %>%
        unnest(c(links), keep_empty = T) %>%
        filter(type %in% keep_links) %>%
        mutate(value = clean_bgg_text(value))
    
    # get categories
    categories =
        links %>%
        filter(type == 'boardgamecategory') %>%
        select(game_id, value) %>%
        collapse_categorical(name = categories)
    
    # publishers
    publishers =
        links %>%
        filter(type == 'boardgamepublisher') %>%
        filter(id %in% publisher_allow) %>%
        select(game_id, value) %>%
        collapse_categorical(name = publishers)
    
    # mechanics
    mechanics =
        links %>%
        filter(type == 'boardgamemechanic') %>%
        select(game_id, value) %>%
        collapse_categorical(name = mechanics)
    
    # artists
    artists =
        links %>%
        filter(type == 'boardgameartist') %>%
        select(game_id, value) %>%
        collapse_categorical(name = artists)
    
    # designers
    designers =
        links %>%
        filter(type == 'boardgamedesigner') %>%
        select(game_id, value) %>%
        collapse_categorical(name = designers)
    
    # now extract bgg families
    # general families
    families =
        data %>%
        select(game_id, links) %>%
        unnest(c(links), keep_empty = T) %>%
        filter(type == 'boardgamefamily') %>%
        filter(!grepl(families_remove, value)) %>%
        filter(grepl(families_allow, value)) %>%
        mutate(value = clean_bgg_text(value)) %>%
        collapse_categorical(name = families)
    
    # themes
    themes =
        data %>%
        select(game_id, links) %>%
        unnest(c(links), keep_empty = T) %>%
        filter(type %in% 'boardgamefamily') %>%
        filter(grepl("^Theme:", value)) %>%
        mutate(value = gsub("^Theme: ", "", value)) %>%
        mutate(value = clean_bgg_text(value)) %>%
        select(game_id, value) %>%
        collapse_categorical(name = themes)
    
    # specific families
    components =
        data %>%
        select(game_id, links) %>%
        unnest(c(links), keep_empty = T) %>%
        filter(type %in% 'boardgamefamily') %>%
        filter(grepl("^Components:", value)) %>%
        mutate(value = gsub("^Components: ", "", value)) %>%
        mutate(value = clean_bgg_text(value)) %>%
        collapse_categorical(., name = components)
    
    # mechanisms
    mechanisms =
        data %>%
        select(game_id, links) %>%
        unnest(c(links), keep_empty = T) %>%
        filter(type %in% 'boardgamefamily') %>%
        filter(grepl("^Mechanism:", value)) %>%
        mutate(value = gsub("^Mechanism: ", "", value)) %>%
        mutate(value = clean_bgg_text(value)) %>%
        select(game_id, value) %>%
        collapse_categorical(name = mechanisms)
    
    outcomes %>%
        left_join(.,
                  info,
                  by = join_by(game_id)) %>%
        left_join(.,
                  names,
                  by = join_by(game_id)) %>%
        left_join(.,
                  categories,
                  by = join_by(game_id)) %>%
        left_join(.,
                  mechanics,
                  by = join_by(game_id)) %>%
        left_join(.,
                  publishers,
                  by = join_by(game_id)) %>%
        left_join(.,
                  designers,
                  by = join_by(game_id)) %>%
        left_join(.,
                  artists,
                  by = join_by(game_id)) %>%
        left_join(.,
                  families,
                  by = join_by(game_id)) %>%
        left_join(.,
                  mechanisms,
                  by = join_by(game_id)) %>%
        left_join(.,
                  components,
                  by = join_by(game_id)) %>%
        left_join(.,
                  themes,
                  by = join_by(game_id)) %>%
        select(game_id, name, yearpublished, everything())
    
}

publisher_allow_list = function() {
    
    # list of ids allowed to enter model for publisher
    c(
        51 # Hasbo
        ,10 # Mayfair Games
        ,102 # Decision Games
        ,196 # Multi-Man Publishing
        ,396 # Alderac Entertainment Group aka AEG
        ,1027 # Days of Wonder
        ,21847 # Pandasaurus Games
        ,1001 # (web published)
        ,4 # (Self-Published)
        ,140 # Splotter Spellen
        ,157 # Asmodee
        ,34 # Ravensburger
        ,28 # Parker Brothers
        ,39 # Pegasus Speile
        ,37 # KOSMOS
        ,20 # Milton Bradley
        ,3 # Rio Grande Games
        ,538 # Z-Man Games
        ,52 # GMT Games
        # ,8923 # IELLO
        ,17 # Fantasy Flight Games
        ,5 # Avalon Hill
        ,3320 # (Unknown)
        ,597 # Eagle-Gryphon Games
        ,5400 # Matagot
        ,26 # Games Workshop Ltd
        ,47 # Queen Games
        ,11652 # Stronghold Games
        ,19 # Steve Jackson Games
        ,13 # Wizards of the Coast
        ,12024 # Cryptozoic Entertainment
        ,10754 # Plaid Hat Games
        ,21608 # CMON Global Limited
        ,108 # Gamewright
        ,221 # WizKids
        ,171 # (Public Domain)
        ,93 # Mattel, Inc
        ,25842 # Space Cowboys
        ,23202 # Stonemaier
        ,34188 # Plan  B
        ,30958 # Capstone Games
        ,22593 # Chip Theory Games
        ,17917 # Ares Games
        ,17543 # Greater Than Games
        ,28072 # Renegade Games
        ,34846 # Restoration Games
        ,29313 # Osprey Games
        ,21765 # Roxley
        ,7345 # Czech Games Edition
        ,29412 # Awaken Realms
        ,3929 # Compass Games
        ,26991 # Button Shy
        ,2456 # The Game Crafter
        ,12 # Cheapass Games
        ,9 # alea
        ,2164 # NorthStar Game Studio
        ,5774 # Bézier Games
        ,18617 #Red Raven Games
        ,102 #Decision Games (I)
        , 489# 3W (World Wide Wargames)
    )
}

families_remove_list = function() {
    
    c("^Admin:",
      "^Misc:",
      "^Promotional:",
      "^Digital Implementations: ",
      "^Components: Game Trayz Inside",
      "^Crowdfunding: Spieleschmiede",
      "^Crowdfunding: Verkami",
      "^Crowdfunding: Indiegogo",
      "^Contests:",
      "^Game:",
      "^Players:",
      "^Players: Games with expansions",
      "^Crowdfunding:") %>%
        paste(., collapse = "|")
}

families_allow_list = function() {
    
    c("^Series: Monopoly-Like",
      "^Series: 18xx",
      "^Series: Cards Against Humanity-Like",
      "^Series: Exit: The Game",
      "^Country:",
      "^Animals",
      "^History",
      "^Sports",
      "^Category",
      "^Cities",
      "^Traditional",
      "^Creatures",
      "^TV",
      "^Region",
      "^Card",
      "^Comic",
      "^Ancient",
      "^Brands",
      "^Versions & Editions",
      "^Food",
      "^Movies",
      "^Setting",
      "^Card Games",
      "^Collectible",
      "^Containers",
      "^Authors",
      "^Characters",
      "^Religious",
      "^Holidays",
      "^Space",
      "^Folk",
      "^Word",
      "^Mythology",
      "^Occupation",
      "^Celebrities",
      "^Toys") %>%
        paste(., collapse = "|")
    
}

collapse_categorical = function(data, name) {
    
    data %>%
        group_by(game_id) %>%
        summarize({{name}} := paste(value, collapse = ", "),
                  .groups = 'drop')
    
}

