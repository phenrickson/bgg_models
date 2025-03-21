longer_outcomes = function(data) {
    data |>
        pivot_longer(
            cols = c("usersrated", "averageweight", "average", "bayesaverage"),
            names_to = "outcome",
            values_to = "value"
        )
}

plot_outcomes_distributions = function(data) {
    data |>
        longer_outcomes() |>
        ggplot(aes(x = value)) +
        geom_histogram(bins = 80) +
        facet_wrap(outcome ~ ., ncol = 2, scales = "free") +
        theme_light()
}

plot_geek_vs_average = function(data, labels = T) {
    p =
        data |>
        ggplot(aes(
            x = average,
            label = name,
            color = bayesaverage,
            y = usersrated
        )) +
        geom_point(alpha = 0.3) +
        scale_y_log10(labels = scales::label_comma()) +
        scale_color_gradient2(
            high = "deepskyblue1",
            low = "red",
            mid = "grey80",
            limits = c(4, 8),
            oob = scales::squish,
            midpoint = 6
        ) +
        theme_light() +
        theme(
            legend.position = 'top',
            legend.title = element_text(hjust = 0.5),
            axis.text = element_text()
        ) +
        guides(
            color = guide_colorbar(
                barwidth = 12,
                barheight = 0.35,
                title = 'geek rating',
                title.position = 'top'
            )
        ) +
        xlab("average rating") +
        ylab("users rated (logged)")

    if (labels == T) {
        p +
            geom_text(size = 2, check_overlap = T, vjust = -1)
    } else {
        p
    }
}

filter_geek = function(data) {
    data |>
        filter(!is.na(bayesaverage))
}

filter_weight = function(data) {
    data |>
        filter(!is.na(averageweight))
}

log_ratings = function(data) {
    data |>
        mutate(usersrated = log(usersrated))
}

get_game_types = function(
    data,
    types = c(
        "wargames",
        "abstracts",
        "childrensgames",
        "familygames",
        "thematic",
        "strategygames",
        "partygames"
    )
) {
    unnest_game_types = function(data) {
        unnested =
            data |>
            select(game_id, ranks) |>
            unnest(ranks)
    }

    filter_game_types = function(
        data,
        types = c(
            "wargames",
            "abstracts",
            "childrensgames",
            "familygames",
            "strategygames",
            "thematic",
            "partygames"
        )
    ) {
        data |>
            inner_join(
                tibble(type = c("family"), name = types),
                by = c("type", "name")
            )
    }

    clean_game_types = function(data) {
        data |>
            mutate(
                name = gsub("games$", "", name),
                name = gsub("s$", "", name),
                name = case_when(name == 'war' ~ 'wargame', TRUE ~ name)
            )
    }

    data |>
        unnest_game_types() |>
        filter_game_types(types = types) |>
        clean_game_types() |>
        select(game_id, game_type = name)
}

add_game_types = function(data, raw_games) {
    data |>
        left_join(
            raw_games |>
                get_game_types(),
            by = c("game_id")
        ) |>
        select(game_id, game_type, name, yearpublished, everything()) |>
        mutate(game_type = replace_na(game_type, 'other'))
}

color_game_types = function() {
    c(
        "wargame" = "#E41A1C",
        "strategy" = "#377EB8",
        "thematic" = "#FFFF33",
        "abstract" = "#984EA3",
        "family" = "#4DAF4A",
        "party" = "#FF7F00",
        "children" = "#F781BF",
        "other" = "grey80"
    )
}

scale_color_game_types = function(values = color_game_types()) {
    scale_color_manual(values = values)
}

scale_fill_game_types = function(values = color_game_types()) {
    scale_fill_manual(values = values)
}

plot_outcomes_matrix = function(data, color = NULL) {
    data |>
        ggplot(aes(
            x = .panel_x,
            y = .panel_y,
            fill = {{ color }},
            color = {{ color }}
        )) +
        ggforce::geom_autopoint(alpha = 0.5, size = 0.5) +
        ggforce::geom_autodensity(alpha = 0.5, position = 'identity') +
        ggforce::facet_matrix(
            vars(c("averageweight", "average", "bayesaverage", "usersrated")),
            layer.diag = 2,
            grid.y.diag = F
        ) +
        theme_light() +
        theme(
            panel.grid.major = element_blank(),
            strip.text = element_text(size = 8)
        )
}

plot_outcomes_relationships = function(data, color = NULL) {
    data |>
        log_ratings() |>
        filter_geek() |>
        filter_weight() |>
        plot_outcomes_matrix(color = color)
}

plot_outcomes_by_type = function(data, color) {
    data |>
        log_ratings() |>
        filter_geek() |>
        filter_weight() |>
        plot_outcomes_matrix(color = {{ color }}) +
        scale_color_game_types() +
        scale_fill_game_types()
}

# split$data |>
#     add_game_types(games_raw) |>
#     filter(yearpublished >= 1980) |>
#     filter_geek() |>
#     group_by(game_type,
#              yearpublished) |>
#     count() |>
#     filter(game_type != 'other') |>
#     ggplot(aes(x=yearpublished, y=n, fill = game_type))+
#     geom_col()+
#     scale_fill_game_types()+
#     bggUtils::theme_bgg()+
#     facet_wrap(game_type ~.)+
#     guides(fill = F)
#
# split$data |>
#     plot_outcomes_relationships()
#
# split$data |>
#     add_game_types(games_raw) |>
#     filter(game_type != 'other') |>
#     filter(game_type != 'children') |>
#     plot_outcomes_by_type(color = game_type)
#
# split$data |>
#     sample_n(1000) |>
#     plot_outcomes_relationships()
#
#
# split$data |>
#     filter_geek() |>
#     log_ratings() |>
#     plot_geek_vs_average()
