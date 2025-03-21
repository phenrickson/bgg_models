# functions for add linkjing
make_bgg_link = function(game_id) {
    paste0("https://boardgamegeek.com/boardgame/", game_id)
}

make_hyperlink = function(myurl, mytext = myurl) {
    paste('<a href="', myurl, '">', mytext, '</a>')
}

make_web_image =
    function(x, height = 100) {
        web_image(url = x, height = px(height))
    }

make_image_link = function(link, height = 52) {
    paste0(
        '<img src =',
        link,
        ' height=',
        paste(height, sep = ""),
        '>',
        '</img>'
    )
}

prep_predictions_dt = function(predictions, games) {
    predictions |>
        arrange(desc(.pred_bayesaverage)) |>
        mutate(
            across(
                c(starts_with(".pred"), -starts_with(".pred_hurdle_class")),
                ~ round(.x, 2)
            )
        ) |>
        mutate(
            name = make_hyperlink(
                make_bgg_link(game_id),
                mytext = paste(name, paste0("(", yearpublished, ")"))
            )
        ) |>
        mutate(
            Rank = row_number(),
            Image = make_image_link(thumbnail),
            Game = name,
            `Average Weight` = .pred_averageweight,
            `Average Rating` = .pred_average,
            `Users Rated` = .pred_usersrated,
            `Geek Rating` = .pred_bayesaverage,
            .keep = 'none'
        )
}

predictions_dt = function(
    predictions,
    games,
    pageLength = 10,
    lazy_load = TRUE
) {
    cols = c(
        "Rank",
        "Image",
        "Average Weight",
        "Average Rating",
        "Users Rated",
        "Geek Rating"
    )

    # Prepare data
    prepared_data <- prep_predictions_dt(predictions, games = games)

    # Create options list with base settings
    dt_options <- list(
        pageLength = pageLength,
        deferRender = TRUE,
        paging = TRUE,
        autoWidth = TRUE,
        scrollX = TRUE,
        scrollCollapse = FALSE,
        initComplete = htmlwidgets::JS(
            "function(settings, json) {",
            paste0(
                "$(this.api().table().container()).css({'font-size': '",
                '10pt',
                "'});",
                "$(this.api().table().container()).css({'width': '100%'});"
            ),
            "}"
        ),
        columnDefs = list(
            list(className = 'dt-center', visible = TRUE, targets = cols),
            list(
                targets = "Game",
                width = "200px",
                render = htmlwidgets::JS(
                    "function(data, type, row) {",
                    "  if (type === 'display') {",
                    "    return '<div style=\"width:200px; word-wrap:break-word;\">' + data + '</div>';",
                    "  }",
                    "  return data;",
                    "}"
                )
            )
        )
    )

    # Add lazy loading if enabled
    if (lazy_load) {
        # Create a unique ID for this table
        table_id <- paste0("dt_", sample(1000:9999, 1))

        # Add a wrapper div with the ID
        html_wrapper <- htmltools::tags$div(
            id = table_id,
            style = "visibility: hidden;",
            htmltools::tags$script(
                htmltools::HTML(
                    paste0(
                        "document.addEventListener('DOMContentLoaded', function() {",
                        "  var observer = new IntersectionObserver(function(entries) {",
                        "    if (entries[0].isIntersecting) {",
                        "      document.getElementById('",
                        table_id,
                        "').style.visibility = 'visible';",
                        "      observer.disconnect();",
                        "    }",
                        "  }, { rootMargin: '200px' });",
                        "  observer.observe(document.getElementById('",
                        table_id,
                        "'));",
                        "});"
                    )
                )
            )
        )

        # Create the datatable and wrap it
        dt <- prepared_data |>
            DT::datatable(
                escape = FALSE,
                rownames = FALSE,
                extensions = c('Responsive', 'Scroller'),
                class = list(stripe = FALSE),
                filter = list(position = 'top'),
                options = dt_options
            )

        # Return the wrapped datatable
        htmltools::tagList(html_wrapper, dt)
    } else {
        # Create the datatable without wrapper
        prepared_data |>
            DT::datatable(
                escape = FALSE,
                rownames = FALSE,
                extensions = c('Responsive', 'Scroller'),
                class = list(stripe = FALSE),
                filter = list(position = 'top'),
                options = dt_options
            )
    }
}

add_dt_colors = function(
    dt,
    seq,
    low_color = 'white',
    high_color = 'dodgerblue2',
    mid_color = NULL,
    column = 'Geek Rating'
) {
    cuts = seq
    my_color_ramp = colorRampPalette(c(low_color, mid_color, high_color))
    max_color = my_color_ramp(length(cuts) - 5)[length(cuts) - 5]

    dt |>
        DT::formatStyle(
            columns = column,
            backgroundColor = DT::styleInterval(
                cuts = cuts,
                values = my_color_ramp(length(cuts) + 1)
            )
        )
}

add_colors = function(dt) {
    # Check if dt is a tagList (from lazy loading)
    if (inherits(dt, "shiny.tag.list")) {
        # Extract the datatable from the tagList
        # The datatable is the second element in the tagList
        datatable_index <- which(sapply(
            dt,
            function(x) inherits(x, "datatables")
        ))
        if (length(datatable_index) > 0) {
            # Apply colors to the datatable
            colored_dt <- dt[[datatable_index]] |>
                add_dt_colors(column = 'Geek Rating', seq = seq(5, 9, 0.1)) |>
                add_dt_colors(
                    column = 'Average Rating',
                    seq = seq(6, 10, 0.1)
                ) |>
                add_dt_colors(
                    column = 'Users Rated',
                    seq = c(
                        0,
                        100,
                        500,
                        seq(1000, 10000, 1000),
                        15000,
                        25000,
                        50000
                    )
                ) |>
                add_dt_colors(
                    column = 'Average Weight',
                    low_color = 'deepskyblue1',
                    mid_color = 'white',
                    high_color = 'orange',
                    seq = seq(0.8, 5, 0.1)
                )

            # Replace the datatable in the tagList with the colored one
            dt[[datatable_index]] <- colored_dt
            return(dt)
        }
    }

    # If dt is a regular datatable or we couldn't find a datatable in the tagList
    dt |>
        add_dt_colors(column = 'Geek Rating', seq = seq(5, 9, 0.1)) |>
        add_dt_colors(column = 'Average Rating', seq = seq(6, 10, 0.1)) |>
        add_dt_colors(
            column = 'Users Rated',
            seq = c(0, 100, 500, seq(1000, 10000, 1000), 15000, 25000, 50000)
        ) |>
        add_dt_colors(
            column = 'Average Weight',
            low_color = 'deepskyblue1',
            mid_color = 'white',
            high_color = 'orange',
            seq = seq(0.8, 5, 0.1)
        )
}

hurdle_dt = function(data, lazy_load = TRUE) {
    # Prepare data
    prepared_data <- data |>
        arrange(desc(.pred_hurdle_yes)) |>
        filter(!is.na(thumbnail)) |>
        mutate(
            name = make_hyperlink(
                make_bgg_link(game_id),
                mytext = paste(name, paste0("(", yearpublished, ")"))
            )
        ) |>
        mutate(
            Image = make_image_link(thumbnail),
            Game = name,
            Description = stringr::str_trunc(description, width = 150),
            `Pr(Hurdle)` = round(.pred_hurdle_yes, 3),
            `Ratings` = usersrated,
            .keep = 'none'
        )

    # Create options list with base settings
    dt_options <- list(
        pageLength = 10,
        deferRender = TRUE,
        paging = TRUE,
        autoWidth = TRUE,
        scrollX = TRUE,
        scrollCollapse = FALSE,
        initComplete = htmlwidgets::JS(
            "function(settings, json) {",
            paste0(
                "$(this.api().table().container()).css({'font-size': '",
                '10pt',
                "'});",
                "$(this.api().table().container()).css({'width': '100%'});"
            ),
            "}"
        ),
        columnDefs = list(
            list(
                className = 'dt-center',
                visible = TRUE,
                targets = c("Image", "Pr(Hurdle)", "Ratings")
            ),
            list(
                targets = "Game",
                width = "200px",
                render = htmlwidgets::JS(
                    "function(data, type, row) {",
                    "  if (type === 'display') {",
                    "    return '<div style=\"width:200px; word-wrap:break-word;\">' + data + '</div>';",
                    "  }",
                    "  return data;",
                    "}"
                )
            ),
            list(
                targets = "Description",
                width = "200px",
                render = htmlwidgets::JS(
                    "function(data, type, row) {",
                    "  if (type === 'display') {",
                    "    return '<div style=\"width:200px; word-wrap:break-word;\">' + data + '</div>';",
                    "  }",
                    "  return data;",
                    "}"
                )
            )
        )
    )

    # Add lazy loading if enabled
    if (lazy_load) {
        # Create a unique ID for this table
        table_id <- paste0("dt_", sample(1000:9999, 1))

        # Add a wrapper div with the ID
        html_wrapper <- htmltools::tags$div(
            id = table_id,
            style = "visibility: hidden;",
            htmltools::tags$script(
                htmltools::HTML(
                    paste0(
                        "document.addEventListener('DOMContentLoaded', function() {",
                        "  var observer = new IntersectionObserver(function(entries) {",
                        "    if (entries[0].isIntersecting) {",
                        "      document.getElementById('",
                        table_id,
                        "').style.visibility = 'visible';",
                        "      observer.disconnect();",
                        "    }",
                        "  }, { rootMargin: '200px' });",
                        "  observer.observe(document.getElementById('",
                        table_id,
                        "'));",
                        "});"
                    )
                )
            )
        )

        # Create the datatable and wrap it
        dt <- prepared_data |>
            DT::datatable(
                escape = FALSE,
                rownames = FALSE,
                extensions = c('Responsive', 'Scroller'),
                class = list(stripe = FALSE),
                filter = list(position = 'top'),
                options = dt_options
            )

        # Return the wrapped datatable
        htmltools::tagList(html_wrapper, dt)
    } else {
        # Create the datatable without wrapper
        prepared_data |>
            DT::datatable(
                escape = FALSE,
                rownames = FALSE,
                extensions = c('Responsive', 'Scroller'),
                class = list(stripe = FALSE),
                filter = list(position = 'top'),
                options = dt_options
            )
    }
}

# Prepare data for gt table
prep_predictions_gt = function(predictions, games, max_rows = 50) {
    result <- predictions |>
        arrange(desc(.pred_bayesaverage))

    result |>
        head(max_rows) |> # Limit to specified number of rows
        mutate(
            across(
                c(starts_with(".pred"), -starts_with(".pred_hurdle_class")),
                ~ round(.x, 2)
            )
        ) |>
        mutate(
            name = make_hyperlink(
                make_bgg_link(game_id),
                mytext = paste(name, paste0("(", yearpublished, ")"))
            )
        ) |>
        mutate(
            Rank = row_number(),
            Image = thumbnail, # Store just the URL, not the HTML
            Game = name,
            `Average Weight` = .pred_averageweight,
            `Average Rating` = .pred_average,
            `Users Rated` = .pred_usersrated,
            `Geek Rating` = .pred_bayesaverage,
            .keep = 'none'
        )
}

# Create a non-interactive gt table
predictions_gt = function(
    predictions,
    games,
    max_rows = 50
) {
    # Columns to include
    cols = c(
        "Rank",
        "Image",
        "Game",
        "Average Weight",
        "Average Rating",
        "Users Rated",
        "Geek Rating"
    )

    # Prepare data
    prepared_data <- prep_predictions_gt(
        predictions,
        games = games,
        max_rows = max_rows
    )

    # Create gt table
    gt_table <- prepared_data |>
        gt::gt() |>
        # Set table options
        gt::tab_options(
            table.width = gt::pct(100),
            column_labels.font.weight = "bold",
            data_row.padding = gt::px(2)
        ) |>
        # Format columns
        gt::fmt_markdown(columns = c("Game")) |> # Remove Image from fmt_markdown
        gt::cols_align(align = "center", columns = -c("Game")) |>
        # Set equal column widths
        gt::cols_width(
            Rank ~ gt::px(60),
            Image ~ gt::px(100),
            Game ~ gt::px(150),
            `Average Weight` ~ gt::px(75),
            `Average Rating` ~ gt::px(75),
            `Users Rated` ~ gt::px(75),
            `Geek Rating` ~ gt::px(75)
        )

    # Add color scales
    gt_table <- gt_table |>
        # Geek Rating color scale
        gt::data_color(
            columns = "Geek Rating",
            colors = scales::col_numeric(
                palette = c("white", "dodgerblue2"),
                domain = c(5, 9)
            )
        ) |>
        # Average Rating color scale
        gt::data_color(
            columns = "Average Rating",
            colors = scales::col_numeric(
                palette = c("white", "dodgerblue2"),
                domain = c(6, 10)
            )
        ) |>
        # Users Rated color scale
        gt::data_color(
            columns = "Users Rated",
            colors = scales::col_numeric(
                palette = c("white", "dodgerblue2"),
                domain = c(0, 50000)
            )
        ) |>
        # Average Weight color scale
        gt::data_color(
            columns = "Average Weight",
            colors = scales::col_numeric(
                palette = c("deepskyblue1", "white", "orange"),
                domain = c(0.8, 3, 5)
            )
        ) |>
        gt::text_transform(
            locations = gt::cells_body(columns = c("Image")),
            fn = function(x) {
                gt::web_image(
                    url = x, # Now x is just the URL, not HTML
                    height = 50
                )
            }
        )

    return(gt_table)
}

# Function for hurdle model table
hurdle_gt = function(data, max_rows = 50) {
    # Check if thumbnail exists in the data
    has_thumbnail <- "thumbnail" %in% names(data)

    # Prepare data
    result <- data |>
        arrange(desc(.pred_hurdle_yes))

    # Only filter by thumbnail if it exists
    if (has_thumbnail) {
        result <- result |>
            filter(!is.na(thumbnail))
    }

    prepared_data <- result |>
        head(max_rows) |> # Limit to specified number of rows
        mutate(
            name = make_hyperlink(
                make_bgg_link(game_id),
                mytext = paste(name, paste0("(", yearpublished, ")"))
            )
        ) |>
        mutate(
            Image = thumbnail, # Store just the URL, not the HTML
            Game = name,
            Description = stringr::str_trunc(description, width = 150),
            `Pr(Hurdle)` = round(.pred_hurdle_yes, 3),
            `Ratings` = usersrated,
            .keep = 'none'
        )

    # Create gt table
    gt_table <- prepared_data |>
        gt::gt() |>
        # Set table options
        gt::tab_options(
            table.width = gt::pct(100),
            table.font.size = gt::px(10),
            column_labels.font.weight = "bold",
            data_row.padding = gt::px(2)
        ) |>
        # Format columns
        gt::fmt_markdown(columns = c("Game", "Description")) |> # Remove Image from fmt_markdown
        gt::cols_align(
            align = "center",
            columns = c("Image", "Pr(Hurdle)", "Ratings")
        ) |>
        gt::cols_width(
            Image ~ gt::px(100),
            Game ~ gt::px(200),
            Description ~ gt::px(200),
            `Pr(Hurdle)` ~ gt::px(100),
            `Ratings` ~ gt::px(100)
        ) |>
        # Add title
        gt::tab_header(
            title = "Hurdle Model Predictions"
        )

    # Add color scale for Pr(Hurdle)
    gt_table <- gt_table |>
        gt::data_color(
            columns = "Pr(Hurdle)",
            colors = scales::col_numeric(
                palette = c("white", "dodgerblue2"),
                domain = c(0, 1)
            )
        ) |>
        gt::text_transform(
            locations = gt::cells_body(columns = c("Image")),
            fn = function(x) {
                gt::web_image(
                    url = x, # Now x is just the URL, not HTML
                    height = 50
                )
            }
        )

    return(gt_table)
}
