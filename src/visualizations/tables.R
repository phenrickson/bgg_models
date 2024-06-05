# functions for add linkjing
make_bgg_link = function(game_id) {
    
    paste0("https://boardgamegeek.com/boardgame/",
           game_id)
}

make_hyperlink = function(myurl,
                          mytext=myurl) {
    paste('<a href="',myurl,'">',mytext,'</a>')
}

make_web_image =
    function(x, height=100) {
        
        web_image(url = x,
                  height = px(height) 
        ) 
        
    }

make_image_link = function(link,
                           height = 52) {
    
    paste0('<img src =',
           link,
           ' height=',
           paste(height, sep=""),
           '>',
           '</img>')
    
}

prep_predictions_dt = function(predictions,
                               games) {
    
    predictions |>
        arrange(desc(.pred_bayesaverage)) |>
        filter(!is.na(thumbnail)) |>
        mutate(across(starts_with(".pred"), ~ round(.x, 2))) |>
        mutate(name = make_hyperlink(make_bgg_link(game_id), 
                                     mytext = paste(name, paste0("(",yearpublished, ")")))) |>
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

predictions_dt = function(predictions,
                          games,
                          pageLength = 10) {
    
    cols =  c("Rank",
              "Image",
              "Average Weight",
              "Average Rating",
              "Users Rated",
              "Geek Rating")
    
    predictions |>
        prep_predictions_dt(games = games) |>
        DT::datatable(escape=F,
                      rownames = F,
                      extensions = c('Responsive'),
                      #  caption = "Games",
                      class = list(stripe =F),
                      filter = list(position = 'top'),
                      options = list(pageLength = pageLength,
                                     initComplete = htmlwidgets::JS(
                                         "function(settings, json) {",
                                         paste0("$(this.api().table().container()).css({'font-size': '", '10pt', "'});"),
                                         "}"),
                                     scrollX=F,
                                     columnDefs = list(
                                         list(className = 'dt-center',
                                              visible=T,
                                              targets = cols
                                         )
                                     )
                      )
        )
}

add_dt_colors = function(dt,
                         seq,
                         low_color = 'white',
                         high_color = 'dodgerblue2',
                         mid_color = NULL,
                         column = 'Geek Rating') {
    
    cuts = seq
    my_color_ramp = colorRampPalette(c(low_color, mid_color, high_color))
    max_color = my_color_ramp(length(cuts)-5)[length(cuts)-5]
    
    dt |>
        DT::formatStyle(
            columns = column,
            backgroundColor = 
                DT::styleInterval(
                    cuts = cuts,
                    values = my_color_ramp(length(cuts)+1)
                )
        )
}

add_colors = function(dt) {
    
    dt |>
        add_dt_colors(column = 'Geek Rating',
                      seq = seq(5, 9, 0.1)) |>
        add_dt_colors(column = 'Average Rating',
                      seq = seq(6, 10, 0.1)) |>
        add_dt_colors(column = 'Users Rated',
                      seq = c(0, 100, 500, seq(1000, 10000, 1000), 15000, 25000, 50000)) |>
        add_dt_colors(column = 'Average Weight',
                      low_color = 'deepskyblue1',
                      mid_color = 'white',
                      high_color = 'orange',
                      seq = seq(0.8, 5, 0.1))
}
