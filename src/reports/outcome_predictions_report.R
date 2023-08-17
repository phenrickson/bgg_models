make_predictions_gt_table = function(predictions,
                                     outcome = bgg_outcomes(),
                                     info = games_info,
                                     game_data = games,
                                     top_n = 10,
                                     description_length = 400,
                                     ...) {
        
        require(gt)
        require(gtExtras)
        
        make_web_image =
                function(x, height=100) {
                        
                        web_image(url = x,
                                  height = px(height) 
                        ) 
                        
                }
        
        add_game_image = function(data,
                                  filter = T,
                                  ...) {
                
                game_image = 
                        data %>%
                        left_join(., info %>%
                                          select(game_id, images) %>%
                                          unnest(images) %>%
                                          select(game_id, thumbnail) %>%
                                          rename(image = thumbnail),
                                  by = c("game_id"))
                
                if (filter == T) {
                        
                        game_image %>%
                                filter(!is.na(image))
                        
                } 
                else {
                        
                        game_image
                }
                
                
        }
        
        add_game_description = function(data,
                                        ...) {
                
                data %>%
                        left_join(., game_data %>%
                                          select(game_id, description),
                                  by = c("game_id"))
                
        }
        
        # make data for table
        table_data = 
                predictions %>%
                select(yearpublished, game_id, name, starts_with(".pred"), any_of(outcome)) %>%
                add_game_description() %>%
                add_game_image(filter = T) %>%
                # make bgg links
                mutate(link = make_bgg_link(game_id)) %>%
                # arrange
                arrange(desc(.pred_bayesaverage)) %>%
                head(top_n) %>%
                # sort
                mutate(
                        rank = row_number(),
                        game = name,
                        image,
                        description,
                        published = yearpublished,
                        id = game_id,
                        complexity = round(.pred_averageweight,1),
                        ratings = .pred_usersrated,
                        average = round(.pred_average, 1),
                        geek = round(.pred_bayesaverage, 1),
                        link,
                        .keep = 'none') %>%
                # make link
                mutate(game = map2(paste0(game, " (", published, ")"),
                                   link,
                                   ~ gt_hyperlink(.x, .y))) %>%
                # truncate description
                mutate(description = stringr::str_trunc(description,
                                                        description_length))
        
        # make gt table
        table_data %>%
                gt() %>%
                cols_hide(columns = c(id, link)) %>%
                gt_img_rows(columns = image, height = 100) %>%
                cols_align(
                        columns = c(published, rank, image, geek, average, complexity, ratings),
                        align = "center"
                ) %>%
                cols_align(
                        columns = c("description", "game"),
                        align = "left"
                ) %>%
                # geek color
                data_color(
                        columns = c(geek),
                        method = "bin",
                        na_color = "dodgerblue2",
                        bins = c(5, 5.5, 6, 6.5, 7, 7.5, 8),
                        autocolor_text = T,
                        palette = c("white", "dodgerblue2")
                        )  %>%
                # average color
                data_color(
                        columns = c(average),
                        method = "bin",
                        na_color = "dodgerblue2",
                        bins = c(1, 3, 4, 5, 6, 6.5, 7, 7.5, 8, 8.5, 9),
                        autocolor_text = T,
                        palette = c("red", "white", "dodgerblue2")
                )  %>%
                # ratings color
                data_color(
                        columns = c(ratings),
                        method = "bin",
                        na_color = "dodgerblue2",
                        bins = c(0, 50, 100, 200, 500, 1000, 2000, 5000, 10000, 25000, 50000, 100000),
                        autocolor_text = T,
                        palette = c("white", "dodgerblue2")
                )  %>%
                # ratings color
                data_color(
                        columns = c(complexity),
                        method = "bin",
                        na_color = 'orange',
                        autocolor_text = T,
                        domain = c(1, 5),
                        palette = c("white", "orange")
                )  %>%
                # set columns
                cols_move(
                        columns = c("rank", "published", "image", "game", "description"),
                        after = "rank"
                ) %>%
                cols_move(
                         columns = c("published"),
                         after = "rank"
                ) %>%
                set_gt_theme() %>%
                set_gt_tab_options() %>%
                cols_width(
                        "image" ~ px(150),
                        "game" ~ px(100),
                        "description" ~ px(400),
                        "published" ~ px(100),
                        #    ends_with("r") ~ px(100),
                        everything() ~ px(50)
                )
        
}
