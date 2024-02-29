make_bgg_link_gt = function(
                data) {
        
        data %>%
                mutate(link = make_bgg_link(game_id)) %>%
                mutate(name = map2(paste0(str_trunc(name, width = 40), " (", yearpublished, ")"),
                                   link,
                                   ~ gt_hyperlink(.x, .y))) %>%
                select(-link)
}

make_game_profile_gt = 
        function(data) {
                
                data %>%
                        unnest(c(bgg_info,
                                 bgg_outcomes,
                                 descriptions,
                                 images,
                                 playercounts)) %>%
                        prep_playercounts() %>%
                        mutate(link = make_bgg_link(game_id)) %>%
                        mutate(
                                game_id,
                                name,
                                # description = stringr::str_trunc(description, width = 300),
                                yearpublished,
                                image = image,
                                playingtime,
                                `Player Count`= case_when(
                                        !is.na(minplayers) & !is.na(maxplayers) ~ 
                                                paste(minplayers, maxplayers, sep = "-"),
                                        TRUE ~ NA),
                                `Recommended Players` = playercount_rec,
                                `Best Players` = playercount_best,
                                .keep = 'none'
                        ) %>%
                        prep_playingtime() %>%
                        rename(`Playing Time` = playingtime) %>%
                        mutate(
                                across(
                                        c("Recommended Players",
                                          "Best Players"),
                                        ~ case_when(.x == "" ~ NA_character_,
                                                    TRUE ~ .x)
                                )
                        ) %>%
                        mutate(link = make_bgg_link(game_id)) %>%
                        mutate(name = map2(paste0(str_trunc(name, width = 40), " (", yearpublished, ")"),
                                           link,
                                           ~ gt_hyperlink(.x, .y))) %>%
                        gt() %>%
                        cols_hide(c(game_id, yearpublished, link)) %>%
                        cols_move(columns = everything(),
                                  after = "image") %>%
                        gt_img_rows(columns = image, height = 200) %>%
                        set_gt_theme() %>%
                        sub_missing(
                                columns = everything(),
                                rows = everything(),
                                missing_text = "---"
                        ) %>%
                        cols_align(
                                columns = -c("name"),
                                align = "center"
                        ) %>%
                        # cols_width(
                        #         "image" ~ px(200)
                        # ) %>%
                        tab_options(table.font.size = 14)
        }

make_game_categories_gt = 
        function(data) {
                
                prep_categories_gt = function(data,
                                              category) {
                        
                        data %>%
                                mutate( !!enquo(category) := 
                                                map(
                                                        {{category}},
                                                        ~ .x %>%
                                                                bind_rows(.,
                                                                          tibble(id = NA, value = NA, type = NA)
                                                                ))
                                ) %>%
                                mutate( !!enquo(category) := 
                                                map(
                                                        {{category}},
                                                        ~ .x %>%
                                                                filter(!is.na(value)) %>%
                                                                reframe( !!enquo(category) := 
                                                                                 paste(value, 
                                                                                       collapse = "<br>")))
                                )
                        
                }
                
                prep_families_gt = function(data,
                                            category) {
                        
                        data %>%
                                mutate( !!enquo(category) := 
                                                map(
                                                        {{category}},
                                                        ~ .x %>%
                                                                bind_rows(.,
                                                                          tibble(family_type = NA, id = NA, family_value = NA, value = NA, type = NA)
                                                                ) %>%
                                                                mutate(value = paste(family_type, family_value, sep = ": ")))
                                ) %>%
                                mutate( !!enquo(category) := 
                                                map(
                                                        {{category}},
                                                        ~ .x %>%
                                                                filter(!is.na(family_value)) %>%
                                                                reframe( !!enquo(category) := 
                                                                                 paste(value, 
                                                                                       collapse = "<br>")))
                                )
                        
                }
                
                
                abbrev_text_gt <- function(x) {
                        paste0(
                                "<div style=\"display:table;table-layout:fixed;width:100%;\">",
                                "<div title=\"", x , "\", ", # `<p>` has been changed to `<div>` here
                                "style=\"overflow-x:hidden;text-overflow:ellipsis;white-space:nowrap\">",
                                x,
                                "</div>",
                                "</div>"
                        )
                }
                
                data %>%
                        prep_categories_gt(category = designers)  %>%
                        prep_categories_gt(category = publishers)  %>%
                        #   prep_categories_gt(category = artists)  %>%
                        prep_categories_gt(category = mechanics) %>%
                        prep_categories_gt(category = categories) %>%
                        prep_families_gt(category = families) %>%
                        unnest(c(bgg_info, designers, publishers, mechanics, categories, families)) %>%
                        select(game_id, name, yearpublished, any_of(c("designers", "publishers", "mechanics", "categories", "families"))) %>%
                        mutate(link = make_bgg_link(game_id)) %>%
                        mutate(name = map2(paste0(str_trunc(name, width = 40), " (", yearpublished, ")"),
                                           link,
                                           ~ gt_hyperlink(.x, .y))) %>%
                        gt() %>%
                        cols_hide(c(name, game_id, yearpublished, link)) %>%
                        cols_move(columns = everything(),
                                  after = "name") %>%
                        fmt_markdown(columns = everything()) %>%
                        set_gt_theme() %>%
                        #    set_gt_tab_options() %>%
                        # tab_options(
                        #         container.width = 1000
                        # ) %>%
                        sub_missing(
                                columns = everything(),
                                rows = everything(),
                                missing_text = "---"
                        ) %>%
                        cols_width(
                                name ~ px(100)
                        ) %>%
                        tab_style(
                                style = "vertical-align:top",
                                locations = cells_body(
                                        columns = everything()
                                )
                        )
                # tab_style(
                #         style = cell_text(align = "center"),
                #         locations = cells_column_labels(columns = everything())
                # )
                # 
        }


make_neighbors_gt_table = function(neighbors,
                                   top_n = 15,
                                   games) {
        
        neighbors %>%
                head(top_n) %>%
                left_join(.,
                          games %>%
                                  mutate(neighbor_name = name,
                                         neighbor_id = game_id,
                                         neighbor_yearpublished = yearpublished,
                                         est_averageweight,
                                         average,
                                         usersrated,
                                         bayesaverage,
                                         .keep = 'none'
                                  ),
                          by = join_by(neighbor_name, neighbor_id)) %>%
                mutate(rank = row_number()-1,
                       published = neighbor_yearpublished,
                       game_id = neighbor_id,
                       neighbor = neighbor_name,
                       similarity = case_when(.metric == 'canberra' | .metric == 'euclidean' | .metric == 'manhattan' ~ (1/sqrt(dist+1)),
                                              TRUE ~ 1-dist),
                       `Average Weight` = est_averageweight,
                       `Average Rating` = average,
                       `Geek Rating` = bayesaverage,
                       `User Ratings` = `usersrated`,
                       .keep = 'none') %>%
                mutate(link = make_bgg_link(game_id)) %>%
                mutate(neighbor = map2(paste0(neighbor, " (", published, ")"),
                                       link,
                                       ~ gt_hyperlink(.x, .y))) %>%
                mutate_if(is.numeric, round, 2) %>%
                gt() %>%
                cols_hide(columns = c(published, game_id, link)) %>%
                set_gt_theme() %>%
                set_gt_tab_options()  %>%
                data_color(
                        columns = c('similarity'),
                        domain = c(1, -1),
                        palette = c('white', 'navy')
                ) %>%
                add_outcomes_color_gt(
                        geek = 'Geek Rating',
                        average = 'Average Rating',
                        ratings = 'User Ratings',
                        complexity = 'Average Weight'
                ) %>%
                cols_align(
                        columns = c("rank",
                                  #  "image",
                                    "similarity",
                                    "Average Weight",
                                    "Average Rating",
                                    "Geek Rating"),
                        align = "center"
                ) %>%
                cols_align(
                        columns = c("neighbor"),
                        align = "left"
                ) %>%
                cols_width(
                        neighbor ~ px(200)
                ) %>%
                cols_move_to_start(
                        columns = c("rank")
                                    #"image")
                ) %>%
                sub_missing(
                        columns = everything(),
                        rows = everything(),
                        missing_text = "---"
                )
}

make_neighbors_rated_gt_table = function(neighbors,
                                   top_n = 15,
                                   games) {
        
        neighbors %>%
                left_join(.,
                          games %>%
                                  mutate(neighbor_name = name,
                                         neighbor_id = game_id,
                                         neighbor_yearpublished = yearpublished,
                                         est_averageweight,
                                         average,
                                         usersrated,
                                         bayesaverage,
                                         .keep = 'none'
                                  ),
                          by = join_by(neighbor_name, neighbor_id)) %>%
                # mutate(
                #         similarity = case_when(.metric == 'canberra' | .metric == 'euclidean' | .metric == 'euclidean' ~ (1/sqrt(dist+1)),
                #                                TRUE ~ 1-dist)
                # ) %>%
                mutate(rank = row_number()-1,
                       published = neighbor_yearpublished,
                       game_id = neighbor_id,
                       neighbor = neighbor_name,
                       similarity = case_when(.metric == 'canberra' | .metric == 'euclidean' | .metric == 'manhattan' ~ (1/sqrt(dist+1)),
                                              TRUE ~ 1-dist),
                       `Average Weight` = est_averageweight,
                       `Average Rating` = average,
                       `Geek Rating` = bayesaverage,
                       `User Ratings` = `usersrated`,
                       .keep = 'none') %>%
                mutate(score = similarity * `Geek Rating`) %>%
                arrange(desc(score)) %>%
                head(top_n) %>%
                # add image
                # add_game_image(
                #         data=.,
                #         info = games
                # ) %>%
                mutate(link = make_bgg_link(game_id)) %>%
                mutate(neighbor = map2(paste0(neighbor, " (", published, ")"),
                                       link,
                                       ~ gt_hyperlink(.x, .y))) %>%
                mutate_if(is.numeric, round, 2) %>%
                gt() %>%
                cols_hide(columns = c(published, game_id, link)) %>%
                # cols_move(columns = c("image"),
                #           after = "rank") %>%
                #          gt_img_rows(columns = image, height = 50) %>%
                set_gt_theme() %>%
                set_gt_tab_options()  %>%
                data_color(
                        columns = c('similarity'),
                        domain = c(1, -1),
                        palette = c('white', 'navy')
                ) %>%
                data_color(
                        columns = c('score'),
                        domain = c(10, 0),
                        palette = c('white', 'navy')
                ) %>%
                add_outcomes_color_gt(
                        geek = 'Geek Rating',
                        average = 'Average Rating',
                        ratings = 'User Ratings',
                        complexity = 'Average Weight'
                ) %>%
                cols_align(
                        columns = c("rank",
                                    "score",
                                    #  "image",
                                    "similarity",
                                    "Average Weight",
                                    "Average Rating",
                                    "Geek Rating"),
                        align = "center"
                ) %>%
                cols_align(
                        columns = c("neighbor"),
                        align = "left"
                ) %>%
                cols_width(
                        neighbor ~ px(200)
                ) %>%
                cols_move_to_start(
                        columns = c("rank", "neighbor", "score", "similarity")
                        #"image")
                )
}


add_gt_predictions_color = function(gt) {
        
        gt %>%
                # geek color
                data_color(
                        columns = c("Geek Rating"),
                        method = "bin",
                        na_color = "white",
                        bins = c(4, 5, 5.5, 6, 6.5, 7, 7.5, 8, 8.5),
                        autocolor_text = T,
                        palette = c("white", "dodgerblue2")
                )  %>%
                # average color
                data_color(
                        columns = c("Average Rating"),
                        method = "bin",
                        na_color = "white",
                        bins = c(0, 1, 3, 4, 5, 6, 6.5, 7, 7.5, 8, 8.5, 9, 10),
                        autocolor_text = T,
                        palette = c("red", "white", "dodgerblue2")
                )  %>%
                # ratings color
                data_color(
                        columns = c("User Ratings"),
                        method = "bin",
                        na_color = "white",
                        bins = c(0, 50, 100, 200, 500, 1000, 2000, 5000, 10000, 25000, 50000, 100000),
                        autocolor_text = T,
                        palette = c("white", "dodgerblue2")
                ) %>%
                # ratings color
                data_color(
                        columns = c("Average Weight"),
                        method = "bin",
                        na_color = "white",
                        #na_color = 'orange',
                        autocolor_text = T,
                        domain = c(1, 5),
                        palette = c("white", "orange")
                ) 
        
}

make_game_estimates_gt_table = function(predictions,
                                        id) {
        
        predictions %>%
                filter(game_id == id) %>%
                mutate(game_id, name, yearpublished,
                       type = 'Current',
                       averageweight, usersrated, average, bayesaverage,
                       .keep = 'none') %>%
                bind_rows(.,
                          predictions %>%
                                  filter(game_id %in% id) %>%
                                  mutate(game_id, name, yearpublished,
                                         type = 'Estimated',
                                         averageweight = .pred_averageweight,
                                         usersrated = .pred_usersrated,
                                         average = .pred_average,
                                         bayesaverage = .pred_bayesaverage,
                                         .keep = 'none')
                ) %>%
                arrange(desc(type)) %>%
                select(game_id, name, yearpublished, type, usersrated, averageweight, average, bayesaverage) %>%
                #    group_by(name, game_id, yearpublished) %>%
                mutate(
                        Type = type,
                        `Average Weight` = round(averageweight,2),
                        `User Ratings` = usersrated,
                        `Average Rating` = round(average,2),
                        `Geek Rating` = round(bayesaverage,2),
                        .keep = 'none'
                ) %>%
                select(Type, `Average Weight`, `Average Rating`, `Geek Rating`, `User Ratings`) %>%
                gt(
                        rowname_col = "Type",
                ) %>%
                set_gt_theme() %>%
                add_gt_predictions_color() %>%
                cols_align(
                        columns = c("Average Weight", "Average Rating", "Geek Rating", "User Ratings"),
                        align = c("center")
                ) %>%
                sub_missing(
                        columns = everything(),
                        rows = everything(),
                        missing_text = "---"
                )
        
}

make_games_matrix = function(data) {
        
        data %>%
                column_to_rownames("id") %>%
                select(-any_of(c("name", "game_id", "yearpublished"))) %>%
                select(where(is.numeric)) %>%
                as.matrix()
        
}

add_id = function(x, 
                  width = 50,
                  trunc = F) {
        
        
        if (trunc == T) {
                
                x %>%
                        mutate(id = str_trunc(paste(name, yearpublished, game_id), width))
        } else 
                
        {
                x %>%
                        mutate(id = paste(name, yearpublished, game_id))
                
        }
        
}


plot_neighbors_heatmap = function(data,
                                  dist_metric = 'canberra',
                                  clust_metric = 'complete',
                                  #  col = RColorBrewer::colorRampPalette(brewer.pal(11, "RdBu"))(100),
                                  ...) {
        
        # get data frame with only game and its neighbors
        # filter(game_id %in% (compare_games %>% head(50) %>% pull(game_id))) %>%
        #   sample_n(50) %>%
        
        # convert to matrix
        mat = data %>%
                add_id(trunc=F) %>%
                make_games_matrix()
        
        # make heatmap
        # mat %>%
        #         rdist(metric = 'canberra') %>%
        #         fastcluster::hclust(method = 'median')
        # 
        mat %>%
                heatmap3::heatmap3(
                        #  heatmap3::heatmap3(col = pal,
                        ...
                        #    Rowv = NA
                )
}

find_neighbors_contributors = 
        function(data,
                 dist_metric = 'canberra',
                 hclust_method = 'complete') {
                
                games_mat = 
                        data %>%
                        add_id() %>% 
                        make_games_matrix() 
                
                columns = 
                        games_mat %>%
                        t() %>%
                        rdist(., metric = dist_metric) %>%
                        fastcluster::hclust(method = hclust_method)
                
                games_mat[,columns$order] %>%
                        colnames()
        }


# make_game_features_gt_table = function()


#make_game_description_gt = 

# games %>%
#         sample_n(1) %>%
#         make_game_profile_gt()
# 
# games %>%
#         sample_n(1) %>%
#         make_game_categories_gt()
# 
# set.seed(1)
# 
# 
# games %>%
#         sample_n(1) %>%
#         select(game_id,
#                name,
#                bgg_info, 
#                descriptions) %>%
#         unnest(c(bgg_info, descriptions)) %>%
#         make_bgg_link_gt() %>%
#         select(game_id, name, yearpublished, description) %>%
#         mutate(description = str_replace_all(description, pattern = "\n\\b", replacement = "</br>")) %>%
#      #   mutate(description = paste(str_wrap(description, width = 80), "<br>")) %>%
#         gt() %>%
#         fmt_markdown(columns = everything()) %>%
#         cols_hide(columns = c(game_id, yearpublished)) %>%
#         set_gt_theme() %>%
#         cols_align(columns = c(name),
#                    align = "left")
#         