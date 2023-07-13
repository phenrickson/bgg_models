
# collection --------------------------------------------------------------

# create caption for visualizations
my_caption = function(data = games) {
        
        list(
                labs(
                        caption = paste(
                                paste("data from boardgamegeek.com as of", max(as.Date(data$load_ts[1]))),
                                #    paste("analysis by Phil Henrickson"),
                                paste("https://phenrickson.github.io/data-analysis-paralysis/"),
                                sep="\n")
                )
        )
        
}

get_workflows = function() {
        
        user_output$workflows
}

get_collection_date = function() {
        
        get_collection() %>%
                pull(user_load_ts) %>%
                head(1) %>%
                as.Date()
        
}

extract_collection = function() {
        
        user_output$user_collection
        
}


# combine collection with games info for report
get_collection = function(collection = extract_collection(),
                          info = games_info,
                          data = games) {
        
        
        collection %>%
                prep_collection() %>%
                left_join(games_info,
                          by = join_by(game_id, name)) %>%
                filter(game_id %in% games$game_id)
        
}

# helper function to get username
get_username = function(data = get_collection()) {
        
        data$username[1]
        
}

# plot collection
collection_categories_plot = function(data,
                                      vars = c("designers", "mechanics", "artists", "categories")) {
        
        map_df(vars,
               ~ data %>%
                       filter(own == 'yes') %>%
                       select(game_id, name, !!.x) %>%
                       unnest(!!.x) %>%
                       group_by(type, id, value) %>%
                       count(sort=T)) %>%
                group_by(type) %>%
                slice_max(order_by = n,
                          n = 25,
                          with_ties = F) %>%
                ggplot(aes(x=n,
                           fill = type,
                           y = tidytext::reorder_within(value, n, type)))+
                geom_col(alpha = 0.5)+
                facet_wrap(~type,
                           scales = "free")+
                tidytext::scale_y_reordered()+
                theme_bgg()+
                ylab("")+
                my_caption()+
                # labs(title = paste("Top", stringr::str_to_title(paste(vars, collapse = ", "))),
                #      subtitle = paste('Most frequent games in collection for', get_username()))+
                theme(plot.title = element_text(size =12,
                                                vjust = 2))+
                scale_fill_brewer(palette = "Set1")+
                guides(fill = 'none')
        
}

# plot owned status
collection_owned_plot = function(data,
                                 vars = c("own", "prevowned", "rated", "preordered", "wanttoplay")) {
        
        data %>%
                select(game_id, any_of(vars)) %>%
                mutate(across(any_of(vars),
                              ~ case_when(.x == 1 | .x == 'yes' ~ 'yes',
                                          .x == 0 | .x == 'no' ~ 'no'))) %>%
                mutate(rated_not_owned = case_when(
                        rated == 'yes' & (own == 'no' & prevowned == 'no') ~ 'yes')) %>%
                mutate(owned_not_rated = case_when(
                        (own == 'yes' | prevowned == 'yes' ) & rated == 'no' ~ 'yes')) %>%
                pivot_longer(cols = -c(game_id)) %>%
                filter(value == 'yes') %>%
                group_by(name) %>%
                summarize(n = sum(value == 'yes')) %>%
                mutate(label = case_when(name == 'owned_not_rated' ~ 'yes',
                                         TRUE ~ 'no')) %>%
                mutate(name = stringr::str_replace_all(name, "_", " "),
                       name = stringr::str_to_title(name),
                       name = stringr::str_replace(name, "Prevowned", "Previously Owned"),
                       name = stringr::str_replace(name, "Wanttoplay", "Want to Play")) %>%
                ggplot(aes(x=n,
                           fill = label,
                           label = n,
                           y = reorder(name, n)))+
                geom_col()+
                geom_text(hjust = -0.05,
                          size = 2.5)+
                theme_bgg()+
                ggtitle(paste(get_username(data), "'s Collection", sep=""),
                        subtitle = str_wrap('Highlighted in red are games the user has owned but not rated, potentially representing games the user has purchased but not played (aka, shelf of shame).',110))+
                my_caption()+
                scale_fill_manual(values = c("grey60", "firebrick3"))+
                guides(fill = "none")+
                ylab("")+
                xlab("number of games")+
                theme(plot.title = element_text(hjust = 0, 
                                                size = 16), 
                      plot.subtitle = element_text(hjust = 0,
                                                   size = 8))
        
}


# set options for gt tables
set_gt_tab_options = function(gt) {
        
        gt %>%
                tab_options(table.font.size = 12,
                            container.height = 800,
                            container.overflow.x = T,
                            container.overflow.y = T)
}

set_gt_widths = function(gt) {
        
        gt %>%
                cols_width(
                        "image" ~ px(150),
                        "game" ~ px(100),
                        "description" ~ px(400),
                        #    ends_with("r") ~ px(100),
                        everything() ~ px(50)
                )
}

set_gt_theme = function(gt) {
        
        gt %>%
                gtExtras::gt_theme_espn()
}


# prep playercounts for table
prep_playercounts = function(data) {
        
        data %>%
                mutate(across(c("playercount_best", "playercount_rec"),
                              ~ gsub(pattern = paste(as.character(seq(9, 100, by = 1)), collapse = "|"),
                                     replacement = "8+",
                                     x = .x))) %>%
                mutate(playercount_rec = case_when(
                        !is.na(playercount_best) & !is.na(playercount_rec) ~ 
                                paste(playercount_best, playercount_rec, sep = ","),
                        !is.na(playercount_best) & is.na(playercount_rec) ~
                                                                 playercount_best,
                        TRUE ~ playercount_rec)) %>%
                rowwise() %>%
                mutate(across(c("playercount_best", "playercount_rec"),
                              ~ str_split(.x, ",", ) %>%
                                      unlist() %>%
                                      unique() %>%
                                      sort() %>%
                                      paste(collapse = ",")))
}

prep_collection_table = function(collection = get_collection() %>%
                                         filter(own == 'yes'),
                                 game_data = games_info,
                                 description_length = 300) {
        
        collection  %>%
                select(username, game_id, name, rating) %>%
                left_join(.,
                          game_data %>%
                                  select(game_id, playercounts, bgg_outcomes, bgg_info, images, descriptions),
                          by = join_by(game_id)) %>%
                unnest(c(playercounts, bgg_info, bgg_outcomes, images, descriptions)) %>%
                mutate(link = make_bgg_link(game_id)) %>%
                prep_playercounts() %>%
                mutate(playercount_rec = 
                               case_when(is.na(playercount_rec) ~ playercount_best,
                                         TRUE ~ playercount_rec)) %>%
                mutate(playingtime = case_when(playingtime < 15 ~ '<15',
                                               playingtime < 30 ~ '<30',
                                               playingtime >= 30 & playingtime <= 60 ~ '30-60',
                                               playingtime > 60 & playingtime <= 120 ~ '61-120',
                                               playingtime > 121 & playingtime <= 180 ~ '121-180',
                                               playingtime > 180 ~ '180+',
                                               TRUE ~ as.character(playingtime))) %>%
                mutate(id = game_id,
                       game = name,
                       published = yearpublished,
                       `best        player count` = playercount_best,
                       `recommended player count` = playercount_rec,
                       `playing time` = case_when(is.na(playingtime)  ~ NA_character_,
                                                  TRUE ~ paste(playingtime, "minutes")),
                       complexity = round(averageweight, 2),
                       image = thumbnail,
                       description =  stringr::str_trunc(description,
                                                         description_length),
                       rating,
                       link = make_bgg_link(game_id),
                       .keep = 'none') %>%
                # make link
                mutate(game = map2(game,
                                   link,
                                   ~ gt_hyperlink(.x, .y)))
}


# build gt table for colleciton
make_collection_gt_table = function(collection_table) {
        
        collection_table %>%
                select(-contains("complexity")) %>%
                gt() %>%
                cols_hide(columns = c(published, id, link)) %>%
                gt_img_rows(columns = image, height = 100) %>%
                cols_move(
                        columns = c("game",
                                    "description",
                                    "playing time", 
                                    "best        player count", 
                                    "recommended player count", 
                                    "rating"),
                        after = "image"
                ) %>%
                cols_align(
                        columns = c("image", 
                                    "playing time",
                                    "best        player count",
                                    "recommended player count", 
                                    "rating"),
                        align = "center"
                ) %>%
                cols_align(
                        columns = c("game", "description"),
                        align = "left"
                ) %>%
                sub_missing(
                        columns = everything(),
                        rows = everything(),
                        missing_text = "---"
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

make_collection_datable = function(collection_table,
                                   page_length = 10) {
        
        
        rating = seq(3, 13)
        complexity = seq(0.5, 5.5, by = 0.1)
        
        color = 'dodgerblue2'
        low_color = 'deepskyblue1'
        high_color = 'orange'
        
        my_color_ramp = colorRampPalette(c("white", color))
        
        complexity_color_ramp = colorRampPalette(c(low_color, "white", high_color))
        
        max_color = my_color_ramp(length(rating)-3)[length(rating)-3]
        
        collection_table %>%
                mutate(Published = as.integer(published),
                       Image = make_image_link(image),
                       # Image = paste0('<img src =',
                       #                image,
                       #                ' height="52"></img>'),
                       Game = game,
                       Best = `best        player count`,
                       Recommended = `recommended player count`,
                   #    Time = `playing time`,
                       Complexity = complexity,
                       Rating = rating,
                       .keep = 'none') %>%
                DT::datatable(escape=F,
                              rownames = F,
                              extensions = c('Responsive'),
                              #  caption = "Games",
                              class = list(stripe =F),
                              filter = list(position = 'top'),
                              options = list(pageLength = page_length,
                                             initComplete = htmlwidgets::JS(
                                                     "function(settings, json) {",
                                                     paste0("$(this.api().table().container()).css({'font-size': '", '10pt', "'});"),
                                                     "}"),
                                             scrollX=F,
                                             columnDefs = list(
                                                     list(className = 'dt-center',
                                                          visible=T,
                                                          targets=c("Image",
                                                                    "Published",
                                                                    "Best",
                                                                    "Recommended",
                                                                    "Complexity",
                                                                    "Rating")
                                                     )
                                             )
                              )
                ) %>%
                DT::formatStyle(
                        columns = "Rating",
                        backgroundColor = 
                                DT::styleInterval(
                                        cuts = rating,
                                        values = my_color_ramp(length(rating)+1)
                                )
                ) %>%
                DT::formatStyle(
                        columns = "Complexity",
                        backgroundColor = 
                                DT::styleInterval(
                                        cuts = complexity,
                                        values = complexity_color_ramp(length(complexity)+1)
                                )
                )
}

# table
make_top_n_table = function(preds,
                            collection = get_collection(),
                            workflow,
                            top_n = 10,
                            years = seq(2010, 2021),
                            outcome, 
                            event_class = 'yes',
                            ...) {
        
        #  outcome = enquo(outcome)
        
        # prediction tables
        preds = preds %>%
                filter(wflow_id == workflow) %>%
                filter(yearpublished %in% c(years)) %>%
                group_by(yearpublished) %>%
                slice_max(.pred_yes, 
                          n = top_n,
                          with_ties = F) %>%
                arrange(desc(.pred_yes)) %>%
                mutate(Rank = row_number()) %>%
                select(game_id, name, any_of(outcome), Rank, yearpublished)  %>%
                ungroup()
        
        highlight_own = 
                collection %>%
                filter(own == 'yes') %>% 
                # mutate(link = make_bgg_link(game_id)) %>%
                # mutate(name = map2(name,
                #                    link,
                #                    ~ gt_hyperlink(.x, .y))) %>%
                pull(name)
        
        highlight_ever_owned = 
                collection %>%
                filter(ever_owned == 'yes') %>%
                # mutate(link = make_bgg_link(game_id)) %>%
                # mutate(name = map2(name,
                #                    link,
                #                    ~ gt_hyperlink(.x, .y))) %>%
                pull(name)
        
        
        table = preds %>%
                # mutate(link = make_bgg_link(game_id),
                #        link = glue::glue("[{name}]({link})"),
                #        name =  map(link, gt::md)) %>%
                select(name, yearpublished, Rank) %>%
                pivot_wider(id_cols = c("Rank"),
                            names_from = c("yearpublished"),
                            values_from = c("name"),
                            values_fn = list)

table %>%
        select(Rank, sort(names(.))) %>%
        unnest(cols = names(.)) %>%
        gt::gt() %>%
        gt::tab_options(table.font.size = 10) %>%
        gt::cols_align(align = c("center")) %>%
        # add color for own
        gt::data_color(
                columns = everything(),
                method = "factor",
                na_color = "white",
                autocolor_text = T,
                fn = function(x) case_when(x %in% highlight_own ~ 'dodgerblue2',
                                           x %in% highlight_ever_owned ~ 'skyblue1',
                                           TRUE ~ 'white')
        )
}

# make_top_n_table(
#         user_output$training_predictions,
#         workflow = 'all_trees_lightgbm',
#         top_n = 25,
#         outcome = user_output$outcome,
# )

# get tidy version of outcome
get_tidy_outcome = function(x) {
        
        x %>%
                stringr::str_replace(., "_",
                                     " ") %>%
                stringr::str_to_title(.)
        
}

# make prob outcome
get_prob_outcome = function(x) {
        
        paste0("Pr(", x, ")")
        
}

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


# make tables with game picture and description
make_predictions_gt_table = function(predictions,
                                     outcome,
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
        
        tidy_outcome = get_tidy_outcome(outcome)
        
        prob_tidy_outcome = get_prob_outcome(tidy_outcome)
        
        # make data for table
        table_data = 
                predictions %>%
                select(yearpublished, game_id, name, .pred_yes, any_of(outcome)) %>%
                add_game_description() %>%
                add_game_image(filter = T) %>%
                # make bgg links
                mutate(link = make_bgg_link(game_id)) %>%
                # arrange
                arrange(desc(.pred_yes)) %>%
                head(top_n) %>%
                # sort
                mutate(
                        rank = row_number(),
                        game = name,
                        image,
                        description,
                        published = yearpublished,
                        id = game_id,
                        !!rlang::sym(prob_tidy_outcome) := round(.pred_yes,3),
                        !!rlang::sym(tidy_outcome) := !!rlang::sym(outcome),
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
                cols_hide(columns = c(published, id, link)) %>%
                gt_img_rows(columns = image, height = 100) %>%
                cols_align(
                        columns = c(rank, image, prob_tidy_outcome, tidy_outcome),
                        align = "center"
                ) %>%
                cols_align(
                        columns = c("description", "game"),
                        align = "left"
                ) %>%
                data_color(
                        columns = c(prob_tidy_outcome),
                        method = "numeric",
                        na_color = "grey60",
                        autocolor_text = T,
                        palette = c("white", "dodgerblue2"),
                        domain = c(0, 1)
                )  %>%
                data_color(
                        columns = c(tidy_outcome),
                        method = "factor",
                        na_color = "white",
                        autocolor_text = T,
                        fn = function(x) case_when(x == 'yes'  ~ 'dodgerblue2',
                                                   TRUE ~ 'white')
                ) %>%
                # set columns
                cols_move(
                        columns = c("rank", "image", "game", "description"),
                        after = "rank"
                ) %>%
                set_gt_theme() %>%
                set_gt_tab_options() %>%
                cols_width(
                        "image" ~ px(150),
                        "game" ~ px(100),
                        "description" ~ px(400),
                        #    ends_with("r") ~ px(100),
                        everything() ~ px(50)
                )
        
}

# # new and upcoming
# bind_rows(
#         user_output$valid_predictions,
#         user_output$upcoming_predictions %>%
#                 filter(yearpublished > user_output$end_train_year + user_output$valid_window)) %>%
#         filter(wflow_id == 'all_trees_lightgbm') %>%
#         make_game_gt_table(
#                 outcome = 'own',d
#                 top_n = 25,
#                 description_length = 400
#         ) %>%
#         gt::tab_header(
#                 title = paste("Top (Newer) Games for",
#                               get_username()),
#                 subtitle = paste("Rankings based on predictive model trained on user's collection using games released through",
#                                  user_output$end_train_year)
#         )
# 
# # older games
# bind_rows(user_output$training_predictions) %>%
#         filter(wflow_id == 'all_trees_lightgbm') %>%
#         make_game_gt_table(
#                 outcome = 'own',
#                 top_n = 25,
#                 description_length = 500,
#         ) %>%
#         gt::tab_header(
#                 title = paste("Top (Older) Games for",
#                               get_username()),
#                 subtitle = paste("Rankings based on predictive model trained on user's collection using games released through",
#                                  user_output$end_train_year)
#         )

make_games_datatable = function(data,
                                caption = 'Top Games',
                                outcome = user_output$outcome,
                                pagelength = 15,
                                ...) {
        
        
        cuts = seq(0, 1.5, 0.05)
        
        color = 'dodgerblue2'
        
        my_color_ramp = colorRampPalette(c("white", color))
        
        max_color = my_color_ramp(length(cuts)-5)[length(cuts)-5]
        
        data %>%
                DT::datatable(escape=F,
                              rownames = F,
                              extensions = c('Responsive'),
                              #  caption = "Games",
                              class = list(stripe =F),
                              filter = list(position = 'top'),
                              options = list(pageLength = pagelength,
                                             initComplete = htmlwidgets::JS(
                                                     "function(settings, json) {",
                                                     paste0("$(this.api().table().container()).css({'font-size': '", '10pt', "'});"),
                                                     "}"),
                                             scrollX=F,
                                             columnDefs = list(
                                                     list(className = 'dt-center',
                                                          visible=T,
                                                          targets=c("Rank",
                                                        #            "Image",
                                                                    "Published",
                                                                    get_prob_outcome(get_tidy_outcome(outcome)),
                                                                    get_tidy_outcome(outcome)
                                                          ))))
                ) %>%
                DT::formatStyle(
                        columns = get_prob_outcome(get_tidy_outcome(outcome)),
                        backgroundColor = 
                                DT::styleInterval(
                                        cuts = cuts,
                                        values = my_color_ramp(length(cuts)+1)
                                )
                ) %>%
                DT::formatStyle(
                        'Own',
                        valueColumns = 'Own',
                        backgroundColor = DT::styleEqual(levels = c('yes','no'),
                                                         c(max_color,
                                                           'white')))
        
        
}


prep_games_datatable = function(predictions,
                                info = games_info,
                                outcome = user_output$outcome) {
        
        predictions %>%
                left_join(.,
                          info %>%
                                  select(game_id, images) %>%
                                  unnest(images) %>%
                                  select(game_id, thumbnail),
                          by = join_by(game_id)) %>%
            #    mutate(image = make_image_link(thumbnail)) %>%
                mutate(name = make_hyperlink(make_bgg_link(game_id), mytext = name)) %>%
                arrange(desc(.pred_yes)) %>%
                mutate(rank = row_number()) %>%
                mutate(Rank = rank,
                       Published = as.integer(yearpublished),
                  #     Image = image,
                       Name = name,
                       !!rlang::sym(get_prob_outcome(get_tidy_outcome(outcome))) := formatC(.pred_yes, digits=3, format="f"),
                       !!rlang::sym(get_tidy_outcome(outcome)) := !!rlang::sym(outcome),
                       .keep = 'none')
        
}


# user_output$training_predictions %>%
#         prep_games_datatable(outcome = user_output$outcome) %>%
#         make_games_datatable()


# assessment -------------------------------------------------------------

# get the model type of the worfklow
tidy_wflow_id = function(x) {
        
        x %>%
                stringr::str_split_i(., pattern = "_", i = 3)
        
}

# separation plot
make_separation_plot = function(predictions,
                                truth,
                                event_level = 'yes') {
        
        .pred_var = paste(".pred", event_level, sep = "_")
        
        predictions %>%
                group_by(wflow_id) %>%
                arrange(!!rlang::sym(.pred_var)) %>%
                mutate(rank = row_number()) %>%
                mutate(wflow_id = tidy_wflow_id(wflow_id)) %>%
                {
                        ggplot(data = .,
                               aes(x=rank,
                                   y=!!rlang::sym(.pred_var)))+
                                geom_point(size = 0.01)+
                                geom_vline(data = . %>%
                                                   filter(!!rlang::sym(truth) == 'yes'),
                                           aes(xintercept = rank),
                                           alpha = 0.5,
                                           color = 'dodgerblue2')+
                                facet_wrap(wflow_id ~.,
                                           ncol = 1)+
                                theme_bgg()
                }
}

add_gt_wflow_color = function(gt) {
        
        
        gt %>%
                gt::data_color(
                        columns = wflow_id,
                        target_columns = everything(),
                        method = "factor",
                        na_color = "white",
                        autocolor_text = T,
                        fn = function(x) case_when(x== 'glmnet' ~ '#000000',
                                                   x== 'lightgbm' ~ "#E69F00",
                                                   TRUE ~ 'white')
                )
}


# user_output$training_predictions %>%
#         make_separation_plot(truth = 'own')

# make roc plot
make_roc_plot = function(predictions,
                         truth,
                         event_level = 'yes') {
        
        predictions %>%
                mutate(wflow_id = tidy_wflow_id(wflow_id)) %>%
                group_by(wflow_id) %>%
                yardstick::roc_curve(
                        truth = !!rlang::sym(truth),
                        .pred_yes,
                        event_level = 'second'
                ) %>%
                autoplot(size = 1.04,
                         color = wflow_id)+
                theme_bgg()+
                ggthemes::scale_color_colorblind()
        
}

# user_output$training_predictions %>%
#         make_roc_plot(truth = 'own')

# make pr curve
make_pr_curve_plot = function(predictions,
                              truth,
                              event_level = 'yes') {
        
        predictions %>%
                mutate(wflow_id = tidy_wflow_id(wflow_id)) %>%
                group_by(wflow_id) %>%
                yardstick::pr_curve(
                        truth = !!rlang::sym(truth),
                        .pred_yes,
                        event_level = 'second'
                ) %>%
                autoplot(size = 1.04,
                         color = wflow_id)+
                theme_bgg()+
                ggthemes::scale_color_colorblind()
}

# user_output$training_predictions %>%
#         make_pr_curve_plot(truth = 'own')


# evaluate threshold
# eval_threshold_plot = function(
                #                 predictions,
#                 thresholds = seq(0, 1, by = 0.05),
#                 truth,
#                 event_level = 'yes') {
#         
#         map_df(thresholds,
#                .f = ~ predictions %>%
#                        mutate(!!rlang::sym(truth) := factor(!!rlang::sym(truth),
#                                                             levels = c("yes", "no"))) %>%
#                        mutate(
#                                .pred = probably::make_two_class_pred(
#                                        estimate = .pred_yes, 
#                                        levels = levels(!!rlang::sym(truth)),
#                                        threshold = .x)
#                        ) %>%
#                        group_by(wflow_id) %>%
#                        precision(
#                                truth = !!truth,
#                                estimate = .pred,
#                                event_level = 'first') %>%
#                        mutate(threshold = .x)) %>%
#                 ggplot(aes(x=threshold,
#                            color = wflow_id,
#                            y=.estimate))+
#                 # geom_point()+
#                 geom_line(size = 1.04, alpha = 0.95)+
#                 facet_wrap(.metric~.)+
#                 theme_bgg()
#         
# }
# 
# user_output$training_predictions %>%
#         eval_threshold_plot(
#                 truth = 'own'
#         )


# user_output$training_predictions %>%
#         eval_threshold_plot(
#                 truth = 'own'
#         )


# user_output$valid_predictions %>%
#         make_roc_plot(truth = 'own')
# 
# 
# user_output$training_predictions %>%
#         make_separation_plot(truth = 'own')


# # density plot
# make_density_plot = function(predictions,
#                              truth,
#                              event_level = 'yes') {
#         
#         .pred_var = paste(".pred", event_level, sep = "_")
#         
#         predictions %>%
#                 mutate(wflow_id = tidy_wflow_id(wflow_id)) %>% 
#                 ggplot(aes(x=rlang::sym(.pred_var),
#                            fill = truth))+
#                 geom_density()+
#                 facet_wrap(wflow_id ~.)
#         
#         
# }

# library(probably)
# library(ggridges)

# make_threshold_plot = function(predictions,
#                                metric,
#                                       
#                                       )
#         


# bind_rows(
#         user_output$training_predictions,
#         user_output$valid_predictions) %>%
#         filter(yearpublished > 1990) %>%
#         group_by(wflow_id,
#                  yearpublished = plyr::round_any(yearpublished, accuracy = 2, floor)) %>%
#         yardstick::roc_auc(
#                 truth = user_output$outcome,
#                 .pred_yes,
#                 event_level = 'second'
#         ) %>%
#         ggplot(aes(x=yearpublished,
#                    y=.estimate,
#                    color = wflow_id))+
#         geom_point()+
#         geom_line(alpha = 0.5,
#                   lwd = 1.04)+
#         theme_minimal()

# # lift/roc chart
# bind_rows(
#         user_output$training_predictions,
#         user_output$valid_predictions) %>%
#         group_by(wflow_id) %>%
#         yardstick::lift_curve(
#                 truth = !!user_output$outcome,
#                 .pred_yes,
#                 event_level = 'second'
#         ) %>%
#         autoplot()+
#         facet_wrap(wflow_id ~.)


# calibration plot
# user_output$training_predictions %>%
#         filter(wflow_id == 'all_trees_lightgbm') %>%
#         cal_plot_breaks(
#                 group = wflow_id,
#                 truth = own,
#                 estimate = .pred_yes,
#                 event_level = 'second')+
#         facet_wrap(wflow_id ~.)

# user_output$valid_predictions %>%
#         group_by(wflow_id) %>%
#         yardstick::roc_curve(
#                 truth = user_output$outcome,
#                 .pred_yes,
#                 event_level = 'second'
#         ) %>%
#         autoplot(lwd = 1.04)+
#         theme_bgg()


# model explanations ------------------------------------------------------


# glmnet plots
# coef plot
# user_workflows %>%
#         get_workflow(model = "glmnet") %>%
#         glmnet_coef_plot()+
#         ggtitle(paste("What Predicts", get_username(collection), "'s Collection?", sep=" "),
#                 subtitle = str_wrap("Coefficients from a penalized (ridge) logistic regression for games owned by specified user. Predictors centered and scaled."))+
#         theme(plot.title = element_text(hjust = 0, 
#                                         size = 16), 
#               plot.subtitle = element_text(hjust = 0,
#                                            size = 10))+
#         my_caption()+
#         xlab("Estimated Effect on Outcome")+
#         ylab("Features")


# # trace plot
# user_workflows %>%
#         get_workflow(model = "glmnet") %>%
#         glmnet_trace_plot()

# lightgbm plots
# variable importance
# user_workflows %>%
#         get_workflow(model = "lightgbm") %>%
#         lightgbm_vip() %>%
#         lightgbm_vip_plot()+
#         ggtitle(paste("What Predicts", get_username(collection), "'s Collection?", sep=" "),
#                 subtitle = str_wrap("Feature importance from light gradient boosted machine, an ensemble of boosted decision trees. Features that are important in predicting a user's collection will appear towards the top of cover, frequency, and/or gain."))+
#         theme(plot.title = element_text(hjust = 0,
#                                         size = 16),
#               plot.subtitle = element_text(hjust = 0,
#                                            size = 10))+
#         my_caption()+
#         xlab("Value")+
#         ylab("Feature")+
#         theme(panel.grid.major = element_blank())

# partial dependence
make_partial_plot = function(workflow,
                             var,
                             length =10,
                             seed = 1) {
        
        model = 
                workflow %>%
                extract_fit_engine()
        
        training =
                workflow %>%
                extract_mold() %$%
                predictors
        
        var_summary = 
                training %>%
                summarize(min = min(!!rlang::sym(var)),
                          median = median(!!rlang::sym(var)),
                          max = max(!!rlang::sym(var)))
        
        var_range = 
                seq(var_summary$min,
                    var_summary$max,
                    length.out = length)
        
        partial_preds = 
                foreach(i = 1:length(var_range),
                        .combine = 'bind_rows') %do% {
                                
                                temp = training
                                
                                set.seed(seed)
                                samp = temp %>%
                                        sample_n(1000)
                                
                                samp[paste(var)] = var_range[i]
                                
                                model %>%
                                        predict(as.matrix(samp)) %>%
                                        as_tibble() %>%
                                        mutate(
                                                .row = row_number(),
                                                .pred = value,
                                                !!rlang::sym(var) := var_range[i],
                                                .keep = 'none')
                                
                        }
        
        partial_preds %>%
                ggplot(aes(x=!!rlang::sym(var),
                           y = .pred))+
                geom_line(aes(group = .row),
                          alpha = 0.5)+
                #  geom_smooth()+
                theme_bgg()
        
        
}

# user_workflows %>%
#         get_workflow(model = 'lightgbm') %>%
#         make_partial_plot(var = 'est_averageweight',
#                           seed = 5)

# user_workflows %>%
#         get_workflow(model = 'lightgbm') %>%
#         make_partial_plot(var = 'word_count',
#                           seed = 5)


# lightgbm interpret ------------------------------------------------------

# retrieve id
get_game_id = function(game_data,
                       game_name) {
        
        game_data %>%
                filter(name %in% game_name) %>%
                filter(!is.na(yearpublished)) %>%
                select(game_id, name) %>%
                pull(game_id)
        
}

# function for getting game data
get_game_data = function(game_data = data,
                         id,
                         name) {
        
        get_game_id = function(game_data,
                               name) {
                
                var = name
                
                game_data %>%
                        filter(name %in% var) %>%
                        filter(!is.na(yearpublished)) %>%
                        select(game_id, name) %>%
                        pull(game_id)
                
        }
        
        if (missing(id)) {
                
                id = game_data %>%
                        get_game_id(name %in% name)
                
                game_data %>%
                        filter(game_id %in% id)
                
        } else {
                game_data %>%
                        filter(game_id %in% id)
        }
        
}

# return predictors from workflow
get_workflow_predictors = function(workflow) {
        
        workflow %>%
                extract_mold() %$%
                predictors %>%
                names()
        
}


# interpret
lightgbm_interpret = function(workflow = user_output$workflows %>%
                                      get_workflow(model = 'lightgbm'),
                              game_data,
                              outcome,
                              ...)  {
        
        selected_game_data = 
                game_data %>%
                mutate(.id = row_number())
        
        # get predicted probabilities
        selected_predicted =
                workflow %>%
                augment(selected_game_data,
                        type = 'prob') %>%
                select(game_id, name, .pred_yes, !!outcome)
        
        # bake with recipe
        # get names of predictors
        game_predictors = 
                workflow %>%
                extract_recipe() %>%
                bake(selected_game_data) %>%
                select(any_of(get_workflow_predictors(workflow))) %>%
                mutate(.id = row_number())
        
        # extract model
        model = 
                workflow %>%
                extract_fit_engine()
        
        # convert to matrix
        game_matrix = 
                game_predictors %>%
                select(-.id) %>%
                as.matrix()
        
        # get feature contributions
        contrib = 
                lightgbm::lgb.interprete(model = model,
                                         data = game_matrix,
                                         idxset = 1:nrow(game_matrix)) %>%
                data.table::rbindlist(idcol = T)
        
        contrib %>%
                # get game info
                left_join(
                        selected_game_data %>%
                                select(.id, game_id, name, yearpublished),
                        by = c(".id")
                ) %>%
                select(.id, game_id, name, everything()) %>%
                # get prediction
                left_join(.,
                          selected_predicted,
                          by = c("game_id", "name")) %>%
                # get values for features
                left_join(
                        game_predictors %>% 
                                pivot_longer(cols = -c(".id"), 
                                             names_to = c("Feature"), 
                                             values_to = "Value"),
                        by = c(".id",
                               "Feature"))
}

# plot
lightgbm_interpret_plot = function(interpret,
                                   n_features = 25,
                                   outcome,
                                   ...) {
        
        # select top features by .id
        top_features = 
                interpret %>%
                group_by(.id) %>%
                slice_max(abs(Contribution),
                          n = n_features,
                          with_ties = F) %>%
                mutate(.id,
                       Feature,
                       Top_Feature = 'yes',
                       .keep = 'none')
        
        # condense other features into other category
        plot_data = 
                interpret %>%
                left_join(.,
                          top_features,
                          by = c(".id", "Feature")) %>%
                mutate(Feature = case_when(Top_Feature == 'yes' ~ Feature,
                                           TRUE ~ 'All Other Features')) %>%
                group_by(.id, game_id, name, Feature) %>%
                summarize(Value = sum(Value),
                          Contribution = sum(Contribution),
                          .groups = 'drop') %>%
                mutate(Feature_Label = case_when(
                        Feature != 'All Other Features' ~ paste(bggUtils::present_text(Feature, minlength=20), "=", round(Value, 2)),
                        TRUE ~ Feature))
        
        plot_data %>%
                left_join(.,
                          interpret %>%
                                  select(game_id,
                                         name,
                                         .pred_yes,
                                         !!outcome) %>%
                                  distinct(),
                          by = join_by(game_id, name)
                ) %>%
                mutate(Facet_Label =
                               paste(
                                       paste0(name),
                                       paste("Pr(Own):", round(.pred_yes, 3)),
                                       paste("Own:", !!rlang::sym(outcome)),
                                       sep = "\n")) %>%
                ggplot(aes(x = Contribution,
                           fill = Contribution,
                           y = tidytext::reorder_within(Feature_Label, Contribution, .id)))+
                geom_col()+
                facet_wrap(~Facet_Label,
                           scales = "free_y")+
                tidytext::scale_y_reordered()+
                ylab("")+
                bggUtils::theme_bgg()+
                scale_fill_gradient2(low = 'red',
                                     mid = 'grey60',
                                     high = 'dodgerblue2',
                                     midpoint = 0,
                                     limits = c(-0.5, 0.5),
                                     oob = scales::squish)+
                guides(fill = 'none')+
                theme(strip.text = element_text(hjust = 0))+
                #  ggtitle(str_wrap("Displaying Shapley values to identify which features were the most influential in reaching an individual prediction. Features that increase a prediction are positive (in blue) while features that decrease a prediction are negative (in red)"))+
                theme(plot.title = element_text(hjust = 0,
                                                size = 10),
                      strip.text.x = element_text(size = 10),
                      plot.subtitle = element_text(hjust = 0,
                                                   size = 10))+
                # my_caption+
                xlab("")+
                ylab("")+
                theme(panel.grid.major = element_blank())
        
}


# # select top 6 games for 2023
# ids =
#         user_output$upcoming_predictions %>%
#         filter(grepl("lightgbm", wflow_id)) %>%
#         head(4) %>%
#         pull(game_id)
#
# # make plots
# plots = map(ids,
#             ~ get_game_data(game_data = user_output$user_collection %>%
#                                     join_bgg_games(games),
#                             id = .x) %>%
#                     lightgbm_interpret(game_data = .,
#                                        outcome = user_output$outcome) %>%
#                     lightgbm_interpret_plot(outcome = user_output$outcome,
#                                             n_features = 15))
#
# # walk
# walk(plots,
#      print)
#
# cowplot::plot_grid(plotlist=plots)


make_effects_plot = function(estimates,
                             category = 'designer') {
        
        if (category == 'category') {
                category_var = 'categories'
        } else if (category == 'family') {
                category_var = 'families'
        } else {
                category_var = paste0(category, 's')
        }
        
        estimates %>%
                filter(grepl(paste0("^", category_var), term)) %>%
                mutate(estimate = round(estimate, 3)) %>%
                filter(estimate != 0) %>%
                filter(!grepl("number_mechanics_ns", term)) %>%
                mutate(term = gsub(paste0("^", category_var, "_"), "", term)) %>%
                # term = gsub("_", " ", term),
                # term = str_to_title(term)) %>%
                select(term, estimate) %>%
                arrange(desc(estimate)) %>%
                left_join(.,
                          games_info %>%
                                  select(game_id, any_of(category_var)) %>%
                                  unnest(!!category_var) %>%
                                  mutate(id,
                                         value,
                                         term = bggUtils::clean_text(value),
                                         type,
                                         .keep = 'none') %>%
                                  distinct(),
                          by = join_by(term)) %>%
                filter(!is.na(id)) %>%
                group_by(sign = case_when(estimate > 0 ~ 'positive',
                                          estimate < 0 ~ 'negative')) %>%
                slice_max(abs(estimate),
                          n = 13,
                          with_ties = F) %>%
                ungroup() %>%
                mutate(label = category_var) %>%
                mutate(value = abbreviate(value, minlength = 30)) %>%
                ggplot(aes(x=estimate,
                           fill = estimate,
                           y=reorder(value, estimate)))+
                geom_col()+
                scale_fill_gradient2(low = 'red',
                                     mid = 'grey60',
                                     high = 'dodgerblue2',
                                     midpoint = 0,
                                     limits = c(-0.1, 0.1),
                                     oob = scales::squish)+
                ylab("")+
                geom_vline(xintercept = 0,
                           alpha = 0.5)+
                theme_bgg()+
                theme(legend.title = element_text())+
                guides(fill = guide_colorbar(barheight = 0.5,
                                             barwidth = 15,
                                             title.position = 'top'))+
                guides(fill = 'none')+
                facet_wrap(paste(str_to_title(category), 
                                 "Effects")~.)+
                theme(plot.title = element_text(hjust = 0,
                                                size = 12,
                                                vjust = 2),
                      plot.subtitle = element_text(hjust = 0,
                                                   size = 10),
                      strip.text.x = element_text(size = 10))
        
}
