make_bgg_link = function(game_id) {
        
        paste0("https://boardgamegeek.com/boardgame/",
               game_id)
}

make_hyperlink = function(myurl,
                          mytext=myurl) {
        paste('<a href="',myurl,'">',mytext,'</a>')
}

make_predictions_gt_table = function(predictions,
                                     outcome = bgg_outcomes(),
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
                        left_join(., game_data %>%
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


extract_vetiver_workflow_objects = 
        function(vetiver_model) {
                
                training_data = 
                        vetiver_model %>%
                        extract_training_data() %>%
                        restore_training_data()
                
                workflow_objs = 
                        vetiver_model %>%
                        unbundle_workflow() %>%
                        extract_workflow_objects()
                
                baked_training_data = 
                        workflow_objs %>%
                        pluck("workflow", 1) %>%
                        extract_recipe() %>%
                        bake(new_data = training_data)
                
                extra_vars  = 
                        map(c("ids", "outcomes"), 
                            ~ workflow_objs %>% 
                                    pluck(.x)) %>% 
                        unlist()
                
                training_matrix = 
                        baked_training_data %>%
                        select(-any_of(extra_vars)) %>%
                        as.matrix()
                
                feature_names = 
                        colnames(training_matrix)
                
                tibble(workflow_objs,
                       training = list(training_data),
                       training_matrix = list(training_matrix),
                       feature_names = list(feature_names)
                ) %>%
                        restore_feature_names()
        }

unbundle_workflow = 
        function(vetiver_model) {
                
                vetiver_model$model %>%
                        bundle::unbundle()
        }

extract_workflow_objects = function(workflow) {
        
        mold = 
                workflow %>% 
                extract_mold() 
        
        ids =        
                map(c("id", "extra"),
                    ~ mold$blueprint$extra_role_ptypes %>%
                            pluck(.x) %>%
                            names()) %>%
                unlist()
        
        predictors = 
                mold$blue$ptypes$predictors %>%
                names()
        
        outcomes =
                mold$blueprint$ptypes$outcomes %>% 
                names()
        
        tibble("workflow" = list(workflow),
               "outcomes" = list(outcomes),
               "predictors" = list(predictors),
               "ids" = list(ids))
        
}


extract_training_data = 
        function(vetiver_model) {
                
                vetiver_model$metadata$user %>%
                        bind_cols()
                
        }

restore_training_data = 
        function(data,
                 vars_to_datetime = c("training_ts", "load_ts"),
                 vars_to_factor = c("users_threshold")) {
                
                data %>%
                        mutate(
                                across(
                                        all_of(vars_to_datetime),
                                        ~ as.POSIXct(.)
                                )
                        ) %>%
                        mutate(
                                across(
                                        all_of(vars_to_factor),
                                        ~ as.factor(.)
                                ),
                        )
                
        }

restore_feature_names = function(data,
                                 workflow,
                                 feature_names) {
        
        
        add_feature_names = function(workflow,
                                     feature_names)
        { 
                
                workflow$fit$fit$fit$feature_names = feature_names
                
                workflow
        }
        
        data %>%
                mutate(workflow = 
                               map2(workflow,
                                    feature_names,
                                    ~ add_feature_names(.x, .y)))
}

prep_newdata = function(workflow_objs,
                        newdata,
                        ids = c("game_id", "name", "yearpublished")) {
        
        newdata_ids = 
                newdata %>%
                select(any_of(ids))
        
        newdata_baked = 
                workflow_objs %>%
                pluck("workflow", 1) %>%
                extract_recipe() %>%
                bake(new_data = newdata)
        
        newdata_baked$game_id = newdata$game_id
        newdata_baked$name = newdata$name
        newdata_baked$description = newdata$description
        
        newdata_matrix = 
                newdata_baked %>%
                select(
                        -any_of(
                                c(
                                        workflow_objs %>% 
                                                pluck("ids", 1),
                                        workflow_objs %>% 
                                                pluck("outcomes", 1)
                                )
                        )
                ) %>%
                as.matrix()
        
        return(list("ids" = newdata_ids,
                    "outcomes" = workflow_objs %>% 
                            pluck("outcomes", 1),
                    "baked" = newdata_baked,
                    "mat" = newdata_matrix))
        
}


calculate_vip = function(workflow_objects) {
        
        workflow_objects %>%
                pluck("workflow", 1) %>%
                extract_fit_engine() %>%
                vip::vip()
        
}


explain_prediction = function(workflow_objects,
                              new_data,
                              ids = c("game_id", "name", "yearpublished")) {
        
        newdata_objects = 
                prep_newdata(
                        workflow_objects,
                        newdata = new_data,
                        ids = ids
                )
        
        outcome = 
                workflow_objects %>%
                pluck("outcomes", 1)
        
        newdata_actual = 
                new_data %>%
                select(any_of(ids), any_of(outcome)) %>%
                rename(.actual = any_of(outcome))
        
        shap = 
                fastshap::explain(
                        object = workflow_objects %>%
                                pluck("workflow", 1) %>%
                                extract_fit_engine(),
                        feature_names =
                                workflow_objects %>%
                                pluck("feature_names", 1),
                        X = newdata_objects$mat,
                        pred_wrapper = predict,
                        exact = T
                )
        
        shap_long = 
                shap_longer(
                        shap,
                        newdata_objects
                )
        
        newdata_long = 
                newdata_longer(
                        newdata_objects
                )
        
        shap_explained = 
                shap_long %>%
                left_join(.,
                          newdata_long,
                          by = c(ids, "feature")
                ) %>%
                mutate(feature_value = paste(feature,
                                             "=",
                                             round(value, 2))
                ) %>%
                mutate(outcome = workflow_objects %>%
                               pluck("outcomes", 1)) %>%
                select(all_of(ids),
                       outcome,
                       feature,
                       value,
                       feature_value,
                       contribution)
        
        newdata_prediction = 
                workflow_objects %>%
                pluck("workflow", 1) %>%
                augment(new_data) %>%
                select(any_of(ids), starts_with(".pred"))
        
        list("shap" = 
                     shap,
             "shap_explained" = 
                     shap_explained,
             "prediction" = 
                     newdata_prediction,
             "actual" = 
                     newdata_actual,
             "ids" = 
                     ids
        )
        
}

# pivot shap results and get actual
shap_longer = function(shap,
                       newdata_objs) {
        
        # id cols to vind in 
        id_cols = newdata_objs$ids
        
        # bind shapley values with id cols
        bind_cols(id_cols,
                  as.data.frame(shap)) %>%
                pivot_longer(cols = -c(names(id_cols)),
                             values_to = 'contribution',
                             names_to = 'feature')
        
}

# pivot newdata baked results
newdata_longer = function(newdata_objs) { 
        
        
        # id cols to vind in 
        id_cols = newdata_objs$ids
        
        # bind shapley values with id cols
        bind_cols(id_cols,
                  newdata_objs$mat) %>%
                pivot_longer(cols = -c(names(id_cols)),
                             values_to = 'value',
                             names_to = 'feature')
}

plot_explain = 
        function(prepped_explain,
                 order_by ='pos',
                 width = 30) {
                
                plot = 
                        
                if (order_by == 'abs') {

                        prepped_explain %>%
                                ggplot(.,
                                       aes(x=contribution,
                                           fill = contribution,
                                           y=tidytext::reorder_within(feature_value, abs(contribution), outcome)))
                } else {
                        
                        prepped_explain %>%
                        ggplot(.,
                               aes(x=contribution,
                                   fill = contribution,
                                   y=tidytext::reorder_within(feature_value, contribution, outcome)))
                
                }
                
                plot +
                        geom_col()+
                        tidytext::scale_y_reordered()+
                        geom_vline(xintercept = 0)+
                        # set theme
                        theme_minimal()+
                        theme(strip.text.x = element_text(size = 10))+
                        facet_wrap(
                                prediction_label ~.,
                                scales = "free",
                                ncol = 2
                        )+
                        # set the color
                        scale_fill_gradient2(low = "red",
                                             mid = "grey80",
                                             high = "blue",
                                             midpoint = 0,
                                             limits = c(-0.2, 0.2),
                                             oob = scales::squish)+
                        guides(fill ="none")+
                        ylab("Feature")+
                        xlab("Shapley Value")
        }

add_plot_explain_title = 
        function(explain_plot,
                 width = 30) {
                
                plot_data = 
                        explain_plot$data
                
                tidy_name = 
                        stringr::str_trunc(unique(plot_data$name), width)
                
                tidy_id = 
                        unique(plot_data$game_id)
                
                explain_plot+
                        labs(
                                title = 
                                        paste(
                                                paste("Game:", tidy_name),
                                                paste("ID:", tidy_id),
                                                sep = "\n"
                                        )
                        )+
                        theme(
                                plot.title = element_text(size = 12),
                                plot.subtitle = element_text(size = 0)
                                
                        )
        }

prep_plot_explain = 
        function(explain_prediction_objects,
                 top_n = 25) {
                
                ids = explain_prediction_objects$ids
                
                make_tidy_outcome = function(data) {
                        
                        data %>%
                                mutate(tidy_outcome = case_when(outcome == 'averageweight' ~ 'Average Weight',
                                                                outcome == 'log_usersrated' ~ 'User Ratings',
                                                                outcome == 'average' ~ 'Average Rating',
                                                                outcome == 'bayesaverage' ~ 'Geek Rating'))
                        
                }
                
                get_shap_contributions = function(shap_explained,
                                                  ...) {
                        
                        top_features = 
                                shap_explained %>%
                                group_by(across(c(any_of(ids), outcome))) %>%
                                slice_max(order_by = abs(contribution),
                                          n = top_n,
                                          with_ties = F) %>%
                                pull(feature)
                        
                        
                        features_label = 
                                'all_other_features'
                        
                        shap_ids = 
                                shap_explained %>%
                                mutate(feature = 
                                               case_when(feature %in% top_features ~ feature,
                                                         TRUE ~ features_label)
                                )
                        
                        shap_contributions = 
                                shap_ids %>%
                                group_by(across(c(any_of(ids), outcome, feature))) %>%
                                summarize(contribution = sum(contribution),
                                          .groups = 'drop')
                        
                        shap_ids %>%
                                inner_join(
                                        shap_contributions,
                                        by = join_by(game_id, name, yearpublished, outcome, feature, contribution)
                                ) %>%
                                bind_rows(
                                        shap_contributions %>%
                                                filter(feature == features_label)
                                )
                        
                }
                
                add_prediction_label = function(explain_prediction_objects,
                                                width = 30,
                                                ...) {
                        
                        prediction_label_data =
                                explain_prediction_objects$shap_explained %>%
                                left_join(.,
                                          explain_prediction_objects$prediction,
                                          by = join_by(game_id, name, yearpublished)) %>%
                                left_join(.,
                                          explain_prediction_objects$actual,
                                          by = join_by(game_id, name, yearpublished)) %>%
                                mutate(tidy_outcome = case_when(outcome == 'averageweight' ~ 'Average Weight',
                                                                outcome == 'log_usersrated' ~ 'User Ratings',
                                                                outcome == 'average' ~ 'Average Rating',
                                                                outcome == 'bayesaverage' ~ 'Geek Rating')) %>%
                                mutate(tidy_name = stringr::str_trunc(name, width))
                        
                        outcome = 
                                unique(explain_prediction_objects$shap_explained$outcome)
                        
                        if (outcome == 'log_usersrated') {
                                prediction_label_data =
                                        prediction_label_data %>%
                                        mutate(.pred = plyr::round_any(exp(.pred), 50),
                                               .actual = plyr::round_any(exp(.actual), 50))
                        } else {
                                
                                prediction_label_data = 
                                        prediction_label_data %>%
                                        mutate(.pred = round(.pred, 2),
                                               .actual = round(.actual, 2)
                                        )
                        }
                        
                        prediction_label_data %>%
                                mutate(
                                        prediction_label =
                                                paste(
                                                        # paste("Game:", tidy_name),
                                                        # paste("ID:", game_id),
                                                        paste(tidy_outcome),
                                                        paste("Prediction:", .pred),
                                                        paste("Current:", .actual),
                                                        sep = "\n"
                                                )
                                ) %>%
                                select(any_of(ids), prediction_label) %>%
                                distinct()
                        
                }
                
                shap_labels = 
                        explain_prediction_objects %>%
                        add_prediction_label()
                
                shap_contributions =
                        explain_prediction_objects %$%
                        # add_prediction_label()
                        shap_explained %>%
                        split(~game_id) %>%
                        map(
                                ~ .x %>%
                                        get_shap_contributions()
                        ) %>%
                        bind_rows() %>%
                        left_join(.,
                                  shap_labels
                        )
                
                shap_contributions %>%
                        mutate(feature =
                                       stringr::str_trunc(bggUtils::present_text(feature, minlength=100), 30),
                               feature_value =
                                       case_when(feature == 'All Other Features' ~ feature,
                                                 TRUE ~ paste(feature, round(value, 2), sep = " = ")
                                       )
                        )
                
        }


make_tidy_outcome = function(x) {
        
        
        if (x == 'averageweight') {'Complexity'} 
        else if (x == 'usersrated') {'Ratings'}
        else if (x == 'average') {'Average'}
        
        
}