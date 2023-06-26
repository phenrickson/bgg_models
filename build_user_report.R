# visualizations for user models
source(here::here("src", "visualizations", "viz_user_models.R"))
source(here::here("src", "features", "preprocess_games.R"))



# collection --------------------------------------------------------------



my_caption = function(data = games) {
        
        list(
                labs(
                        caption = paste(
                                paste("data from boardgamegeek.com as of", max(as.Date(data$load_ts[1]))),
                                paste("analysis by Phil Henrickson"),
                                paste("https://phenrickson.github.io/data-analysis-paralysis/"),
                                sep="\n")
                )
        )
        
}


# pull game info
games_info = 
        pins::pin_read(
                board = pins::board_folder(here::here("data", "processed")),
                name = "games"
        )


collection = 
        user_output$user_collection %>%
        prep_collection() %>%
        left_join(games_info,
                  by = join_by(game_id, name)) %>%
        filter(game_id %in% games$game_id)

# helper function to get username
get_username = function(data = collection) {
        
        collection$username[1]
}


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
                my_caption+
                ggtitle(paste("Top", 
                              stringr::str_to_title(paste(vars, collapse = ", ")),
                              "for",
                              get_username(data)))+
                theme(plot.title = element_text(size =14))+
                scale_fill_brewer(palette = "Set1")+
                guides(fill = 'none')
        
}

# collection %>%
# collection_categories_plot()


collection_owned_plot = function(data,
                                 vars = c("own", "prevowned", "rated", "preordered", "wanttoplay")) {
        
        data %>%
                select(game_id, any_of(vars)) %>%
                mutate(across(vars,
                              ~ case_when(.x == 1 | .x == 'yes' ~ 'yes',
                                          .x == 0 | .x == 'no' ~ 'no'))) %>%
                mutate(unplayed = case_when(
                        (own == 'yes' | prevowned == 'yes' ) & rated == 'no' ~ 'yes')) %>%
                pivot_longer(cols = -c(game_id)) %>%
                filter(value == 'yes') %>%
                group_by(name) %>%
                summarize(n = sum(value == 'yes')) %>%
                mutate(label = case_when(name == 'unplayed' ~ 'yes',
                                         TRUE ~ 'no')) %>%
                ggplot(aes(x=n,
                           fill = label,
                           label = n,
                           y = reorder(name, n)))+
                geom_col()+
                geom_text(hjust = -0.1)+
                theme_bgg()+
                ggtitle(paste(get_username(data), "'s Collection", sep=""),
                        subtitle = str_wrap('Unplayed games are games the user has owned but not rated, potentially representing a shelf of shame. In this event, these games are highlighted in red in order to (potentially) make the user feel bad about themselves.',120))+
                my_caption+
                scale_fill_manual(values = c("grey60", "firebrick3"))+
                guides(fill = "none")+
                ylab("")+
                xlab("number of games")+
                theme(plot.title = element_text(hjust = 0, 
                                                size = 16), 
                      plot.subtitle = element_text(hjust = 0,
                                                   size = 8))
        
}

# collection %>%
#         collection_owned_plot()

# table
make_top_n_table = function(preds,
                            collection,
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
                mutate(rank = row_number()) %>%
                select(game_id, name, any_of(outcome), rank, yearpublished)  %>%
                ungroup()
        
        highlight = preds %>%
                filter(!!rlang::sym(outcome) == event_class) %>%
                pull(name)
        
        table = preds %>%
                select(name, yearpublished, rank) %>%
                pivot_wider(id_cols = c("rank"),
                            names_from = c("yearpublished"),
                            values_from = c("name"),
                            values_fn = list)
        
        table %>%
                select(rank, sort(names(.))) %>%
                unnest(cols = names(.)) %>%
                gt::gt() %>%
                gt::tab_options(table.font.size = 10) %>%
                gt::cols_align(align = c("center")) %>%
                gt::data_color(
                        columns = everything(),
                        method = "factor",
                        na_color = "white",
                        autocolor_text = T,
                        fn = function(x) case_when(x %in% highlight ~ 'dodgerblue2',
                                                   TRUE ~ 'white')
                )
        
}

# # # make top 10 table
make_top_n_table(
        user_output$training_predictions,
        workflow = 'all_splines_glmnet',
        top_n = 25,
        outcome = "ever_owned"
)

# make top 10 table
make_top_n_table(
        user_output$training_predictions,
        workflow = 'all_trees_lightgbm',
        top_n = 5,
        outcome = "own"
)

make_game_gt_table = function(predictions,
                              outcome,
                              game_data = games,
                              top_n = 10,
                              description_length = 400,
                              ...) {
        
        require(gt)
        require(gtExtras)
        
        make_bgg_link = function(game_id) {
                
                paste0("https://boardgamegeek.com/boardgame/",
                       game_id)
        }
        
        make_web_image =
                function(x, height=100) {
                        
                        web_image(url = x,
                                  height = px(height) 
                        ) 
                        
                }
        
        make_hyperlink = function(myurl,
                                  mytext=myurl) {
                paste('<a href="',myurl,'">',mytext,'</a>')
        }
        
        add_game_image = function(data,
                                  filter = T,
                                  ...) {
                
                if (filter == T) {
                        
                        data %>%
                                left_join(., game_data %>%
                                                  select(game_id, image),
                                          by = c("game_id")) %>%
                                filter(!is.na(image))
                        
                } 
                else {
                        
                        data %>%
                                left_join(., game_data %>%
                                                  select(game_id, image),
                                          by = c("game_id"))
                }
                
                
        }
        
        add_game_description = function(data,
                                        ...) {
                
                data %>%
                        left_join(., game_data %>%
                                          select(game_id, description),
                                  by = c("game_id"))
                
        }
        
        tidy_outcome = outcome %>%
                stringr::str_replace(., "_",
                                     " ") %>%
                stringr::str_to_title(.)
        
        prob_tidy_outcome = 
                paste0("Pr(", tidy_outcome, ")")
        
        
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
                gtExtras::gt_theme_espn() %>%
                tab_options(table.font.size = 12)
        
}


# bind_rows(user_output$training_predictions,
#         user_output$valid_predictions) %>%
#   filter(yearpublished < year(Sys.Date())) %>%


library(gt)

bind_rows(
        user_output$valid_predictions,
        user_output$upcoming_predictions %>%
                filter(yearpublished > 2023)) %>%
        filter(wflow_id == 'all_trees_lightgbm') %>%
        make_game_gt_table(
                outcome = 'own',
                top_n = 25,
                description_length = 500
        ) %>%
        tab_header(
                title = paste("Top (Newer) Games for",
                              user_output$user_collection$username[1]),
                subtitle = paste("Rankings based on predictive model trained on user's collection using games released through", 
                                 user_output$end_train_year)
        )

bind_rows(user_output$training_predictions) %>%
        filter(wflow_id == 'all_trees_lightgbm') %>%
        make_game_gt_table(
                outcome = 'own',
                top_n = 25,
                description_length = 500
        ) %>%
        tab_header(
                title = paste("Top (Older) Games for",
                              user_output$user_collection$username[1]),
                subtitle = paste("Rankings based on predictive model trained on user's collection using games released through", 
                                 user_output$end_train_year)
        )



# assessment -------------------------------------------------------------

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

# make 
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
                scale_color_brewer(palette = "Set1")
}

user_output$training_predictions %>%
        make_roc_plot(truth = 'own')

user_output$valid_predictions %>%
        make_roc_plot(truth = 'own')


user_output$training_predictions %>%
        make_separation_plot(truth = 'own')


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

library(probably)
library(ggridges)

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

user_output$training_predictions %>%
        filter(wflow_id == 'all_trees_lightgbm') %>%
        cal_plot_breaks(
                group = wflow_id,
                truth = own,
                estimate = .pred_yes,
                event_level = 'second')+
        facet_wrap(wflow_id ~.)

user_output$valid_predictions %>%
        group_by(wflow_id) %>%
        yardstick::roc_curve(
                truth = user_output$outcome,
                .pred_yes,
                event_level = 'second'
        ) %>%
        autoplot(lwd = 1.04)+
        theme_bgg()

eval_threshold_plot = function(
                predictions,
                thresholds = seq(0, 1, by = 0.05),
                truth,
                event_level = 'yes') {
        
        map_df(thresholds,
               .f = ~ predictions %>%
                       mutate(!!rlang::sym(truth) := factor(!!rlang::sym(truth),
                                                            levels = c("yes", "no"))) %>%
                       mutate(
                               .pred = make_two_class_pred(
                                       estimate = .pred_yes, 
                                       levels = levels(!!truth),
                                       threshold = .x)
                       ) %>%
                       group_by(wflow_id) %>%
                       yardstick::kap(
                               truth = !!truth,
                               estimate = .pred,
                               event_level =) %>%
                       mutate(threshold = .x)) %>%
                ggplot(aes(x=threshold,
                           color = wflow_id,
                           y=.estimate))+
                # geom_point()+
                geom_line(alpha = 0.8)+
                facet_grid(.metric~.)+
                theme_minimal()
        
}


user_output$training_predictions %>%
        eval_threshold_plot(
                truth = 'own'
        )

thresholds = seq(0, 1, by = 0.05)

map_df(thresholds,
       .f = ~user_output$training_predictions %>%
               mutate(ever_owned = factor(ever_owned,
                                          levels = c("yes", "no"))) %>%
               mutate(
                       .pred = make_two_class_pred(
                               estimate = .pred_yes, 
                               levels = levels(ever_owned),
                               threshold = .x)
               ) %>%
               group_by(wflow_id) %>%
               yardstick::kap(
                       truth = ever_owned,
                       estimate = .pred,
                       event_level =) %>%
               mutate(threshold = .x)
) %>%
        ggplot(aes(x=threshold,
                   color = wflow_id,
                   y=.estimate))+
        # geom_point()+
        geom_line(alpha = 0.8)+
        facet_grid(.metric~.)+
        theme_minimal()

# separation chart



# glmnet plots
# coef plot
user_workflows %>%
        get_workflow(model = "glmnet") %>%
        glmnet_coef_plot()+
        ggtitle(paste("What Predicts", get_username(collection), "'s Collection?", sep=" "),
                subtitle = str_wrap("Coefficients from a penalized (ridge) logistic regression for games owned by specified user. Predictors centered and scaled."))+
        theme(plot.title = element_text(hjust = 0, 
                                        size = 16), 
              plot.subtitle = element_text(hjust = 0,
                                           size = 10))+
        my_caption+
        xlab("Estimated Effect on Outcome")+
        ylab("Features")


# trace plot
user_workflows %>%
        get_workflow(model = "glmnet") %>%
        glmnet_trace_plot()

# lightgbm plots
# variable importance
user_workflows %>%
        get_workflow(model = "lightgbm") %>%
        lightgbm_vip() %>%
        lightgbm_vip_plot()+
        ggtitle(paste("What Predicts", get_username(collection), "'s Collection?", sep=" "),
                subtitle = str_wrap("Feature importance from light gradient boosted machine, an ensemble of boosted decision trees. Features that are important in predicting a user's collection will appear towards the top of cover, frequency, and/or gain."))+
        theme(plot.title = element_text(hjust = 0, 
                                        size = 16), 
              plot.subtitle = element_text(hjust = 0,
                                           size = 10))+
        my_caption+
        xlab("Value")+
        ylab("Feature")+
        theme(panel.grid.major = element_blank())

# partial dependence

model = 
        user_workflows %>%
        get_workflow(model = "lightgbm") %>%
        extract_fit_engine()

training =
        user_workflows %>%
        get_workflow(model = "lightgbm") %>%
        extract_mold() %$%
        predictors

var = 'number_mechanics'

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

user_workflows %>%
        get_workflow(model = 'lightgbm') %>%
        make_partial_plot(var = 'number_mechanics',
                          length = 10,
                          seed = 5)

user_workflows %>%
        get_workflow(model = 'lightgbm') %>%
        make_partial_plot(var = 'est_averageweight',
                          seed = 5)

user_workflows %>%
        get_workflow(model = 'lightgbm') %>%
        make_partial_plot(var = 'word_count',
                          seed = 5)

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

get_workflow_predictors = function(workflow) {
        
        workflow %>%
                extract_mold() %$%
                predictors %>%
                names()
        
}

data = user_output$user_collection %>%
        join_bgg_games(games)

# lightgbm_predict = 
#         function(workflow = user_workflows %>%
#                          get_workflow(model = 'lightgbm'),
#                  data,
#                  ...) {
#                 
#                 if(missing(game_id))
#                         
# 
#                 workflow %>%
#                         augment(data,
#                                 type = 'prob') %>%
#                         select(game_id, name, .pred_yes, ever_owned)
#         }

# interpret
lightgbm_interpret = function(workflow = user_workflows %>%
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
                                       paste0(name, " ", "(", game_id, ")"),
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
                                        size = 16),
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

#cowplot::plot_grid(plotlist=plots)

# # plot natural splines mechanics
# glmnet_workflow %>%
#         extract_mold() %$%
#         predictors %>% 
#         select(contains("number_mechanics")) %>%
#         bind_cols(.,
#                   lightgbm_workflow %>% 
#                           extract_mold() %$% 
#                           predictors %>%
#                           select(number_mechanics)
#         ) %>%
#         pivot_longer(c(starts_with("number_mechanics_ns_")),
#                      names_to = "feature",
#                      values_to = "value") %>%
#         mutate(feature = gsub("number_mechanics_", "", feature)) %>%
#         ggplot(aes(x = number_mechanics, y = value)) +
#         geom_line() +
#         facet_wrap(~ feature)
# 
# # plot natural splines est_averageweight
# glmnet_workflow %>%
#         extract_mold() %$%
#         predictors %>% 
#         select(contains("est_averageweight")) %>%
#         bind_cols(.,
#                   lightgbm_workflow %>% 
#                           extract_mold() %$% 
#                           predictors %>%
#                           select(est_averageweight)
#         ) %>%
#         pivot_longer(c(starts_with("est_averageweight_ns_")),
#                      names_to = "feature",
#                      values_to = "value") %>%
#         mutate(feature = gsub("est_averageweight_", "", feature)) %>%
#         ggplot(aes(x = est_averageweight, y = value)) +
#         geom_line() +
#         facet_wrap(~ feature)
