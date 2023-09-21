
# setup -------------------------------------------------------------------

Sys.setenv(TAR_PROJECT = "project_data")

# not run
targets::tar_load_globals()

# source
source(here::here("src", "models", "train_outcomes.R"))
source(here::here("src", "features", "recipes_outcomes.R"))
source(here::here("src", "models", "neighbors.R"))
source(here::here("src", "reports", "user_collection_report.R"))
source(here::here("src", "reports", "outcome_reports.R"))
source(here::here("src", "reports", "game_reports.R"))
source(here::here("src", "reports", "knitr_settings.R"))

# additional packages
library(rdist)
library(plotly)
library(tidytext)
library(shiny)
library(shinyWidgets)

# tables
library(gt)
library(gtExtras)

# data --------------------------------------------------------------------

tar_load(
        games_imputed
)

tar_load(
        end_train_year
)

tar_load(
        games
)

tar_load(
        games_predicted
)


# models ------------------------------------------------------------------

# load models
explain_average =
        vetiver::vetiver_pin_read(
                board = get_gcs_board('deployed'),
                name = "vetiver_average_"
        ) %>%
        extract_vetiver_workflow_objects()

explain_averageweight =
        vetiver::vetiver_pin_read(
                board = get_gcs_board('deployed'),
                name = "vetiver_averageweight_"
        ) %>%
        extract_vetiver_workflow_objects()

explain_usersrated =
        vetiver::vetiver_pin_read(
                board = get_gcs_board('deployed'),
                name = "vetiver_usersrated"
        ) %>%
        extract_vetiver_workflow_objects()

# recipes -----------------------------------------------------------------


# training
training = 
        games_imputed %>%
        filter(yearpublished <= end_train_year) %>%
        filter(!is.na(bayesaverage)) %>%
        arrange(desc(bayesaverage))

# games published after end of training year
upcoming =
        games_imputed %>%
        filter(yearpublished > end_train_year) %>%
        left_join(.,
                  games_predicted %>%
                          select(game_id, starts_with(".pred"))) %>%
        arrange(desc(.pred_bayesaverage))

# make recipe
compare_recipe = 
        training %>%
        filter(!is.na(bayesaverage)) %>%
        make_recipe(
                ids = id_vars(),
                predictors = c("minage",
                               "minplayers",
                               "maxplayers",
                               "playingtime",
                               "minplaytime",
                               "maxplaytime",
                               "mechanics",
                               "categories",
                               "families",
                               "est_averageweight")
        ) %>%
        # add dummies
        # mechanics at min 50
        add_dummies(mechanics,
                    threshold = 50) %>%
        # # families at min 100
        add_dummies(families,
                    threshold = 100) %>%
        # include all categories
        add_dummies(categories,
                    threshold = 1) %>%
        add_preprocessing() %>%
        add_imputation() %>%
        add_zv() %>% 
        #   add_splines(vars = c("number_mechanics")) %>%
        add_normalize() %>%
        step_rm(
                starts_with("families_"),
                -starts_with("families_collectible_collectible_card_games"),
                -starts_with("families_category"),
                -starts_with("families_components_block_wargames"),
                -starts_with("families_components_dice_with_icons"),
                -starts_with("families_components_map_continental_national_scale"),
                -starts_with("families_components_marbles"),
                -starts_with("families_components_miniatures"),
                -starts_with("families_components_polyominoes"),
                -starts_with("families_components_traditional_playing_cards"),
                -starts_with("families_components_wooden_pieces_boards"),
                -starts_with("families_mechanis"),
                -starts_with("families_players"),
                -starts_with("families_word_games_spelling_letters")
        ) %>%
        step_rm(
                any_of(
                        c(
                                "word_count",
                                "year",
                                "published_before_1900",
                                "description_from_publisher",
                                "deluxe_edition",
                                "number_categories",
                                paste(
                                        "categories",
                                        c(
                                                "age_of_reason",
                                                "american_civil_war",
                                                "american_west",
                                                "book",
                                                "civil_war",
                                                "comic_book_strip",
                                                "electronic",
                                                "environmental",
                                                "fantasy",
                                                "farming",
                                                "industry_manufacturing",
                                                "mafia",
                                                "medieval",
                                                "modern_warfare",
                                                "murder_mystery",
                                                "mythology",
                                                "napoleonic",
                                                "nautical",
                                                "novel_based",
                                                "pike_and_shot",
                                                "pirates",
                                                "political",
                                                "post_napoleonic",
                                                "prehistoric",
                                                "racing",
                                                "religious",
                                                "renaissance",
                                                "science_fiction",
                                                #     "space_exploration",
                                                "spies_secret_agents",
                                                #   "travel",
                                                "movies_tv_radio_theme",
                                                "video_game_theme",
                                                "game_system",
                                                "expansion_for_base_game",
                                                "fan_expansion",
                                                "korean_war",
                                                "vietnam_war",
                                                "american_indian_wars",
                                                "american_revolutionary_war",
                                                "ancient",
                                                "arabian",
                                                "medical",
                                                "world_war_i",
                                                "world_war_ii",
                                                "zombies"
                                        ),
                                        sep = "_"
                                )
                        )
                )
        ) %>%
        prep()

# recipe to get raw values
compare_games_raw_recipe = 
        training %>%
        filter(!is.na(bayesaverage)) %>%
        make_recipe(
                ids = id_vars(),
                predictors = c("minage",
                               "minplayers",
                               "maxplayers",
                               "playingtime",
                               "minplaytime",
                               "maxplaytime",
                               "mechanics",
                               "categories",
                               "families",
                               "est_averageweight")
        ) %>%
        # add dummies
        # mechanics at min 50
        add_dummies(mechanics,
                    threshold = 50) %>%
        # # families at min 100
        add_dummies(families,
                    threshold = 100) %>%
        # include all categories
        add_dummies(categories,
                    threshold = 1) %>%
        add_preprocessing() %>%
        add_imputation() %>%
        add_zv() %>%
        prep(retain = T)

# compare games with their raw, non normalized values
compare_games_raw =
        compare_games_raw_recipe %>%
        bake(
                bind_rows(training,
                          upcoming
                )
        ) %>%
        select(-name) %>%
        # add in names
        left_join(.,
                  games_imputed %>%
                          select(game_id, name)
                  ) %>%
        select(game_id, name, yearpublished, everything())

# features to use in comparing
compare_features =
        compare_recipe %$%
        last_term_info %>%
        unnest(role) %>%
        filter(role == 'predictor') %>%
        pull(variable)

# bake with recipe
compare_games = 
        compare_recipe %>%
        bake(
                bind_rows(training,
                          upcoming
                )
        ) %>%
        select(-name) %>%
        # add in names
        left_join(.,
                  games_imputed %>%
                          select(game_id, name)
        ) %>%
        select(game_id, name, yearpublished, any_of(compare_features))

# compare PCA
compare_recipe_pca =
        compare_recipe %>%
        add_normalize() %>%
        add_pca(
                threshold = 0.50,
                keep_original_cols = F,
                id = 'pca') %>%
        prep(retain = T)

# bake with PCA
compare_games_pca = 
        compare_recipe_pca %>%
        bake(
                bind_rows(training,
                          upcoming
                )
        ) %>%
        select(-name) %>%
        # add in names
        left_join(.,
                  games_imputed %>%
                          select(game_id, name)
        ) %>%
        select(game_id, name, yearpublished, starts_with("PC"))

# run game report
rmarkdown::run(here::here("shiny", "build_game_report.Rmd"), shiny_args = list(port = 8241))

# game_neighbors =
#         compare_games %>%
#         find_game_neighbors(
#                 game = 'Brass: Birmingham',
#                 metric = 'canberra'
#         )
#         
# game_neighbors %>%
#         make_neighbors_rated_gt_table(
#                 games = games_imputed %>%
#                         left_join(.,
#                                   games %>%
#                                           select(game_id, images))
#         )
# 
# # 
# # 
# # contributors = 
# #         find_neighbor_contributors(
# #                 compare_games_pca,
# #                 neighbors$neighbor_id[1:10],
# #                 neighbors_heatmap)
# # 
# # top_components =
# #         c(contributors %>% head(3), contributors %>% tail(3))
# 
# 
# 
# 
# # df = 
# #         compare_games_pca %>%
# #         add_id()
# #         # filter(game_id %in% (compare_games %>% head(50) %>% pull(game_id))) %>%
# #         #   sample_n(50) %>%
# #         filter(game_id %in% neighbors$neighbor_id)
# # 
# # mat = df %>%
# #         mutate(id = str_trunc(paste(name, game_id), width = 50))%>%
# #         column_to_rownames("id") %>%
# #         select(starts_with("PC")) %>%
# #         as.matrix()
# # 
# # clust = 
# #         mat %>%
# #         head(15) %>%
# #         rdist(metric = 'canberra') %>%
# #         fastcluster::hclust(method = 'median')
# 
# # foo = 
# #         mat %>%
# #         head(10) %>%
# #         heatmap3::heatmap3(
# #                 #  heatmap3::heatmap3(col = pal,
# #                 cexRow = 0.75,
# #                 cexCol = 0.5,
# #                 margins = c(3, 7),
# #              #   col = pal,
# #                 revC = F
# #                 #    Rowv = NA
# #         )
# 
# # top_components =
# #         c(contributors %>% head(2), contributors %>% tail(2)) %>%
# #         gsub("^PC0", "PC",. )
# 
# # top_components = c("PC4", "PC")
# 
# compare_recipe_pca %>%
#         tidy(id = 'pca') %>%
#         filter(component %in% (top_components %>% gsub("^PC0", "PC",. ))) %>%
#         mutate(component = factor(component, levels = top_components)) %>%
#         group_by(component) %>%
#         slice_max(order_by = abs(value), n =15, with_ties = F) %>%
#         mutate(terms = bggUtils::present_text(terms, minlength = 40)) %>%
#         ggplot(aes(x=abs(value),
#                    fill = value,
#                    y=reorder_within(terms, abs(value), component)))+
#         geom_col()+
#         facet_wrap(component ~.,
#                    scales = "free",
#                    ncol = 2
#         )+
#         scale_y_reordered()+
#         theme_bgg()+
#         ylab("")+
#         guides(fill = guide_colorbar(barwidth = 10,
#                                      barheight = 0.5))+
#         scale_fill_distiller(palette = "RdBu",
#                              direction = 1)+
#         guides(fill = 'none')
# 
# compare_games_pca %>%
#         select(game_id, name, yearpublished,
#                any_of(top_components)) %>%
#         mutate_if(is.numeric, round, 3) %>%
#         view()
# pivot_longer(
#         cols = -c(game_id, name, yearpublished),
#         values_to = c("value"),
#         names_to = c("component")
# ) %>%
#         add_id() %>%
#         
#         ggplot(aes(x=component,
#                    y=value,
#                    label = name,
#                    by = id))+
#         geom_point(
#                 alpha = 0.5,
#                 position = ggforce::position_jitternormal(sd_x = 0.05)
#         )+
#         ggrepel::geom_text_repel()
# 
# 
# 
# 
# variable_contributors = 
#         compare_recipe_pca %>%
#         tidy(id = 'pca') %>%
#         filter(component %in% top_components) %>%
#         group_by(component) %>%
#         slice_max(n = 10,
#                   order_by = abs(value),
#                   with_ties = F)
# 
# library(plotly)
# compare_games %>%
#         filter(game_id %in% neighbors$neighbor_id[1:25]) %>%
#         select(game_id, name, yearpublished,
#                any_of(
#                        variable_contributors %>%
#                                filter(component == top_components[2]) %>%
#                                pull(terms))
#         ) %>%
#         pivot_longer(cols = -c(name, game_id, yearpublished),
#                      names_to = c("component"),
#                      values_to = c("value")) %>%
#         mutate(component = bggUtils::present_text(component)) %>%
#         group_by(component) %>%
#         summarize(var = var(value)) %>%
#         arrange(var) %>%
#         print(n = 25)
# ggplot(aes(x=value,
#            y=component,
#            group = paste(name, game_id),
#            color = paste(name, game_id)))+
#         geom_jitter(height = 0.1)
# # mutate(id = str_trunc(paste(name, game_id), width = 50)) %>%
# # mutate(id = factor(id, levels = neighbors$id)) %>%
# # mutate(width = case_when(id == neighbors$id[1] ~ 1,
# #                          TRUE ~ 1)) %>%
# ggplot(aes(x=component,
#            y=value,
#            group = id,
#            color = id))+
#         geom_line()+
#         theme_bgg()+
#         scale_color_viridis_d(option = 'B')+
#         theme(axis.text.x = element_text(size = 6,
#                                          angle = 90))+
#         xlab("")+
#         ylab("")+
#         guides(linewidth = "none",
#                alpha = "none")
# 
# foo$colInd
# 
# 
# theme(axis.text.y = element_text(size = 4))
# 
# compare_games %>%        
#         filter(game_id %in% neighbors$neighbor_id) %>%
#         mutate(id = str_trunc(paste(name, game_id), width = 35)) %>%
#         select(id)
# filter(is.na(id))
# select(-game_id, -name, -yearpublished) %>%
#         column_to_rownames("id") %>%
#         as.matrix() %>%
#         heatmap()
# 
# 
# map_df(
#         seq(1, 2),
#         ~ apply(mat[c(1,.x),], 2, diff) %>%
#                 abs()) %>%
#         colSums() %>%
#         sort()
# 
# 
# 
# 
# 
# bind_cols(df %>%
#                   select(game_id, name, yearpublished),
#           .)
# 
# 
# 
# 
# mat[1,]
# 
# 
# dist = 
#         mat %>%
#         rdist(metric = 'angular')
# 
# 
# hclust = 
#         dist %>%
#         hclust(method = 'ward.D')
# 
# foo = 
#         mat %>%
#         t() %>%
#         dist() %>%
#         hclust()
# plot(hang = -1,
#      cex = 0.5)
# 
# plot(hclust)
# 
# plot(hclust)
# dist(method = 'canberra') %>%
#         hclust(method = "ward.D")
# 
# plot(foo,
#      hang = -1,
#      cex = 0.5)
# 
# heatmaply::heatmaply(
#         col = pal,
#         show_denodrogram = c(F, F)
# )
# heatmap(col = pal)
# 
# 
# # parallel coordinates
# compare_games_pca %>%
#         filter(game_id %in% neighbors$neighbor_id) %>%
#         pivot_longer(
#                 cols = -c(game_id, name, yearpublished),
#                 values_to = c("value"),
#                 names_to = c("component")
#         ) %>%
#         filter(component %in% paste("PC0",))
# 
# 
# 
# 
# # compare_games %>%
# #         select(starts_with("mechanics_"))
# 
# # samp = 
# #         games_predicted %>%
# #         sample_n(1) %>%
# #         pull(game_id)
# # 
# # games_predicted %>%
# #         make_game_estimates_gt_table(
# #                 id = samp
# #         ) %>%
# #         tab_header(
# #                 title
# #         )
# #         filter(game_id %in% samp) %>%
# #         mutate(game_id, name, yearpublished,
# #                type = 'Current',
# #                averageweight, usersrated, average, bayesaverage,
# #                .keep = 'none') %>%
# #         bind_rows(.,
# #                   games_predicted %>%
# #                           filter(game_id %in% samp) %>%
# #                           mutate(game_id, name, yearpublished,
# #                                  type = 'Estimated',
# #                                  averageweight = .pred_averageweight,
# #                                  usersrated = .pred_usersrated,
# #                                  average = .pred_average,
# #                                  bayesaverage = .pred_bayesaverage,
# #                                  .keep = 'none')
# #         ) %>%
# #         arrange(desc(type)) %>%
# #         select(game_id, name, yearpublished, type, usersrated, averageweight, average, bayesaverage) %>%
# #     #    group_by(name, game_id, yearpublished) %>%
# #         mutate(
# #                 Type = type,
# #                 `Average Weight` = round(averageweight,2),
# #                 `User Ratings` = usersrated,
# #                 `Average Rating` = round(average,2),
# #                 `Geek Rating` = round(bayesaverage,2),
# #                 .keep = 'none'
# #         ) %>%
# #         select(Type, `Average Weight`, `Average Rating`, `Geek Rating`, `User Ratings`) %>%
# #         gt(
# #                 rowname_col = "Type",
# #         ) %>%
# #         set_gt_theme() %>%
# #         add_gt_predictions_color() %>%
# #         cols_align(
# #                 columns = c("Average Weight", "Average Rating", "Geek Rating", "User Ratings"),
# #                 align = c("center")
# #         )
# # 
# #         
# #            #    average, averageweight, usersrated, bayesaverage)
# # 
# # # make_average_ggplot = function(data) {
# # #         
# # #         
# # #         
# # # }
# # # 
# # 
# # vis_data =
# #         compare_games %>%
# #         select(-est_averageweight) %>%
# #         left_join(.,
# #                   games_imputed %>%
# #                           select(game_id, average, usersrated, est_averageweight, bayesaverage),
# #                   by = join_by(game_id)
# #         )
# # 
# # library(ggrepel)
# # 
# # vis_medians = 
# #         vis_data %>%
# #         summarize(
# #                 across(
# #                         c("average",
# #                           "bayesaverage",
# #                           "est_averageweight",
# #                           "usersrated"),
# #                         ~ median(.x)
# #                 )
# #         )
# # 
# # averageweight_plot = 
# #         vis_data %>%
# #         ggplot(aes(x=est_averageweight,
# #                    label = paste0(name, ' (', yearpublished, ')'),
# #                    #   size = log(usersrated),
# #                    y=average))+
# #         geom_point(alpha = 0.15,
# #                    color = 'grey60')+
# #         guides(size = 'none')+
# #         geom_vline(
# #                 xintercept = vis_medians$est_averageweight,
# #                 linetype = 'dotted',
# #         )+
# #         geom_hline(
# #                 yintercept = vis_medians$average,
# #                 linetype = 'dotted',
# #         )+
# #         theme_bgg()+
# #         ylab("Average")+
# #         xlab("Average Weight (Estimated)")
# # 
# # 
# # usersrated_plot = 
# #         vis_data %>%
# #         ggplot(aes(x=average,
# #                    label = paste0(name, ' (', yearpublished, ')'),
# #                    y=usersrated))+
# #         geom_point(alpha = 0.15,
# #                    color = 'grey60')+
# #         guides(size = 'none')+
# #         geom_vline(
# #                 xintercept = vis_medians$average,
# #                 linetype = 'dotted',
# #         )+
# #         geom_hline(
# #                 yintercept = vis_medians$usersrated,
# #                 linetype = 'dotted'
# #         )+
# #         theme_bgg()+
# #         xlab("Average")+
# #         ylab("User Ratings (logged)")+
# #         scale_y_log10(
# #                 breaks = c(100, 1000, 10000, 25000, 50000, 100000)
# #         )
# # 
# # 
# # 
# # add_game_highlight = function(plot,
# #                               plot_data = NULL,
# #                               color = 'blue',
# #                               game_ids) {
# #         
# #         plot_data = 
# #                 plot$data
# #         
# #         # if (game_id %in% plot_data$game_id) {
# #         #         
# #         plot +
# #                 geom_point(
# #                         data = plot_data %>%
# #                                 filter(game_id %in% game_ids),
# #                         color = color
# #                 ) +
# #                 geom_text_repel(
# #                         data = plot_data %>%
# #                                 filter(game_id %in% game_ids),
# #                         color = color,
# #                         vjust = -1,
# #                         size = 4
# #                 )
# #         #                 geom_text_repel(
# #         #                         data = plot_data %>%
# #         #                                 filter(game_id == game_id),
# #         #                         color = color,
# #         #                         size = 3
# #         #                 )
# #         # # }
# #         
# # }
# # 
# # neighbors = 
# #         compare_games %>%
# #         find_game_neighbors(
# #                 game = 'Nemesis'
# #         )
# # 
# # gridExtra::grid.arrange(
# #         averageweight_plot %>%
# #                 add_game_highlight(
# #                         game_ids = neighbors$neighbor_id[1]),
# #         usersrated_plot %>%
# #                 add_game_highlight(
# #                         game_ids = neighbors$neighbor_id[1]),
# #         ncol = 2
# # )
# # 
# # 
# # averageweight_plot %>%
# #         add_game_highlight(
# #                 game_ids = neighbors$neighbor_id[1])
# # 
# # usersrated_plot %>%
# #         add_game_highlight(
# #                 game_ids = neighbors$neighbor_id[1])
# 
# 
# # 
# # compare_games %>%
# #         find_game_neighbors(game = 'Gloomhaven') %>%
# #         make_neighbors_gt_table(
# #                 games = games_imputed
# #         ) %>%
# #         cols_width(
# #                 neighbor ~ px(200)
# #         ) %>%
# #         tab_options(data_row.padding = px(15))
# # 
# #         tab_style(
# #                 style = cell_text(),
# #                 locations = cells_body(),
# #                 css = list("line-height" = "40px")  # Adjust the height as needed
# #         )
# #         cols_width(
# #                 neighbor ~ px(200)
# #         )
# #         cols_width(
# #                 columns = c(neighbor),
# #                 width = px(100)
# #         )
# # compare_ica_recipe = 
# #         compare_recipe %>%
# #         step_ica(all_numeric_predictors(),
# #                  num_comp = 50) %>%
# #         prep(retain = T)
# 
# # compare_umap_recipe =
# #         compare_recipe %>%
# #         embed::step_umap(all_predictors())
# # 
# # compare_umap = 
# #         compare_umap_recipe %>%
# #         bake(new_data = NULL)
# # 
# # compare_umap %>%
# #         plotly::plot_ly(
# #                 x = ~ UMAP1,
# #                 y = ~ UMAP2,
# #                 hoverinfo = "text",
# #                 text = ~ paste(
# #                         paste0(name, " (", yearpublished, ")"),
# #                         round(UMAP1,2),
# #                         round(UMAP2,2),
# #                         sep = "\n")
# #         ) %>%
# #         add_markers(size = 0.25)
# #         
# # 
# # compare_mat = 
# #         compare_pca_estimates %>%
# #         select(game_id, name, starts_with("PC")) %>%
# #         mutate(id = paste(name, game_id)) %>%
# #         select(game_id, 
# #                id,
# #                PC01, PC02, PC03, PC04, PC05, PC06, PC07, PC08, PC09, PC10, 
# #                any_of(paste0("PC", seq(11, 20, 1))))
# # 
# #         column_to_rownames("game_id")
# #         as.matrix() %>%
# #         heatmap(scale ="column")
# 
# # neighbors = 
# #         compare_games %>%
# #         find_game_neighbors(
# #                 game = 'Food Chain Magnate',
# #                 metric = 'canberra'
# #         ) %>%
# #         head(16)
# # 
# # pca_plot_2d = 
# #         compare_games_pca %>%
# #         plotly::plot_ly(
# #                 x = ~ PC001,
# #                 y = ~ PC002,
# #                 hoverinfo = "text",
# #                 type = 'scatter',
# #                 #  type = 'scatter',
# #                 mode = 'markers',
# #                 marker = list(
# #                         color = 'darkgrey',
# #                         size = 8,
# #                         opacity = 0.2
# #                 ),
# #                 text = ~ paste(
# #                         paste0(name, " (", yearpublished, ")"),
# #                         paste("PC001:", round(PC001,2)),
# #                         paste("PC002:", round(PC002,2)),
# #                         sep = "\n"),
# #                 name = 'All Games'
# #         ) %>%
# #         add_trace(
# #                 data = compare_games_pca %>%
# #                         filter(game_id %in% neighbors$neighbor_id), 
# #                 #   mode = 'markers',
# #                 # mode = 'lines+markers',
# #                 # line = 
# #                 #         list(color = 'purple'),
# #                 marker = list(
# #                         color = 'red',
# #                         size = 8,
# #                         opacity = 1
# #                 ),
# #                 name = "Neighbors"
# #         ) %>%
# #         # add trace for game
# #         add_trace(
# #                 data = compare_games_pca %>%
# #                         # filter(game_id %in% find_game_id(input$game))
# #                         filter(game_id %in% (neighbors$neighbor_id[1])),
# #                 mode = 'markers',
# #                 marker = list(
# #                         color = 'orange',
# #                         size = 12,
# #                         opacity = 1
# #                 ),
# #                 name = 'Game'
# #         ) %>%
# #         layout(legend = list(x = 0.1, y = 0.9))
# 
# # 
# # compare_pca_estimates %>%
# #         left_join(.,
# #                   games_imputed) %>%
# #         plotly::plot_ly(
# #                 x = ~ PC01,
# #                 y = ~ PC02,
# #                 z = ~ PC03,
# #                 hoverinfo = "text",
# #                 text = ~ paste(
# #                         paste0(name, " (", yearpublished, ")"),
# #                         round(PC01,2),
# #                         round(PC02,2),
# #                         round(PC03,2),
# #                         sep = "\n")
# #         ) %>%
# #         add_markers(size = 0.5,
# #                     color = ~ average)
# 
# 
# # 
# # library(ggforce)
# # 
# # average_plot = 
# #         vis_data %>%
# #         select(game_id, name, est_averageweight, usersrated, average, bayesaverage)
# #         
# #         
# # vis_data %>%
# #         select(game_id, name, est_averageweight, usersrated, average, bayesaverage) %>%
# #         mutate(log_usersrated = log(usersrated)) %>%
# #         ggplot(
# #                 aes(x=.panel_x,
# #                     y=.panel_y))+
# #         geom_autopoint()+
# #         facet_matrix(
# #                 vars(average, est_averageweight),
# #                 vars(log_usersrated, average)
# #         )
# # 
# # average_plot = 
# #         vis_data %>%
# #         make_average_plot() %>%
# #         hide_legend()
# # 
# # usersrated_plot = 
# #         vis_data %>%
# #         make_usersrated_plot() %>%
# #         hide_legend() %>%
# #         layout(yaxis = list(type = "log"))
# # 
# # subplot(average_plot,
# #         usersrated_plot,
# #         nrows = 1)
# # 
# # 
# # rmarkdown::run(
# #         file = here::here("shiny", "game_report.Rmd")
# # )
# # 
# # # compare_data %>%
# # #         find_game_neighbors(
# # #                 game = "Rex: Final Days of an Empire",
# # #                 metric = 'angular'
# # #         ) %>%
# # #         unnest(neighbors) %>%
# # #         print(n = 20)
# # # 
# # # compare_pca_recipe %>%
# # #         tidy(id = 'pca') %>%
# # #         filter(component %in% paste0('PC', seq(1, 6))) %>%
# # #         group_by(component) %>%
# # #         slice_max(order_by = abs(value),
# # #                   n = 15) %>%
# # #         mutate(terms = bggUtils::present_text(terms)) %>%
# # #         ggplot(aes(x=value,
# # #                    y=reorder_within(terms, value, component)))+
# # #         geom_col()+
# # #         scale_y_reordered()+
# # #         facet_wrap(component ~.,
# # #                    scales = "free",
# # #                    ncol = 2)+
# # #         ylab("")
# # 
# # 
# # # compare_baked %>%
# # #         select(game_id, name, num_mechanics, starts_with("number_mechanics")) %>%
# # #         pivot_longer(cols = -c("game_id", "name", "num_mechanics"),
# # #                      names_to = "variable") %>%
# # #         ggplot(aes(x=num_mechanics,
# # #                    color = variable,
# # #                    y = value))+
# # #         geom_line()+
# # #         theme_minimal()+
# # #         theme(legend.position = 'top')
# # 
# # 
# # # 
# # # compare_pca_estimates %>%
# # #         left_join(.,
# # #                   games_imputed) %>%
# # #         plotly::plot_ly(
# # #                 x = ~ PC01,
# # #                 y = ~ PC02,
# # #                 z = ~ PC03,
# # #                 hoverinfo = "text",
# # #                 text = ~ paste(
# # #                         paste0(name, " (", yearpublished, ")"),
# # #                         round(PC01,2),
# # #                         round(PC02,2),
# # #                         round(PC03,2),
# # #                         sep = "\n")
# # #         ) %>%
# # #         add_markers(size = 0.5,
# # #                     color = ~ average)
# # 
# # 
# # # ggplot(aes(x=PC01,
# # #            y=PC02))+
# # #         geom_point()
# # # 
# # # compare_data %>%
# # #         find_game_neighbors(
# # #                 game_id = 238908,
# # #                 metric = 'angular') %>%
# # #         unnest(neighbors)
# # 
# # 
# # # compare_data %>%
# # #         names() %>%
# # #         sort()
# # # 
# # 
# # # unnest(neighbors) %>%
# # #         left_join(.,
# # #                   compare_data %>%
# # #                           mutate(neighbor_id = game_id,
# # #                                  neighbor_yearpublished = yearpublished,
# # #                                  .keep = 'none')) %>%
# # #         mutate(
# # #                 Name = name,
# # #                 Metric = .metric,
# # #                 Neighbor = paste0(neighbor_name, ' (', neighbor_yearpublished, ')'),
# # #                 Distance = dist,
# # #                 .keep = 'none') %>%
# # #         mutate_if(is.numeric, round, 3) %>%
# # #         head(25+1) %>%
# # #         group_by(Name, Metric) %>%
# # #         gt::gt() %>%
# # #         gt::cols_align(columns = c("Neighbor"),
# # #                        align = "left")
# # # 
# # # rmarkdown::render(
# # #         here::here("shiny", "find_game_neighbors.Rmd")
# # # )
# # 
# # 
# # 
# # # find_game_neighbors(
# # #         find_game_id = 12,
# # #         comparison_data = compare_data) %>%
# # #         unnest(neighbors)
# # # 
# # # cdist(y_matrix, x_matrix, metric = 'angular') %>%
# # #         as.data.frame() %>%
# # #         as_tibble() %>%
# # #         mutate(similarity = 1- dist)
# # # rename(dist = V1) %>%
# # #         mutate(similarity = 1-dist)
# # # 
# # # 
# # # 
# # # 
# # # compare_matrix 
# # # 
# # # compare_matrix %>%
# # #         rdist(X = .,
# # #               metric = 'angular')
# # 
# # make_heatmap = function(data) {
# #         
# #         
# #         
# # }
# # 
# # 
# # prep_pca_heatmap = function(data,
# #                             num_components = 20) {
# #         
# #         temp = data %>%
# #                 set_names(., gsub("^PC0", "PC", names(data)))
# #         
# #         temp %>%
# #                 select(game_id, name, 
# #                        any_of(paste0("PC", seq(1, num_components)))) %>%
# #                 mutate(id = paste(name, game_id)) %>%
# #                 select(id, starts_with("PC")) %>%
# #                 as.data.frame() %>%
# #                 column_to_rownames("id") %>%
# #                 as.matrix()
# #         
# # }
# # 
# # prep_matrix = function(data,
# #                        features = compare_features) {
# #         
# #         data %>%
# #                 mutate(id = paste(name, game_id)) %>%
# #                 select(id,
# #                        any_of(compare_features)
# #                 ) %>%
# #                 column_to_rownames("id") %>%
# #                 as.matrix() %>%
# #                 t()
# #         
# # }
# # 
# # 
# # neighbors = 
# #         compare_games %>%
# #         find_game_neighbors(
# #                 game = 'Ra',
# #                 metric = 'hamming'
# #         )
# # head(16)
# # 
# # compare_pca_estimates %>%
# #         #  sample_n(100) %>%
# #         #  filter(game_id %in% neighbors$neighbor_id %>% head(1)) %>%
# #         filter(
# #                 game_id %in% neighbors$neighbor_id) %>%
# #         #    game_id %in% sample(compare_data$game_id, 25)) %>%
# #         prep_pca_heatmap() %>%
# #         heatmap(
# #                 Colv =NA,
# #                 Rowv = NA,
# #                 cexRow = 1,
# #                 labRow = str_trunc(rownames(.), width = 30),
# #         )
# # heatmaply::heatmaply()
# # 
# # compare_pca_estimates %>%
# #         #  sample_n(100) %>%
# #         #  filter(game_id %in% neighbors$neighbor_id %>% head(1)) %>%
# #         filter(
# #                 game_id %in% neighbors$neighbor_id) %>%
# #         #    game_id %in% sample(compare_data$game_id, 25)) %>%
# #         prep_pca_heatmap() %>%
# #         round(., 2) %>%
# #         heatmaply::heatmaply(., 
# #                              colors = "RdBu",
# #                              #   Rowv = NA,
# #                              #   dendogram = "column",
# #                              xlab = "", 
# #                              ylab = "", 
# #                              main = "",
# #                              margins = c(60,100,40,20),
# #                              titleX = FALSE,
# #                              hide_colorbar = TRUE,
# #                              branches_lwd = 0.1,
# #                              label_names = c("id", "Feature:", "value"),
# #                              fontsize_row = 8, 
# #                              fontsize_col = 5,
# #                              labCol = colnames(.),
# #                              labRow = str_trunc(rownames(.), width = 30),
# #                              heatmap_layers = theme(axis.line=element_blank())
# #         )
# # 
# # 
# # 
# # make_neighbors
# # 
# # # visualizations ----------------------------------------------------------
# # 
# # 
# # add_trace_games = function(plot,
# #                            game_ids,
# #                            color = 'blue',
# #                            ax = 10,
# #                            ay = 20,
# #                            trace_label = "test") {
# #         
# #         plot %>%
# #                 add_trace(
# #                         data = plotly_data(plot, 1) %>%
# #                                 filter(game_id %in% game_ids), 
# #                         mode = 'markers',
# #                         textposition = 'topright',
# #                         marker = list(
# #                                 color = color,
# #                                 symbol = 'circle',
# #                                 opacity = 1
# #                         ),
# #                         name = trace_label
# #                 ) %>%
# #                 add_annotations(
# #                         data = plotly_data(plot, 1) %>%
# #                                 filter(game_id %in% game_ids),
# #                         text = ~ name,
# #                         ax = ax,
# #                         ay = ay,
# #                         arrowhead = 0,
# #                         font = list(
# #                                 size = 12,
# #                                 color = color)
# #                 )
# #         
# # }
# # 
# # make_average_plot = function(data,
# #                              neighbors) {
# #         
# #         data %>%
# #                 plotly::plot_ly(
# #                         x = ~ est_averageweight,
# #                         y = ~ average,
# #                         #size = ~ log1p(usersrated),
# #                         type = 'scatter',
# #                         hoverinfo = "text",
# #                         mode = 'markers',
# #                         marker = list(
# #                                 color = 'darkgrey',
# #                                 symbol = 'circle-open',
# #                                 opacity = 0.5),
# #                         text =  ~ paste(
# #                                 paste0(name, ' (', yearpublished, ')'),
# #                                 paste("Average Weight", round(est_averageweight, 2)),
# #                                 paste("Average Rating:", round(average, 2)),
# #                                 paste("User Ratings:", usersrated),
# #                                 paste("Geek Rating", round(bayesaverage, 2)),
# #                                 sep = "\n"),
# #                         name = 'All Games'
# #                 ) %>%
# #                 hide_colorbar() %>%
# #                 layout(
# #                         xaxis = list(title = "Average Weight (Estimated)"),
# #                         yaxis = list(title = "Average Rating")
# #                 ) %>%
# #                 layout(legend = list(orientation = "h",   # show entries horizontally
# #                                      xanchor = "center",  # use center of legend as anchor
# #                                      x = 0.5,
# #                                      y = 11))
# #         
# # }
# # 
# # 
# # make_usersrated_plot = function(data,
# #                                 neighbors) {
# #         
# #         data %>%
# #                 plotly::plot_ly(
# #                         x = ~ average,
# #                         y = ~ usersrated,
# #                         #size = ~ log1p(usersrated),
# #                         type = 'scatter',
# #                         hoverinfo = "text",
# #                         mode = 'markers',
# #                         marker = list(
# #                                 color = 'darkgrey',
# #                                 symbol = 'circle-open',
# #                                 opacity = 0.5),
# #                         text =  ~ paste(
# #                                 paste0(name, ' (', yearpublished, ')'),
# #                                 paste("Average Weight", round(est_averageweight, 2)),
# #                                 paste("Average Rating:", round(average, 2)),
# #                                 paste("User Ratings:", usersrated),
# #                                 paste("Geek Rating", round(bayesaverage, 2)),
# #                                 sep = "\n"),
# #                         name = 'All Games'
# #                 ) %>%
# #                 hide_colorbar() %>%
# #                 layout(
# #                         yaxis = list(title = "User Ratings (logged)"),
# #                         xaxis = list(title = "Average")
# #                 ) %>%
# #                 layout(legend = list(orientation = "h",   # show entries horizontally
# #                                      xanchor = "center",  # use center of legend as anchor
# #                                      x = 0.5,
# #                                      y = 11))
# #         
# # }
# # 
# # 
# # neighbors = 
# #         compare_games %>%
# #         find_game_neighbors(
# #                 game = 'Twilight Imperium: Fourth Edition'
# #         )
# # 
# # plot_data = 
# #         compare_games %>%
# #         select(-est_averageweight) %>%
# #         left_join(.,
# #                   games_imputed %>%
# #                           select(game_id, average, usersrated, est_averageweight, bayesaverage)
# #         )
# # 
# # plot_data %>%
# #         make_average_plot() %>%
# #         add_trace_games(game_ids = neighbors$neighbor_id[1],
# #                         color = 'blue',
# #                         ax = 10,
# #                         ay = -30,
# #                         trace_label = 'Game') %>%
# #         add_trace_games(game_ids =  neighbors$neighbor_id[2:5],
# #                         color = 'black',
# #                         ax = -20,
# #                         ay = 20,
# #                         trace_label = 'Neighbors')
# # 
# # # plot average vs usersrated
# # plot_data %>%
# #         make_usersrated_plot() %>%
# #         add_trace_games(game_ids = neighbors$neighbor_id[1],
# #                         color = 'blue',
# #                         ax = 20,
# #                         ay = -20,
# #                         trace_label = 'Game') %>%
# #         add_trace_games(game_ids =  neighbors$neighbor_id[2:5],
# #                         color = 'black',
# #                         ax = -20,
# #                         ay = 20,
# #                         trace_label = 'Neighbors')
# # 
# # 
# # plotly::plot_ly(
# #         x = ~ est_averageweight,
# #         y = ~ average,
# #         type = 'scatter',
# #         hoverinfo = "text",
# #         mode = 'markers',
# #         marker = list(
# #                 color = 'darkgrey',
# #                 symbol = 'circle-open',
# #                 size = 8,
# #                 opacity = 0.5),
# #         text =  ~ paste(
# #                 paste0(name, ' (', yearpublished, ')'),
# #                 paste("Average Weight", round(est_averageweight, 2)),
# #                 paste("Average Rating:", round(average, 2)),
# #                 paste("User Ratings:", usersrated),
# #                 paste("Geek Rating", round(bayesaverage, 2)),
# #                 sep = "\n"),
# #         name = 'All Games') %>%
# #         hide_colorbar() %>%
# #         layout(legend = list(orientation = "h",   # show entries horizontally
# #                              xanchor = "center",  # use center of legend as anchor
# #                              x = 0.5,
# #                              y = 11)) %>%
# #         layout(
# #                 xaxis = list(title = "Average Weight (Estimated)"),
# #                 yaxis = list(title = "Average Rating")
# #         )
# # 
# # make_average_plot %>%
# #         plot_games_data = 
# #         compare_games %>%
# #         select(-est_averageweight) %>%
# #         left_join(.,
# #                   games_imputed %>%
# #                           select(game_id, average, usersrated, est_averageweight, bayesaverage)
# #         )
# # 
# # 
# # plot_games_average = 
# #         plot_games_data %>%
# #         plotly::plot_ly(
# #                 x = ~ est_averageweight,
# #                 y = ~ average,
# #                 type = 'scatter',
# #                 #  color = ~ average,
# #                 #  colors = "RdBu",
# #                 hoverinfo = "text",
# #                 # type = 'scatter',
# #                 #  type = 'scatter',
# #                 mode = 'markers',
# #                 marker = list(
# #                         color = 'darkgrey',
# #                         symbol = 'circle-open',
# #                         size = 8,
# #                         # colorbar = list(
# #                         #         side = 'top',
# #                         #         orientation = 'h'),
# #                         opacity = 0.5),
# #                 text =  ~ paste(
# #                         paste(name, yearpublished),
# #                         paste("Average Weight", round(est_averageweight, 2)),
# #                         paste("Average Rating:", round(average, 2)),
# #                         paste("User Ratings:", usersrated),
# #                         paste("Geek Rating", round(bayesaverage, 2)),
# #                         sep = "\n"),
# #                 name = 'All Games'
# #         ) %>%
# #         hide_colorbar() %>%
# #         layout(
# #                 xaxis = list(title = "Average Weight (Estimated)"),
# #                 yaxis = list(title = "Average Rating")
# #         )
# # 
# # 
# # plot_games_average %>%
# #         add_trace(
# #                 data = plot_games_data %>%
# #                         filter(game_id %in% neighbors$neighbor_id[1]), 
# #                 mode = 'markers',
# #                 textposition = 'topright',
# #                 #   mode = 'markers',
# #                 # mode = 'lines+markers',
# #                 # line = 
# #                 #         list(color = 'purple'),
# #                 marker = list(
# #                         color = 'blue',
# #                         symbol = 'circle',
# #                         opacity = 1
# #                 ),
# #                 name = "Game"
# #         )  %>%
# #         add_trace(
# #                 data = plot_games_data %>%
# #                         filter(game_id %in% neighbors$neighbor_id[-1]), 
# #                 #   mode = 'markers',
# #                 # mode = 'lines+markers',
# #                 # line = 
# #                 #         list(color = 'purple'),
# #                 marker = list(
# #                         color = "navy",
# #                         symbol = 'circle',
# #                         opacity = 1
# #                 ),
# #                 name = "Neighbors"
# #         )  %>%
# #         hide_colorbar() %>%
# #         layout(legend = list(orientation = "h",   # show entries horizontally
# #                              xanchor = "center",  # use center of legend as anchor
# #                              x = 0.5,
# #                              y = 11)) %>%
# #         # add annotations for first game
# #         add_annotations(
# #                 data = plot_games_data %>%
# #                         filter(game_id %in% neighbors$neighbor_id[1]),
# #                 text = ~ name,
# #                 ax = 10,
# #                 ay = -20,
# #                 arrowhead = 0,
# #                 font = list(
# #                         size = 12,
# #                         color = "blue")
# #         ) %>%
# #         # add annotations for neigthbors
# #         add_annotations(
# #                 data = plot_games_data %>%
# #                         filter(game_id %in% neighbors$neighbor_id[2:8]),
# #                 text = ~ name,
# #                 ax = sample(c(30, 20, -20, -30), 5, replace =T),
# #                 ay = sample(c(-30,-20, 20, 30), 5, replace=T),
# #                 arrowhead = 0,
# #                 font = list(
# #                         size = 12,
# #                         color = "navy")
# #         )
# # 
# # # layout(
# # #         coloraxis=list(
# # #                 colorbar=list(
# # #                         orientation="v"
# # #                 )
# # #         )
# # # )
# # # 
