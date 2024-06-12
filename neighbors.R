# get source code
targets::tar_load_globals()

neighbors_recipe = function(data,
                            ids = id_vars(),
                            predictors = c("minplayers",
                                           "maxplayers",
                                           "playingtime",
                                           "minplaytime",
                                           "maxplaytime",
                                           "image",
                                           "thumbnail",
                                           "minage",
                                           "categories",
                                           "mechanics",
                                           "families",
                                           "mechanisms",
                                           "components"),
                            mechanics_threshold = 1,
                            categories_threshold = 1,
                            families_threshold = 100,
                            components_threshold = 25,
                            mechanisms_threshold = 25) {
    
    data |>
        build_recipe(
            ids = ids,
            predictors = predictors,
        ) |>
        step_rm(has_role("extras")) |>
        # bgg preprocessing
        add_preprocessing() |>
        # imputation
        add_imputation() |>
        # remove year
        step_rm(year) |>
        # include most mechanics
        add_dummies(mechanics,
                    threshold = mechanics_threshold) |>
        # # include all categories
        # add_dummies(categories,
        #             threshold = categories_threshold) |>
        # families
        add_dummies(families,
                    threshold = families_threshold) |>
        # components
        add_dummies(components,
                    threshold = components_threshold) |>
        # mechanisms
        add_dummies(mechanisms,
                    threshold = mechanisms_threshold) |>
        # remove zero variance
        step_zv(all_numeric_predictors()) |>
        # remove highly correlated
        step_corr(all_numeric_predictors(), threshold = 0.95) |>
        # normalize
        step_normalize(all_numeric_predictors())
    
}

games = 
    bggUtils::get_games_from_gcp()

games_prepped = games |> 
    bggUtils:::preprocess_bgg_games()

averageweight_fit =
    vetiver::vetiver_pin_read(model_board,
                              name = "bgg_averageweight_")

games_imputed = 
    games_prepped |> impute_averageweight(model = averageweight_fit)

games_imputed |>
    separate_longer_delim(delim = ", ", cols = 'families') |>
    select(game_id, families) |>
    filter(grepl("^players|^category|^players", families))

base_recipe = 
    train |>
    prepare_for_neighbors() |>
    neighbors_recipe() |>
    update_role(game_id, new_role = "game_ids") |>
    step_rm(has_role("extras"),
            has_role("id")) |>
    step_rm(all_nominal(),
            -has_role("game_id"))


prepare_for_neighbors = function(data) {
    
    
    parse_families = function(data, terms = '^players|^category|^players') {
        
        parsed =
            data |>
            separate_longer_delim(delim = ", ", cols = 'families') |>
            select(game_id, families) |>
            filter(grepl(terms, families)) |>
            group_by(game_id) |>
            summarize(families := paste(families, collapse = ", "),
                      .groups = 'drop')
        
        
        data |>
            select(-families) |>
            left_join(parsed, by = join_by(game_id))
        
    }
    
    data |>
        parse_families() |>
        select(-starts_with(".pred"))
}

games_filtered = 
    games_imputed |>
    filter(usersrated>=25)

pca_recipe =
    base_recipe |>
    step_pca(all_numeric_predictors(),
             keep_original_cols = T,
             threshold = 0.25,
             id = 'pca') |>
    prep(strings_as_factors = F,
         retain = T)

split <- games_filtered |>
    initial_validation_split(prop = c(0.6, 0.2))

base_recipe = 
    train |>
    prepare_for_neighbors() |>
    neighbors_recipe() |>
    update_role(game_id, new_role = "game_ids") |>
    step_rm(has_role("extras"),
            has_role("id")) |>
    step_rm(all_nominal(),
            -has_role("game_id"))

prepped_train = 
    base_recipe |>
    bake(new_data = NULL) 

prepped_valid = 
    base_recipe |>
    bake(new_data = valid)


# baked = 
#     rec |>
#     bake(new_data = NULL)
# 
# library(ggforce)
# 
# # baked |>
# #     select(game_id, name, yearpublished, paste0("PC", seq(1, 4))) |>
# #     ggplot(
# #         aes(x=.panel_x,
# #             y=.panel_y)
# #     )+
# #     geom_autopoint(size = 2, alpha = 0.5) +
# #     geom_autodensity()+
# #     facet_matrix(vars(starts_with("PC")), layer.diag = 2)
# 
# baked |>
#     ggplot(aes(x=PC1,
#                y=PC2,
#                label = name))+
#     geom_text(size = 2,
#               alpha = 0.5)
# 
# baked |>
#     select(game_id, name, yearpublished, paste0("PC", seq(1, 4))) |>
#     arrange(PC1)
# 
# baked |>
#     ggplot(aes(x=PC4,
#                y=PC5,
#                label = name))+
#     geom_text(size = 1.5,
#               alpha = 0.5) +
#     geom_point(data = baked |>
#                    filter(name == 'Concordia' | name == 'Godzilla: Tokyo Clash'),
#                color = 'blue')
# 
# 
# find_games = function(data,
#                       games  = NULL,
#                       game_ids = NULL) {
#     
#     if (!is.null(games)) {
#         data |>
#             filter(name %in% games)
#     } else {
#         data |>
#             filter(game_id %in% game_ids)
#     }
# }
# 
# prepare_xy = function(data,
#                       ...) {
#     
#     x = 
#         data |>
#         find_games(...)
#     
#     x_mat = 
#         x |>
#         select(starts_with("PC")) |>
#         as.matrix()
#     
#     y = 
#         data
#     
#     y_mat = 
#         y |>
#         select(starts_with("PC")) |>
#         as.matrix()
#     
#     list(x = x,
#          x_mat = x_mat,
#          y = y,
#          y_mat = y_mat)
# }
# 
# find_neighbors = function(xy, method = 'cosine') {
#     
#     x_mat = xy$x_mat
#     y_mat = xy$y_mat
#     x = xy$x
#     y = xy$y
#     
#     dist = 
#         proxy::simil(x = x_mat, y=y_mat, method = method)
#     
#     # order
#     simil = 
#         order(dist)[1:nrow(y_mat)]
#     
#     ordered =
#         tibble(.row_y = simil,
#                dist = dist[simil])
#     
#     # join up
#     ordered |>
#         left_join(
#             y |>
#                 mutate(
#                     .row_y = row_number(),
#                     neighbor_name = name,
#                     neighbor_game_id = game_id,
#                     .keep = 'none'
#                 ),
#             by = join_by(.row_y)
#         ) |>
#         mutate(
#             game_id = x$game_id,
#             name = x$name,
#             yearpublished = x$yearpublished,
#         ) |>
#         select(game_id, name, yearpublished, neighbor_game_id, neighbor_name, dist) |>
#         arrange(desc(dist))
#     
# }
# 
# neighbors_report = function(neighbors, n =15) {
#     
#     neighbors |>
#         print(n = 15)
#     
#     summed =
#         neighbors |>
#         group_by(game_id, name, yearpublished) |>
#         summarize(
#             mean = mean(dist),
#             median = median(dist),
#             sd = sd(dist),
#             variance = var(dist),
#             harmonic = psych::harmonic.mean(dist),
#             .groups = 'drop') |>
#         pivot_longer(cols = -c(game_id, name, yearpublished),
#                      names_to = c('metric'))
#     
#     lab =  paste(paste(summed$metric, round(summed$value,3), sep = ": "))
#     
#     plot_dist = 
#         neighbors |>
#         group_by(game_id, name, yearpublished, dist = plyr::round_any(dist, 0.05)) |>
#         count() |>
#         ggplot(aes(x=dist, y=n, fill = dist))+
#         geom_col()+
#         scale_fill_gradient2(low = 'coral', mid = 'white', high = 'dodgerblue', midpoint = 0)+
#         scale_x_reverse()+
#         facet_wrap(~paste(name, yearpublished))+
#         guides(fill = 'none')
#     # labs(subtitle = paste(lab, collapse = "\n"))
# }
# 
# neighbors = 
#     baked |>
#     select(game_id, name, yearpublished, PC01:PC15) |>
#     prepare_xy(games = 'Maracaibo') |>
#     find_neighbors()
# 
# neighbors |>
#     neighbors_report()
# 
# data_neighbors = 
#     baked |>
#     mutate(neighbor_game_id = game_id,
#            neighbor_name = name) |>
#     inner_join(neighbors |>
#                    select(neighbor_game_id) |>
#                    head(25)
#     )
# 
# # baked |>
# #     sample_n(1000) |>
# #     select(game_id, name, yearpublished, PC01:PC15) |>
# #     pivot_longer(cols = starts_with("PC"),
# #                  names_to = c("feature")) |>
# #     ggplot(aes(x=feature,
# #                y=value))+
# #     geom_point(position = ggforce::position_jitternormal(sd_x = 0.05, sd_y = 0.01))
# #     geom_text(size = 2, alpha = 0.5,
# #               color = 'grey60') +
# #     geom_point(data = data_neighbors,
# #                color = 'blue')+
# #     geom_text(data = data_neighbors,
# #               color = 'blue',
# #               vjust = -1,
# #               size = 2)
# 
# baked |>
#     inner_join(neighbors)
# 
# rec |>
#     tidy(id = 'pca') |>
#     group_by(component) |>
#     slice_max(abs(value), n = 25) |>
#     filter(component %in% paste0('PC', seq(1, 6))) |>
#     mutate(terms = bggUtils::present_bgg_text(terms, minlength = 25)) |>
#     ggplot(aes(x=value,
#                y=tidytext::reorder_within(terms, value, component)))+
#     geom_col()+
#     facet_wrap(~ component, 
#                ncol = 3,
#                scales = "free_y")+
#     tidytext::scale_y_reordered()+
#     ylab('Feature')
# 
# 
# select(metric, value)
# 
# print(n = 25)
# 
# neighbors |>
#     group_by(game_id, name) |>
#     summarize(
#         mean = mean(dist),
#         median = median(dist),
#         harmonic = psych::harmonic.mean(dist)
#     )
# 
# neighbors |>
#     group_by(game_id, name, dist = plyr::round_any(dist, 0.05)) |>
#     count() |>
#     ggplot(aes(x=dist, y=n, fill = dist))+
#     geom_col()+
#     scale_fill_gradient2(low = 'coral', mid = 'white', high = 'dodgerblue', midpoint = 0)+
#     scale_x_reverse()
# facet_wrap(~name)+
#     theme_light()
# print(n = 25)
# 
# 
# data = baked |>
#     select(game_id, name, yearpublished, any_of(paste0("PC", seq(1, 10))))
# 
# out |>
#     arrange(desc(dist))
# 
# out |>
#     arrange(desc(dist)) |>
#     head(10) |>
#     select(game_id, name, neighbor_game_id, neighbor_name) |>
#     inner_join(
#         data |>
#             rename(neighbor_game_id = game_id) |>
#             select(neighbor_game_id,
#                    starts_with("PC"))
#     ) |>
#     pivot_longer(cols = starts_with("PC"),
#                  names_prefix = "PC",
#                  names_to = c("feature")) |>
#     mutate(feature = as.numeric(feature)) |>
#     ggplot(aes(x=feature, y=value, group=paste(neighbor_game_id, neighbor_name)))+
#     geom_line()
# 
# 
# 
