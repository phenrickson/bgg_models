# shapley values for games

outcome_models = tribble(~outcome, ~model,
                         "averageweight", averageweight_fit,
                         "average", average_fit,
                         "usersrated", usersrated_fit)

# shap

# workflow
wflow = usersrated_fit

# model
mod = wflow %>%
        extract_fit_engine()

# template
template = wflow %>% 
        extract_preprocessor() %$%
        template

# predictors
predictors = wflow$pre$mold$predictors

# ids
ids = wflow$pre$mold$extras$roles$id

# outcomes
outcomes = wflow$pre$mold$outcomes
        

# pred wrapper
pfun <- function(object, newdata) {
        predict(object, data = newdata)
}

# shapley values from training data
shap = fastshap::explain(mod,
                         X = predictors %>%
                                 as.matrix(),
                         exact = T)

# shap
shap_pivoted =
        shap %>%
        as_tibble() %>%
        bind_cols(ids %>%
                          select(game_id, name, yearpublished),
                  .) %>%
        pivot_longer(cols = -c(game_id, name, yearpublished),
                     values_to = 'contribution',
                     names_to = 'feature') %>%
        filter(contribution !=0)



var = 'number_categories'
shap_var <- data.frame(var = predictors[[var]],
                       shap = shap[[var]])

shap_var %>%
        ggplot(aes(var, shap)) +
        geom_point(alpha = 0.3,
                   position = ggforce::position_jitternormal(sd_y = 0.001,
                                                             sd_x = 0.1)) +
        geom_smooth()+
        ylab("Shapley value")+
        xlab(var)+
        geom_hline(yintercept = 0)+
        theme_bw()+
        ylab(paste(paste("effect on user ratings", 
                         "decreases                                     increases",
                         sep = "\n")))
                         

d_var = 'publishers_capstone_games'
shap_d_var <- data.frame(d_var = predictors[[d_var]],
                       shap = shap[[var]])


# designer medians
shap %>%
        as.data.frame() %>%
        select(starts_with("publishers")) %>%
        apply(., 2, median) %>%
        data.frame() %>%
        rownames_to_column('var') %>%
        magrittr::set_names(., c("var", "median")) %>%
        ggplot(aes(x=median,
                   y= reorder(var ,median)))+
        geom_col()


shap_d_var %>%
        mutate(d_var = factor(d_var)) %>%
        ggplot(aes(d_var, 
                   shap)) +
        geom_boxplot()+
        xlab(d_var)
        ggridges::geom_density_ridges()
        geom_
        geom_boxplot()
        geom_point(alpha = 0.3,
                   position = ggforce::position_jitternormal(sd_y = 0.001)) +
        geom_smooth()+
        ylab("Shapley value")+
        xlab(var)+
        geom_hline(yintercept = 0)

# top features
shap_top_features = 
        shap_pivoted %>%
        group_by(feature) %>%
        summarize(sum_abs = sum(abs(contribution))) %>%
        filter(sum_abs > 0) %>%
        slice_max(order_by = sum_abs,
                  n = 40,
                  with_ties = F)

set.seed(1999)
sample_games = 
        template %>%
        sample_n(100) %>%
        pull(game_id)

shap_pivoted %>%
        filter(game_id %in% sample_games) %>%
     #   filter(feature %in% shap_top_features$feature) %>%
        mutate(feature = case_when(feature %in% shap_top_features$feature ~ feature,
                                   TRUE ~ 'other')) %>%
        group_by(feature) %>%
        mutate(abs_median = median(abs(contribution))) %>%
        ungroup() %>%
        mutate(feature = str_to_title(gsub("_", " ", feature))) %>%
        ggplot(aes(x=contribution,
                   y=reorder(feature,
                             abs_median)))+
        geom_point()

