my_class_metrics = function() {
    
    yardstick::metric_set(
        yardstick::bal_accuracy,
        yardstick::kap,
        yardstick::mcc,
        yardstick::f_meas,
        yardstick::precision,
        yardstick::recall
    )
}

my_reg_metrics = function() {
    
    metric_set(
        yardstick::rmse,
        yardstick::mae,
        yardstick::mape,
        yardstick::rsq
    )
}

# helper function to pivot estimates
pivot_estimates = function(data,
                           names_from = c(".metric"),
                           values_from = c(".estimate"),
                           ...) {
    
    data |>
        pivot_wider(
            names_from = c(".metric"),
            values_from = c(".estimate"),
            ...) |>
        select(-.estimator) |>
        as.data.frame()
}

# pivot predictions into longer format
pivot_outcomes = function(predictions,
                          metrics,
                          bgg_outcomes = c("averageweight",
                                           "average",
                                           "bayesaverage",
                                           "usersrated")
) {
    
    pivot_predictions = function(predictions) {
        
        predictions |>
            select(yearpublished,
                   game_id, 
                   name, 
                   starts_with(".pred")
            ) |>
            pivot_longer(
                cols = c(starts_with(".pred")),
                names_to = c("outcome"),
                names_prefix = ".pred_",
                values_to = c(".pred")
            )
    }
    
    pivot_actual = function(predictions,
                            bgg_outcomes) {
        
        predictions |>
            select(yearpublished, game_id, name,
                   any_of(bgg_outcomes)
            ) |>
            pivot_longer(
                cols = any_of(bgg_outcomes),
                names_to = c("outcome"),
                values_to = c("actual")
            )
    }
    
    preds = 
        predictions |>
        pivot_predictions()
    
    actual = 
        predictions |>
        pivot_actual(
            bgg_outcomes = bgg_outcomes
        )
    
    actual |>
        left_join(preds,
                  by = join_by(yearpublished, game_id, name, outcome))
    
}

# assess outcomes with supplied metrics
assess_outcomes = function(data,
                           metrics) {
    
    data |>
        metrics(
            truth = actual,
            estimate = .pred
        ) |>
        mutate(across(c(.estimate),
                      ~ round(.x, digits = 3)
                      )
        )
    
}

## assess outcomes with filtering
assess_outcomes_by_threshold = function(data,
                                        metrics = my_reg_metrics(),
                                        groups = c("outcome", "yearpublished"),
                                        threshold = c(0, 25)) {
    
    
    map_df(
        threshold,
        ~ data |>
            filter(usersrated >=.x) |>
            pivot_outcomes() |>
            mutate(minratings := .x) |>
            group_by(minratings, across(any_of(groups))) |>
            assess_outcomes(
                metrics = metrics
            ) 
    )
}

# calculate classification metrics for whether a game becomes a hit
add_bgg_hit = function(data, var, value, threshold = 6.5) {
    
    data |>
        mutate({{var}} := case_when({{value}} >= threshold ~ 'yes',
                                    TRUE ~ 'no'),
               {{var}} := factor({{var}}, levels = c('yes', 'no'))
        )
}

calculate_bgg_hit = function(data,
                             threshold = 6.5) {
    
    data |>
        mutate(bayesaverage = replace_na(bayesaverage, 5.5)) |>
        add_bgg_hit(
            var = bgg_hit,
            value = bayesaverage,
            threshold = threshold
        ) |>
        add_bgg_hit(
            var = .pred_bgg_hit,
            value = .pred_bayesaverage,
            threshold = threshold
        ) |>
        mutate(threshold := threshold)
}

assess_bgg_hit = function(data,
                          metrics) {
    
    data |>
        metrics(
            truth = bgg_hit,
            estimate = .pred_bgg_hit
        )
}

plot_predictions =  function(data,
                             ...) {
    
    data |>
        mutate(actual = case_when(outcome == 'usersrated' ~ log1p(actual),
                                  TRUE ~ actual),
               .pred = case_when(outcome == 'usersrated' ~ log1p(.pred),
                                 TRUE ~ .pred)
        ) |>
        mutate(yearpublished = as.character(yearpublished)) |>
        ggplot(aes(x=.pred, 
                   ...,
                   y=actual)) +
        geom_point(position = ggforce::position_jitternormal(sd_y = 0.05))+
        facet_wrap(outcome ~.,
                   scales = "free")+
        geom_abline(slope = 1,
                    linetype = 'dashed',
                    alpha = 0.8)+
        theme_bgg()+
        ggpubr::stat_cor(p.accuracy = 0.01,
                         show.legend = FALSE)+
        ggthemes::scale_color_colorblind()
}

table_predictions = function(predictions,
                             ...) {
    
    require(reactable)
    
    tab = predictions |>
        pivot_outcomes() |>
        pivot_wider(
            names_from = c("outcome"),
            names_glue = "{outcome}_{.value}",
            values_from = c("actual", ".pred")
        )
    
    pred_names = grep(".pred", names(tab), value = T)
    actual_names = grep("actual", names(tab), value = T)
    
    tab |>
        arrange(desc(bayesaverage_.pred)) |>
        reactable::reactable(
            ...,
            columnGroups = list(
                colGroup(name = "Prediction", columns = pred_names),
                colGroup(name = "Actual", columns = actual_names)
            ),
            columns = list(
                yearpublished = colDef(
                    name = 'Published'
                ),
                game_id = colDef(
                    name = 'ID',
                    show = F
                ),
                name = colDef(
                    name = 'Name'
                ),
                averageweight_actual = colDef(
                    name = 'Average Weight',
                    format = colFormat(
                        digits = 2
                    )
                ),
                averageweight_.pred = colDef(
                    name = 'Average Weight',
                    format = colFormat(
                        digits = 2
                    )
                ),
                average_actual = colDef(
                    name = 'Average',
                    format = colFormat(
                        digits = 2
                    )
                ),
                average_.pred = colDef(
                    name = 'Average',
                    format = colFormat(
                        digits = 2
                    )
                ),
                bayesaverage_actual = colDef(
                    name = 'Geek',
                    format = colFormat(
                        digits = 2
                    )
                ),
                bayesaverage_.pred = colDef(
                    name = 'Geek',
                    format = colFormat(
                        digits = 2
                    )
                ),
                usersrated_actual = colDef(
                    name = 'Ratings',
                    format = colFormat(
                        digits = 0,
                        separators = T
                    )
                ),
                usersrated_.pred = colDef(
                    name = 'Ratings',
                    format = colFormat(
                        digits = 0,
                        separators = T
                    )
                )
            )
        )
    
    
}