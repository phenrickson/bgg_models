f2_meas = function() {
    
    metric_tweak("f2_meas", f_meas, beta = 2)
    
}

f1_meas = function() {
    
     metric_tweak("f1_meas", f_meas)
}

extract_tune_preds = function(tuned,
                              outcome = 'hurdle') {
    
    params = 
        tuned |>
        pluck("params", 1)
    
    split = 
        tuned |>
        pluck("split", 1)
    
    data = 
        split |>
        validation() |> 
        select(-any_of({{outcome}}))
    
    preds = 
        tuned |>
        pluck("result", 1) |>
        collect_predictions(parameters = params) |>
        bind_cols(data)
    
    tuned |>
        add_column(tune_preds = list(preds)) |>
        select(outcome, wflow_id, tune_preds) |>
        unnest(tune_preds)
    
}

add_pred_class = function(data,
                          threshold) {
    
    data |>
        mutate(.pred_class = case_when(.pred_yes > threshold ~ 'yes',
                                       TRUE ~ 'no'),
               .pred_class = factor(.pred_class, levels = c("no", "yes"))
        )
}


assess_class_threshold = function(preds,
                                  outcome = 'hurdle',
                                  threshold = seq(0, 1, 0.01),
                                  class_metrics = my_class_metrics,
                                  event_level = 'second') {
    
    map_df(
        threshold,
        ~ preds |>
            add_pred_class(threshold = .x) |>
            group_by(threshold = .x) |>
            class_metrics(
                truth = {{outcome}},
                estimate = .pred_class,
                event_level = 'second'
            )
    )
    
}

plot_class_results = function(results) {
    
    
    thresh =
        results |>
        select_class_threshold()
    
    results |>
        ggplot(aes(x=threshold, 
                   color = .metric,
                   y=.estimate))+
        geom_line()+
        facet_grid(.metric~.)+
        theme_light()+
        scale_colour_viridis_d()+
        geom_vline(data = thresh,
                   aes(xintercept = threshold,
                       color = .metric),
                   linetype = 'dashed',
                   alpha = 0.8)+
        guides(color = 'none')
    
}

select_class_threshold = function(results) {
    
    results |>
        group_by(.metric) |>
        slice_max(n=1, .estimate, with_ties = F)
    
}

my_class_metrics = function() {
    
    yardstick::metric_set(
        yardstick::bal_accuracy,
        yardstick::kap,
        yardstick::mcc,
        f1_meas(),
        f2_meas(),
        yardstick::precision,
        yardstick::recall,
        yardstick::j_index
    )
}

tune_class_metrics = function() {
    yardstick::metric_set(yardstick::roc_auc,
                          yardstick::mn_log_loss,
                          yardstick::pr_auc)
}

my_reg_metrics = function() {
    
    metric_set(
        yardstick::rmse,
        yardstick::mae,
        yardstick::mape,
        yardstick::rsq,
        yardstick::ccc
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
                   starts_with(".pred"),
                   -starts_with(".pred_hurdle")
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
                             alpha = 0.8,
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
        geom_point(position = ggforce::position_jitternormal(sd_y = 0.05),
                   alpha = alpha)+
        facet_wrap(outcome ~.,
                   scales = "free")+
        geom_abline(slope = 1,
                    linetype = 'dashed',
                    alpha = alpha)+
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