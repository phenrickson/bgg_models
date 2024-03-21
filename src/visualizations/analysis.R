plot_outcome_predictions =  function(data, color = yearpublished, facet = 'outcome') {
    
    data |>
        mutate(actual = case_when(outcome == 'usersrated' ~ log1p(actual),
                                  TRUE ~ actual),
               .pred = case_when(outcome == 'usersrated' ~ log1p(.pred),
                                 TRUE ~ .pred)
        ) |>
        mutate(yearpublished = as.character(yearpublished)) |>
        ggplot(aes(x=.pred, 
                   color = {{color}},
                   y=actual)) +
        geom_point(alpha = 0.5,
                   position = ggforce::position_jitternormal(sd_y = 0.05))+
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
