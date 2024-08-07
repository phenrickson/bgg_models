get_tuning_plots = function(results) {
    
    results |>
        select(outcome, wflow_id, result) |>
        mutate(plot = map2(result, outcome, ~ .x |> 
                               autoplot() + 
                               labs(title = .y) +
                               theme_bw())) |>
        pull(plot) 
}

extract_vetiver_features = function(v) {
    
    v |>
        extract_vetiver_model() |>
        extract_model_features() 
}

extract_vetiver_model = function(vetiver_obj) {
    
    vetiver_obj |>
        pluck("model") |>
        bundle::unbundle()
}

extract_model_type = function(wflow) {
    
    wflow |>
        extract_fit_parsnip() |>
        pluck("spec") |>
        pluck("engine")
    
}

extract_model_features = function(wflow) {
    
    # check type
    engine = wflow |>
        extract_model_type()
    
    if (engine == 'lightgbm') {
        
        features = map_df(
            c('frequency', 'cover', 'gain'),
            ~ wflow|>
                vip::vi_model(.x,
                              percentage = T) |>
                mutate(type = .x)
        )
        
    } else if (engine == 'glmnet') {
        
        features = wflow |>
            get_coefs.glmnet()
    } else {
        
        features = wflow |>
            vip::vi_model()
    }
    
    features |>
        add_column(engine = engine) |>
        select(engine, everything())
    
}

plot_features.lightgbm = function(features,
                                  top_n = 25,
                                  minlength = 50) {
    
    features |> 
        group_by(type) |> 
        slice_max(Importance, n = top_n) |> 
        mutate(Variable = bggUtils::present_bgg_text(Variable, minlength = minlength)) |> 
        ggplot(aes(x=Importance, 
                   y = reorder(Variable, Importance)))+
        geom_col()+
        facet_wrap(~type, scales = "free_x", ncol = 3) + 
        tidytext::scale_y_reordered()+
        ylab("Feature")
}

plot_features.default = function(features,
                                 top_n = 25,
                                 minlength = 50) {
    features |> 
        slice_max(Importance, n = top_n) |> 
        mutate(Variable = bggUtils::present_bgg_text(Variable, minlength = minlength)) |> 
        ggplot(aes(x=Importance, 
                   y = reorder(Variable, Importance)))+
        geom_col()+
        tidytext::scale_y_reordered()+
        ylab("Feature")
    
}

plot_model_features = function(features,
                               ...) {
    
    engine = unique(features$engine)
    
    # check engine
    if (engine == 'lightgbm') {
        features |>
            plot_features.lightgbm(...)
    } else if (engine == 'glmnet') {
        
        features |>
            coef_plot.glmnet(...)
    } else {
        
        features |>
            plot_features.default()
    }
}

get_glmnet_objs =
    function(glmnet_wflow) {
        
        # extract engine
        glmnet_engine =
            glmnet_wflow %>%
            extract_fit_engine()
        
        # extract parsnip
        glmnet_parsnip =
            glmnet_wflow %>%
            extract_fit_parsnip()
        
        
        # return both
        list("engine" = glmnet_engine,
             "parsnip" = glmnet_parsnip)
        
    }

get_glmnet_coefs = function(glmnet_objs,
                            type = 'parsnip',
                            remove_intercept = T,
                            return_zeroes = T) {
    
    coefs = 
        glmnet_objs |>
        pluck(type) |>
        tidy(return_zeroes = return_zeroes)
    
    if (remove_intercept == T) {
        coefs =
            coefs |>
            filter(term != "(Intercept)")
    } 
    
    coefs
}

color_fill_gradient = function(plot,
                               limits,
                               midpoint = 0,
                               low_color = "red",
                               mid_color = "grey80",
                               high_color = "deepskyblue1",
                               oob = scales::squish) {
    
    plot + 
        scale_fill_gradient2(low = low_color,
                             mid = mid_color,
                             high = high_color,
                             midpoint = 0,
                             limits = limits,
                             oob = oob) +
        scale_fill_gradient2(low = low_color,
                             mid = mid_color,
                             high = high_color,
                             midpoint = 0,
                             limits = limits,
                             oob = oob)
}

fill_colorbar = function(plot) {
    
    plot +
        guides(fill = guide_colorbar(barheight = 0.5,
                                     barwidth = 15,
                                     title.position = 'top')
        )
    
}

color_colorbar = function(plot) {
    
    plot +
        guides(color = guide_colorbar(barheight = 0.5,
                                      barwidth = 15,
                                      title.position = 'top')
        )
    
}

get_coefs.glmnet = function(workflow,
                            remove_intercept = T) {
    
    glmnet_objs = get_glmnet_objs(workflow)
    
    coefs = 
        get_glmnet_coefs(glmnet_objs,
                         type = 'parsnip',
                         remove_intercept = remove_intercept)
    
    if (remove_intercept == T) {
        coefs =
            coefs |>
            filter(term != "(Intercept)")
    }  
    
    coefs
}


top_coefs_by_sign = function(coefs,
                             n = 25) {
    
    coefs |>
        mutate(sign = case_when(estimate > 0 ~ 'increases probability',
                                estimate < 0 ~ 'decreases probability')) |>
        mutate(sign = factor(sign, levels = c("increases probability", "decreases probability"))) |>
        group_by(sign) |>
        slice_max(abs(estimate),
                  n =n)
}

coef_plot.glmnet = function(coefs,
                            top_n = 50,
                            minlength = 50,
                            limits = c(-0.05,0.05),
                            midpoint = 0,
                            facet_by_sign = F,
                            ...) {
    
    
    if (is.null(top_n)) {
        coefs = coefs
    } else {
        coefs = coefs |>
            filter(estimate != 0) |>
            slice_max(order_by = abs(estimate), n = top_n, with_ties = F)
    }
    
    present_coefs = 
        coefs |>
        mutate(tidy_term = bggUtils::present_bgg_text(term, minlength = minlength))
    
    plot = present_coefs |>
        ggplot(aes(x=estimate,
                   fill = estimate,
                   y= reorder(tidy_term, estimate))) +
        geom_col(color = 'white')+
        theme_bgg()+
        labs(y = "Feature",
             x = "Effect on Outcome")
    
    suppressMessages({
        p =
            plot |>
            color_fill_gradient(midpoint = midpoint,
                                limits = limits,
                                oob = scales::squish)
    })
    
    if (facet_by_sign == T) {
        
        p = plot +
            facet_wrap(sign ~.,
                       scales = "free")+
            theme(strip.text.x = element_text(size = 10))
    }
    
    p |>
        fill_colorbar()
    
}

trace_plot.glmnet = function(workflow,
                             upper_estimate = 0.05,
                             lower_estimate = -0.5,
                             minlength = 50,
                             max.overlaps = 25) {
    
    
    glmnet_objs = 
        workflow |>
        get_glmnet_objs()
    
    # get lambda
    lambda =
        glmnet_objs|> 
        pluck("parsnip") |> 
        tidy() |> 
        pull(penalty) |> 
        unique()
    
    coefs = 
        glmnet_objs |>
        get_glmnet_coefs(
            type = 'engine'
        )
    
    plot_coefs = 
        coefs |>
        mutate(label_left =
                   case_when(
                       lambda == min(lambda) & abs(estimate) > upper_estimate ~
                           bggUtils::present_bgg_text(term, minlength = minlength))) %>%
        group_by(term) %>%
        mutate(label_right =
                   case_when(
                       lambda == max(lambda) & estimate > lower_estimate ~
                           bggUtils::present_bgg_text(term, minlength = minlength))) %>%
        ungroup()
    
    plot_coefs |>
        ggplot(aes(x=log(lambda),
                   y=estimate,
                   group = term))+
        geom_line(alpha = 0.5,
                  color = 'grey60')+
        geom_vline(xintercept = log(lambda),
                   linetype = 'dotted',
                   alpha = 0.5)+
        guides(color = 'none')+
        theme_minimal()+
        ggrepel::geom_text_repel(
            aes(label = label_left),
            max.overlaps = max.overlaps,
            size = 2,
            direction = "y",
            hjust =1.5,
            segment.size = .5,
            segment.alpha = .5,
            segment.linetype = "dashed",
            box.padding = .5,
            segment.curvature = 0.2,
            segment.ncp = 3,
            segment.angle = 20)+
        coord_cartesian(xlim = c(min(log(glmnet_objs$engine$lambda)-2), 0))+
        theme(panel.grid.major = element_blank())+
        geom_hline(yintercept = 0,
                   linetype = 'dotted',
                   alpha = 0.5)
}


coef_plot_by_group = function(coefs,
                              group = 'mechanics',
                              width = 40,
                              scales = "free",
                              shrink = T,
                              ...) {
    
    tmp = 
        paste0('^', group)
    
    coefs |>
        filter(grepl(tmp, term)) |>
        top_coefs_by_sign() |>
        mutate(term = gsub(tmp, "", term)) |>
        coef_plot.glmnet(...) +
        facet_wrap(sign ~.,
                   scales = scales,
                   shrink = shrink) +
        theme(strip.text.x = element_text(size = 10),
              axis.text.y = element_text(hjust = 1))+
        scale_x_continuous(breaks = scales::pretty_breaks(n=3))+
        guides(fill = 'none') +
        labs(title = stringr::str_to_title(group))+
        scale_y_discrete(label=function(x) stringr::str_trunc(x, width = width))
    
    
}