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
                            minlength = 50,
                            limits = c(-0.05,0.05),
                            midpoint = 0,
                            facet_by_sign = F,
                            ...) {
        
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