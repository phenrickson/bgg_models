
# helper function to retrieve workflow quickly
get_workflow = function(workflows,
                        model) {
        
        workflows %>%
                filter(grepl(model, wflow_id)) %>%
                pluck(".workflow", 1)
        
}

# calibration plot

# calculate breaks 
cal_breaks = function(data,
                      outcome,
                      var,
                      n_breaks = 10,
                      width = 0.1,
                      level = 'yes',
                      ...) {
        
        cal_cut_breaks = 
                function(var,
                         ...) {
                                # ggplot2::cut_interval(var, 
                                #                       n = n_breaks,
                                #                       center = 0)
                        
                        ggplot2::cut_width(var,
                                              width = width,
                                              center = 0)
                }
        
        cal_find_point = function(cut_label,
                                  calc = 'min') {
                
                cut_num = cut_label %>%
                        str_replace_all("\\(|\\)|\\[|\\]", "") %>%
                        strsplit(",") %>%
                        unlist() %>%
                        as.numeric() 
                
                if (calc == 'min') {
                        
                        cut_num %>%
                                mean()
                }
                
        }
        
        var = enquo(var)
        outcome = enquo(outcome)
        
        data %>%
                mutate(cut_label = cal_cut_breaks(!!var, n_breaks = n_breaks)) %>%
                group_by(wflow_id, !!outcome, cut_label) %>%
                count() %>%
                ungroup() %>%
                rowwise() %>%
                mutate(pred = cal_find_point(cut_label)) %>%
                group_by(wflow_id, cut_label) %>%
                mutate(obs = n / sum(n)) %>%
                ungroup() %>%
                filter(!!outcome == level)
        
}

# make calibration plot
cal_plot = function(cal_breaks) {
        
        cal_breaks %>%
                ggplot(aes(x=pred,
                           y = obs))+
                geom_point(aes(size = n))+
                geom_line(alpha = 0.5)+
                facet_wrap(wflow_id ~.)+
                coord_obs_pred()+
                geom_abline(slope = 1,
                            linetype = 'dashed',
                            alpha = 0.5)+
                xlab("predicted probability")+
                ylab("observed probability")
        
}

# not run
# predictions %>%
#         cal_breaks(outcome = ever_owned,
#                    var = .pred_yes,
#                    level = 'yes',
#                    n_breaks = 6) %>%
#         cal_plot()


# variable importance from lightgbm
lightgbm_vip = 
        function(lightgbm_wflow, n_features = 25) {
        
        lightgbm_wflow %>%
                extract_fit_engine() %>%
                lightgbm::lgb.importance() %>%
                pivot_longer(cols = -c("Feature"),
                             names_to = c("type"),
                             values_to = c("value")) %>%
                group_by(type) %>%
                slice_max(order_by = value,
                          n = n_features)
}

# plot vip
lightgbm_vip_plot = function(lightgbm_vip) {
        
        lightgbm_vip %>%
                mutate(Feature = bggUtils::present_text(Feature)) %>%
                ggplot(aes(x=value,
                           y = reorder(Feature, value)))+
                geom_col()+
                facet_wrap(type ~.,
                           scales = "free_x")+
                theme_minimal()+
                theme(axis.text.y = element_text(size = 8))+
                ylab("")
        
}

# not run
# lightgbm_wflow %>%
#         lightgbm_vip(n_features = 25) %>%
#         lightgbm_vip_plot()

# extract glmnet coefs
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

# glmnet_wflow %>%
#         glmnet_objs()

# trace plot
glmnet_trace_plot = function(glmnet_wflow) {
        
        # get glmnet objs
        glmnet_objs =
                get_glmnet_objs(glmnet_wflow)
        
        # make data for plot
        plot_data =
                glmnet_objs$engine %>%
                tidy(return_zeroes = T) %>%
                filter(term != '(Intercept)') %>%
                mutate(label_left =
                               case_when(
                                       lambda == min(lambda) & abs(estimate) > 0.08 ~
                                               bggUtils::present_text(term,
                                                                      minlength = 40))) %>%
                group_by(term) %>%
                mutate(label_right =
                               case_when(
                                       lambda == max(lambda) & estimate > -2 ~
                                               bggUtils::present_text(term,
                                                                      minlength = 40))) %>%
                ungroup()
        
        # make plot
        plot_data %>%
                ggplot(aes(x=log(lambda),
                           y=estimate,
                           group = term))+
                geom_line(alpha = 0.5,
                          color = 'grey60')+
                guides(color = 'none')+
                theme_minimal()+
                geom_vline(xintercept = log(0.0129),
                           linetype = 'dotted')+
                ggrepel::geom_text_repel(
                        aes(label = label_left),
             #           fontface = "bold",
                        size = 2,
                        direction = "y",
                        hjust =1.5,
                        segment.size = .5,
                        segment.alpha = .5,
                        segment.linetype = "dashed",
                        box.padding = .5,
                        segment.curvature = 0.2,
                        segment.ncp = 3,
                        max.overlaps = 15,
                        segment.angle = 20)+
                coord_cartesian(xlim = c(min(log(glmnet_objs$engine$lambda)-2), 0))+
                theme(panel.grid.major = element_blank())+
                geom_hline(yintercept = 0,
                           linetype = 'dotted',
                           alpha = 0.5)
        # ggrepel::geom_text_repel(
        #         aes(label = label_right),
        #         fontface = "bold",
        #         size = 2,
        #         direction = "y",
        #         hjust = -.5,
        #         segment.size = .7,
        #         segment.alpha = .5,
        #         segment.linetype = "dotted",
        #         box.padding = .5,
        #         segment.curvature = 0.2,
        #         segment.ncp = 3,
        #         segment.angle = 20)
        
}

# coef plot
glmnet_coef_plot = function(glmnet_wflow,
                            point = T) {
        
        # get glmnet objs
        glmnet_objs =
                get_glmnet_objs(glmnet_wflow)
        
        # make coef plot
        plot = glmnet_objs %$%
                parsnip %>%
                tidy() %>%
                filter(term != '(Intercept)') %>%
                group_by(sign = factor(case_when(estimate > 0 ~ 'increases probability',
                                                 estimate < 0 ~ 'decreases probability'),
                                       levels = c("increases probability", "decreases probability"))) %>%
                filter(estimate !=0) %>%
                slice_max(abs(estimate),
                          n = 25,
                          with_ties = F)  %>%
                mutate(term = bggUtils::present_text(term)) %>%
                mutate(sign = factor(sign,
                                     levels = c("decreases probability", "increases probability"))) 
        
        if (point == T) {
                
                plot %>%
                        ggplot(aes(x=estimate,
                           fill = estimate,
                           color = estimate,
                           y= tidytext::reorder_within(term, estimate, sign)))+
                      #  geom_point()+
                        theme_minimal()+
                        #theme(panel.grid.major = element_line(alpha = 0.5))+
                        theme(axis.text.y = element_text(size = 8))+
                        scale_color_gradient2(low = 'red',
                                              mid = 'grey60',
                                              high = 'dodgerblue2',
                                              midpoint = 0,
                                              limits = c(-0.1, 0.1),
                                              oob = scales::squish)+
                        scale_fill_gradient2(low = 'red',
                                              mid = 'grey60',
                                              high = 'dodgerblue2',
                                              midpoint = 0,
                                              limits = c(-0.1, 0.1),
                                              oob = scales::squish)+
                        ylab("")+
                        tidytext::scale_y_reordered()+
                        geom_vline(xintercept = 0,
                                   linetype = 'dashed')+
                        theme_bgg()+
                        theme(legend.title = element_text())+
                        guides(color = guide_colorbar(barheight = 0.5,
                                                      barwidth = 15,
                                                      title.position = 'top'),
                               fill = 'none')+
                        labs(color = paste('Decreases Probability',
                                           '      ',
                                           'Increases Probability'))
                
                
        } else {
                
                ggplot(aes(x=estimate,
                           fill = estimate,
                           color = estimate,
                           y= tidytext::reorder_within(term, estimate, sign)))+
                        geom_col()+
                        theme_minimal()+
                        #theme(panel.grid.major = element_line(alpha = 0.5))+
                        theme(axis.text.y = element_text(size = 8))+
                        scale_color_gradient2(low = 'red',
                                              mid = 'grey60',
                                              high = 'dodgerblue2',
                                              midpoint = 0,
                                              limits = c(-0.1, 0.1),
                                              oob = scales::squish)+
                        scale_fill_gradient2(low = 'red',
                                             mid = 'grey60',
                                             high = 'dodgerblue2',
                                             midpoint = 0,
                                             limits = c(-0.1, 0.1),
                                             oob = scales::squish)+
                        ylab("")+
                        facet_wrap(~sign,
                                   ncol = 2,
                                   scales = "free")+
                        tidytext::scale_y_reordered()
        }
        
        
}

# # trace plot
# plot_trace_glmnet(glmnet_wflow)
# 
# # coef plot
# plot_coef_glmnet(glmnet_wflow)

# top games by year
