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
#         get_glmnet_objs()

# trace plot
plot_trace_glmnet = function(glmnet_wflow) {
        
        # get glmnet objs
        glmnet_objs = 
                get_glmnet_objs(glmnet_wflow)
        
        # make data for plot
        dat = 
                glmnet_objs$engine %>%
                tidy(return_zeroes = T) %>%
                filter(term != '(Intercept)') %>%
                mutate(label_left = 
                               case_when(
                                       lambda == min(lambda) & abs(estimate) > 0.1 ~
                                               present_text(term))) %>%
                group_by(term) %>%
                mutate(label_right = 
                               case_when(
                                       lambda == max(lambda) & estimate > -2 ~ 
                                               present_text(term))) %>%
                ungroup()
        
        # make plot
        dat %>%
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
                        fontface = "bold",
                        size = 2,
                        direction = "y",
                        hjust =1.5,
                        segment.size = .7,
                        segment.alpha = .5,
                        segment.linetype = "dotted",
                        box.padding = .5,
                        segment.curvature = 0.2,
                        segment.ncp = 3,
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


plot_coef_glmnet = function(glmnet_wflow) {
        
        # get glmnet objs
        glmnet_objs = 
                get_glmnet_objs(glmnet_wflow)
        
        # make coef plot
        glmnet_objs %$%
                parsnip %>%
                tidy() %>%
                filter(term != '(Intercept)') %>%
                group_by(sign = factor(case_when(estimate > 0 ~ 'positive',
                                                 estimate < 0 ~ 'negative'),
                                       levels = c("positive", "negative"))) %>%
                filter(estimate !=0) %>%
                slice_max(abs(estimate),
                          n = 25,
                          with_ties = F)  %>%
                mutate(term = present_text(term)) %>%
                ggplot(aes(x=estimate,
                           color = estimate,
                           y= reorder(term, estimate)))+
                geom_point()+
                theme_minimal()+
                #theme(panel.grid.major = element_line(alpha = 0.5))+
                theme(axis.text.y = element_text(size = 8))+
                scale_color_gradient2(low = 'red',
                                      mid = 'grey60',
                                      high = 'dodgerblue2',
                                      midpoint = 0,
                                      limits = c(-0.1, 0.1),
                                      oob = scales::squish)+
                ylab("")+
                geom_vline(xintercept = 0)
        
        # facet_wrap(sign~.,
        #            ncol = 1,
        #            scales = "free_y")
        
}

present_text = function(x,
                       minlength = 40) {
        
        suppressWarnings({
                x %>%
                        str_replace_all("_", " ") %>%
                        str_to_title() %>%
                        str_replace("Minage", "Min Age") %>%
                        str_replace("Minplayers", "Min Players") %>%
                        str_replace("Maxplayers", "Max Players") %>%
                        str_replace("Minplaytime", "Min Play Time") %>%
                        str_replace("Maxplaytime", "Max Play Time") %>%
                        str_replace("Playingtime", "Play Time") %>%
                        str_replace("Usa", "USA") %>%
                        str_replace("Averageweight", "Average Weight") %>%
                        str_replace("Usersrated", "Users Rated") %>%
                        str_replace("Rockpaperscissors", "Rock Paper Scissors") %>%
                        str_replace("Collectible Collectible", "Collectible") %>%
                        str_replace("Murdermystery", "Murder Mystery") %>%
                        str_replace("World War Ii", "World War II") %>%
                        str_replace("Gemscrystals", "Gems & Crystals") %>%
                        str_replace("Gmt", "GMT") %>%
                        str_replace("Cmon", "CMON") %>%
                        str_replace("Zman", "ZMan") %>%
                        str_replace("Movies Tv", "Movies TV") %>%
                        str_replace("Auctionbidding", "Auction Bidding") %>%
                        str_replace("Postnapoleonic", "Post Napoleonic") %>%
                        str_replace("Paperandpencil", "Paper And Pencil") %>%
                        str_replace("Digital Hybrid Appwebsite Required", "Digital Hybrid App") %>%
                        str_replace("Components 3 Dimensional", "Components") %>%
                        str_replace("3d", "3D") %>%
                        str_replace("Usa", "USA") %>%
                        str_replace("3D", "3D Components") %>%
                        str_replace("Wizkids I", "WizKids") %>%
                        str_replace("Decision Kids I", "Decision Kids") %>%
                        str_replace("^Families", "Fam:") %>%
                        str_replace("^Mechanics", "Mech:") %>%
                        str_replace("^Categories", "Cat:") %>%
                        str_replace("^Publishers", "Pub:") %>%
                        str_replace("^Designers", "Des:") %>%
                        str_replace("^Artists", "Art:") %>%
                        str_replace("Selfpublished", "Self-Published") %>%
                        str_replace("Gamewright", "GameWright") %>%
                        str_replace("Eaglegryphon", "Eagle-Gryphon") %>%
                        str_replace("Scoreandreset", "Score and Reset") %>%
                        abbreviate(minlength = minlength)
        })
        
}


# # trace plot
# plot_trace_glmnet(glmnet_wflow)
# 
# # coef plot
# plot_coef_glmnet(glmnet_wflow)

