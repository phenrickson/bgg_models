
# function to compute shapley values for new data given a workflow
shap_explain = function(workflow,
                        newdata,
                        ids = c("game_id", "name", "yearpublished")) {
        
        # helper functions
        # extract model objects from workflow
        shap_extract_objs = function(workflow) {
                
                # model
                fit = workflow %>%
                        extract_fit_engine()
                
                # recipe
                recipe = workflow %>%
                        extract_recipe()
                
                # template
                template = workflow %>% 
                        extract_preprocessor() %$%
                        template
                
                # predictors
                predictors = workflow$pre$mold$predictors
                
                # ids
                ids = workflow$pre$mold$extras$roles$id
                
                # outcomes
                outcomes = workflow$pre$mold$outcomes
                
                # return
                shap_objs = list("fit" = fit,
                                 "recipe" = recipe,
                                 "template" = template,
                                 "predictors" = predictors,
                                 "ids" = ids,
                                 "outcomes" = outcomes)
                
                return(shap_objs)
                
        }
        
        # prep newdata given recipe
        shap_prep_newdata = function(shap_objs,
                                     newdata,
                                     ids) {
                
                # get all ids
                newdata_ids = newdata %>%
                        select(all_of(names(shap_objs$ids))) %>%
                        select(one_of(ids))
                
                # get all outcomes
                newdata_outcomes = newdata %>%
                        select(all_of(names(shap_objs$outcomes)))
                
                # bake with recipe
                newdata_baked = shap_objs %$%
                        recipe %>%
                        bake(newdata)
                
                # matrix of predictors
                newdata_mat = newdata_baked %>%
                        select(all_of(names(shap_objs$predictors))) %>%
                        as.matrix()
                
                return(list("ids" = newdata_ids,
                            "outcomes" = newdata_outcomes,
                            "baked" = newdata_baked,
                            "mat" = newdata_mat))
                
        }
        
        # shap explain newdata
        shap_explain_newdata = function(shap_objs,
                                        newdata_objs) {
                
                
                # pred wrapper
                pfun <- function(object, newdata) {
                        predict(object, data = newdata)
                }
                
                # shapley values for new record
                shap = fastshap::explain(
                        # model obj
                        object = shap_objs$fit,
                        # newdata matrix
                        newdata = newdata_objs$mat,
                        # exact values
                        exact = T)
                
                return(shap)
                
        }
        
        # pivot shap results and get actual
        shap_longer = function(shap,
                               newdata_objs) {
                
                # id cols to vind in 
                id_cols = newdata_objs$ids
                
                # bind shapley values with id cols
                bind_cols(id_cols,
                          as.data.frame(shap)) %>%
                        pivot_longer(cols = -c(names(id_cols)),
                                     values_to = 'contribution',
                                     names_to = 'feature')
                
        }
        
        # pivot newdata baked results
        newdata_longer = function(newdata_objs) { 
                
                
                # id cols to vind in 
                id_cols = newdata_objs$ids
                
                # bind shapley values with id cols
                bind_cols(id_cols,
                          newdata_objs$mat) %>%
                        pivot_longer(cols = -c(names(id_cols)),
                                     values_to = 'value',
                                     names_to = 'feature')
        }
        
        # extract objects from workflow needed for shapley values
        shap_objs = shap_extract_objs(workflow)
        
        # prep new data for use with mdodel
        newdata_objs = shap_prep_newdata(shap_objs,
                                         newdata,
                                         ids)
        
        # outcome
        outcome = names(shap_objs$outcomes)
        
        # compute exact shap explanation for new data
        shap = shap_explain_newdata(shap_objs,
                                    newdata_objs)
        
        # pivot longer shapley values
        shap_long = shap_longer(shap, 
                                newdata_objs)
        
        # pivot longer newdata
        newdata_long = newdata_longer(newdata_objs)
        
        # combine
        shap_explained = shap_long %>%
                left_join(.,
                          newdata_long,
                          by = c(ids, "feature")) %>%
                mutate(feature_value = paste(feature,
                                             "=",
                                             round(value, 2))
                ) %>%
                mutate(outcome = outcome) %>%
                select(all_of(ids),
                       outcome,
                       feature,
                       feature_value,
                       contribution)
        
        # dataframe of obs with outcome, .pred, and .actual
        newdata_pred =
                # pred
                workflow %>%
                augment(newdata) %>%
                select(one_of(ids), .pred) %>% 
                left_join(.,
                          # actual
                          newdata %>%
                                  select(one_of(ids, outcome)) %>%
                                  rename(.actual = !!enquo(outcome)),
                          by = ids
                ) %>%
                mutate(outcome = paste(outcome))
        

        
        return(        
                list("shap_explained" = shap_explained,
                     "newdata_pred" = newdata_pred)
        )
        
}


# # predict
# games_upcoming_est = 
#         games_upcoming %>%
#         impute_averageweight(averageweight_mod = averageweight_fit)

# test function
# foo = shap_explain(average_fit,
#              newdata = games_upcoming_est %>%
#                      sample_n(1))

# bar = map(seq(1990, 2020),
#           ~ usersrated_fit %>%
#                   augment(
#                           games_upcoming_est %>%
#                                   mutate(yearpublished = .x) %>%
#                                   filter(name == 'Cascadia')) %>%
#                   select(game_id, name, yearpublished, .pred)) %>%
#         bind_rows()
# 
# bar %>%
#         ggplot(aes(x=yearpublished,
#                    y=exp(.pred)))+
#         geom_point()+
#         theme_bw()


# prepare data for shapley plot
shap_prep_plot = function(shap) {
        
        suppressWarnings({

                        shap$newdata_pred =
                                shap$newdata_pred %>%
                                mutate(.actual = case_when(is.infinite(.actual) ~ 0,
                                                           is.na(.actual) ~ 0,
                                                           TRUE ~ .actual)) %>%
                                mutate(.pred = plyr::round_any(exp(.pred), 50, f = ceiling),
                                       .actual = plyr::round_any(exp(.actual), 100))
                        #%>%
                        # mutate(.pred = plyr::round_any(exp(.pred), 100, f = ceiling),
                        #        .actual = plyr::round_any(exp(.pred), 100))
                        
                }
        
        )
        
        return(shap)
        
}

# make shot plot for a category
shap_explain_plot = function(shap,
                             top_features = 25) {
        
        # prep for plot
        shap = shap_prep_plot(shap)
        
        # slice top n features
        shap_slice =  shap %$%
                shap_explained %>%
                mutate(feature_value = bggUtils::present_text(feature_value)) %>%
                slice_max(n = top_features, 
                          order_by = abs(contribution),
                          with_ties = F)
        
        # make plot
        shap_slice %>%
                {
                        ggplot(.,
                               aes(x=contribution,
                                   fill = contribution,
                                   y=reorder(feature_value, contribution)))+
                                geom_col()+
                                geom_vline(xintercept = 0)+
                                #   coord_cartesian(xlim = c(-0.5, 0.5))+
                                ggtitle(
                                        paste(
                                                paste("Game:", .$name[1]),
                                                paste("ID:", .$game_id[1]),
                                                sep = "\n"
                                        )
                                ) +
                                # set theme
                                theme_minimal()+
                                # theme_set(theme_gray(base_family = "DejaVuSerif"))+
                                facet_wrap(facets = paste(
                                        paste(str_to_title(shap$newdata_pred$outcome)),
                                        paste('Estimated:', format(round(shap$newdata_pred$.pred, 2), 
                                                                   nsmall = 2)),
                                        paste('Current:', format(round(shap$newdata_pred$.actual, 2),
                                                                 nsmall = 2)),
                                        sep = "\n")~.)+
                                # set the subtitle align
                                theme(plot.subtitle = element_text(hjust = 0)) +
                                # set the color
                                scale_fill_gradient2(low = "red",
                                                     mid = "grey80",
                                                     high = "blue",
                                                     midpoint = 0,
                                                     limits = c(-0.2, 0.2),
                                                     oob = scales::squish)+
                                guides(fill ="none")+
                                ylab("Feature")
                        
                }
}

# # make grid
# shap_explain(usersrated_fit,
#              newdata = games_upcoming_est %>%
#                      mutate(yearpublished = 2000) %>%
#                   #   filter(grepl('Oathsworn', name)) %>%
#                      # filter(game_id == 361640)) %>%
#                      sample_n(1)) %>%
#         shap_explain_plot(year = F)
# 
# # test functoin on notable game
# shap_explain(average_fit,
#              newdata = games_upcoming_est %>%
#                      # filter(game_id == 361640)) %>%
#                      sample_n(1)) %>%
#         shap_explain_plot(year = F)
# 
# shap_explain(average_fit,
#              newdata = games_upcoming_est %>%
#                      # filter(game_id == 361640)) %>%
#                      sample_n(1)) %>%
#         shap_explain_plot(year = F)


