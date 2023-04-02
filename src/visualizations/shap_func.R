# input
present_text = function(x) {
        
        x %>%
                str_replace("_", " ") %>%
                str_to_title()
        
        x = gsub("Cut Playing Time X", "Playing Time", x)
        x = gsub("Rockpaperscissors", "Rock Paper Scissors", x)
        x = gsub("Collectible Collectible", "Collectible", x)
        x = gsub("Murdermystery", "Murder Mystery", x)
        x = gsub("Bgg Average", "BGG Average", x)
        x = gsub("World War Ii", "World War II", x)
        x = gsub("Gemscrystals", "Gems & Crystals", x)
        x = gsub("History ", "", x)
        x = gsub("Gmt", "GMT", x)
        x = gsub("Cmon", "CMON", x)
        x = gsub("Zman", "ZMan", x)
        x = gsub("Movies Tv", "Movies TV", x)
        x = gsub("Auctionbidding", "Auction Bidding", x)
        x = gsub("Postnapoleonic", "Post Napoleonic", x)
        x = gsub("Paperandpencil", "Paper And Pencil", x)
        x = gsub("Digital Hybrid Appwebsite Required", "Digital Hybrid App", x)
        x = gsub("3dimensional", "", x)
        x = gsub("3d", "3D", x)
        x = gsub("Usa", "USA", x)
        x = gsub("3D", "3D Components", x)
        x = gsub("Averageweight", "Average Weight", x)
        x = gsub("Wizkids I", "WizKids", x)
        x = gsub("Decision Kids I", "Decision Kids", x)
        x = gsub("Selfpublished", "Self-Published", x)
        x = gsub("Gamewright", "GameWright", x)
        x = gsub("Eaglegryphon", "Eagle-Gryphon", x)
        x = gsub("Scoreandreset", "Score and Reset", x)
        x
}

# function to compute shapley values for new data
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
                select(all_of(ids),
                       feature,
                       feature_value,
                       contribution)
        
        # outcome
        outcome = names(shap_objs$outcomes)
        
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
        
        # outcome
        outcome = names(shap_objs$outcomes)

        return(        
                list("shap_explained" = shap_explained,
                     "newdata_pred" = newdata_pred)
        )
        
}


# predict
games_upcoming_est = 
        games_upcoming %>%
        impute_averageweight(averageweight_mod = averageweight_fit)

# test function
# foo = shap_explain(average_fit,
#              newdata = games_upcoming_est %>%
#                      sample_n(1))

# test functoin on notable game
foo = shap_explain(average_fit,
                   newdata = games_upcoming_est %>%
                           filter(name == 'Frosthaven')
)

# plot
foo %$%
        shap_explained %>%
        slice_max(n = 25, 
                  order_by = abs(contribution),
                  with_ties = F) %>%
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
                        facet_wrap(facets = paste(
                                paste(str_to_title(foo$newdata_pred$outcome)),
                                paste('Estimated:', format(round(foo$newdata_pred$.pred, 2), 
                                                           nsmall = 2)),
                                paste('Current:    ', format(round(foo$newdata_pred$.actual),
                                                             nsmall = 2)),
                                sep = "\n")~.)+
                        # set the subtitle align
                        theme(plot.subtitle = element_text(hjust = 0)) +
                        # set the color
                        scale_fill_gradient2(low = "red",
                                             mid = "grey60",
                                             high = "blue",
                                             midpoint = 0,
                                             limits = c(-0.1, 0.1),
                                             oob = scales::squish)+
                        guides(fill ="none")
                
        }
        
        
        

# make shapley plot
# extract pieces needed from workflow
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

# prep new obs given recipe
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


# plot
shap_plot = function(shap,
                     newdata_objs) {
        
        
        
        
}