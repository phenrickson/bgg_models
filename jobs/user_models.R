
# setup -------------------------------------------------------------------

# run setup for user models
source(here::here("src", "models", "setup_user_model.R"))


# register parallel -------------------------------------------------------

# set parallel
library(doParallel)
all_cores <- parallel::detectCores(logical = FALSE)
doMC::registerDoMC(cores = all_cores)

# parameters for user training
username = 'mrbananagrabber'
end_train_year = 2020
valid_window = 2
retrain_window = 1
outcome = 'ever_owned'


# run ---------------------------------------------------------------------


message("fetching user collection...")

# create user objects for training
user_collection =
        assemble_user_collection(username,
                                 games_imputed)

message(paste("setting up user models for ", '`', outcome,  '`', "...", sep=""))

user_train_setup = 
        make_user_split_and_resamples(user_collection,
                              games = games_imputed,
                              end_train_year = end_train_year,
                              valid_window = valid_window,
                              outcome = outcome,
                              min_users = 30)

message("creating user recipes...")

# get train and valid
train =  user_train_setup$user_split$train
valid = user_train_setup$user_split$valid

# create user recipes
user_recipes = 
        make_user_recipes(data = user_train_setup$user_split$train,
                          outcome = user_train_setup$outcome)

# model specs to tune over
models_list =
        mget(grep("_class_spec",
                  names(.GlobalEnv),
                  value=TRUE)) %>%
        # extract names
        magrittr::set_names(., gsub("_class_spec", "", names(.)))

# user wflows
user_wflows = 
        workflow_set(
                preproc =        
                        list(
                        minimal = user_recipes$minimal_splines,
                        impute = user_recipes$all_impute,
                        splines = user_recipes$all_impute_splines,
                        pca = user_recipes$all_impute_splines %>%
                                step_rm(contains("year_")) %>%
                                step_pca(all_numeric_predictors(),
                                         threshold = 0.75,
                                         id = 'pca'),
                        corr = user_recipes$all_impute_splines %>%
                                step_corr(all_numeric_predictors()),
                        trees = user_recipes$all_trees
                ),
                models =
                        models_list,
                cross = T
        ) %>%
        # keep only specified wflows
        filter(wflow_id %in% 
                       c(
                               #"minimal_impute_glmnet",
                               "minimal_glmnet",
                               # "all_impute_glmnet",
                               "impute_mars",
                               "splines_glmnet",
                               #    "pca_glmnet",
                               "pca_knn",
                               "corr_glm",
                               "trees_cart",
                               "trees_xgb",
                               "trees_lightgbm")
        )

# tune --------------------------------------------------------------------

# glm
tictoc::tic("tuning glm...")
set.seed(1999)
glm_res =
        user_wflows %>%
        filter(grepl("corr_glm", wflow_id)) %>%
        workflow_map(
                fn = 'tune_grid',
                resamples = user_train_setup$user_train_resamples,
                metrics = prob_metrics,
                control = ctrl_grid
                #  ,grid = glmnet_grid
        )
tictoc::toc()


# glmnet
tictoc::tic("tuning glmnet...")
set.seed(1999)
glmnet_res =
        user_wflows %>%
        filter(grepl("splines_glmnet", wflow_id)) %>%
        workflow_map(
                fn = 'tune_grid',
                resamples = user_train_setup$user_train_resamples,
                metrics = prob_metrics,
                control = ctrl_grid
              #  ,grid = glmnet_grid
        )
tictoc::toc()

# # cart
# tictoc::tic("tuning cart...")
# set.seed(1999)
# cart_res =
#         user_wflows %>%
#         filter(grepl("cart", wflow_id)) %>%
#         workflow_map(
#                 fn = 'tune_grid',
#                 resamples = user_train_setup$user_train_resamples,
#                 metrics = prob_metrics,
#                 control = ctrl_grid
#         )
# tictoc::toc()

# 
# # mars
# tictoc::tic("tuning mars...")
# mars_res =
#         user_wflows %>%
#         filter(grepl("mars", wflow_id)) %>%
#         workflow_map(
#                 fn = 'tune_grid',
#                 resamples = user_train_setup$user_train_resamples,
#                 metrics = prob_metrics,
#                 control = ctrl_grid,
#                 grid = mars_grid)
# tictoc::toc()

# # knn
# tictoc::tic("tuning knn...")
# knn_res =
#         user_wflows %>%
#         filter(grepl("knn", wflow_id)) %>%
#         workflow_map(
#                 fn = 'tune_grid',
#                 resamples = user_train_setup$user_train_resamples,
#                 metrics = prob_metrics,
#                 control = ctrl_grid,
#                 grid = knn_grid
#         )
# tictoc::toc()

# lightgbm
tictoc::tic("tuning lightgbm...")
set.seed(1999)
lightgbm_res =
        user_wflows %>%
        filter(grepl("lightgbm", wflow_id)) %>%
        workflow_map(
                fn = 'tune_grid',
                resamples = user_train_setup$user_train_resamples,
                metrics = prob_metrics,
                control = ctrl_grid
        )
tictoc::toc()



# resamples -----------------------------------------------------------------


# get results from training
training_results  =
        mget(grep("_res$",
                         names(.GlobalEnv),
                         value=TRUE)) %>%
        bind_rows()  %>%
        check_results

# training predictions
training_preds = 
        training_results %>%
        collect_predictions(select_best = T,
                            metric = 'mn_log_loss') %>%
        left_join(.,
                  user_train_setup$user_split$train %>%
                          mutate(.row = row_number()) %>%
                          select(.row, game_id, name, yearpublished)) %>%
        trim_results

# tuning results
training_results %>%
        rank_results(rank_metric = 'mn_log_loss',
                     select_best = T) %>%
        select(wflow_id, .config, .metric, mean, std_err, n, rank) %>%
        arrange(.metric, rank)


# validation results --------------------------------------------------------------


# fit on train
set.seed(1999)
train_fits =
        training_results %>%
        finalize_fits(metric = 'mn_log_loss')

# valid predictions
valid_preds =
        train_fits %>%
        get_predictions()

# valid metrics
valid_metrics = 
        train_fits %>%
        get_metrics()

# #
valid_preds %>%
        filter(wflow_id == 'trees_lightgbm') %>%
        arrange(desc(.pred_yes))
#
valid_preds %>%
        filter(wflow_id == 'splines_glmnet') %>%
        arrange(desc(.pred_yes))


# upcoming ----------------------------------------------------------------


# make final test split
test_split = 
        split_collection(data = user_collection,
                         end_train_year = end_train_year + retrain_window,
                         valid_window = 4,
                         min_users = 30)

# filter test set to remove those that dont meet hurdle
test_split$valid =
        test_split$valid %>%
        filter(pred_hurdle > .25)

# create test split
test_split_rsample  = 
        test_split %>%
        split_rsample()

# fit on train-valid, assess on test
final_fits = 
        train_fits %>%
        mutate(.workflow = map(last_fit,
                               ~ .x %>%  extract_workflow())) %>%
        mutate(outcome = outcome) %>%
        select(wflow_id, outcome, .workflow) %>%
        mutate(last_fit = 
                       map(.workflow,
                           ~ .x %>% 
                                   last_fit(test_split_rsample,
                                            metrics = prob_metrics,
                                            control = ctrl_last_fit)))

# get results so far
final_fits %>%
        get_metrics()

# input
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

# # get fits
# cart = 
#         final_fits %>%
#         filter(wflow_id == 'trees_cart') %>%
#         pluck("last_fit", 1) %>%
#         extract_workflow() %>%
#         extract_fit_engine()

# 
# rpart.plot::rpart.plot(cart,
#                        roundint=F)



glmnet_wflow =  
        final_fits %>%
        filter(wflow_id == 'splines_glmnet') %>%
        pluck("last_fit", 1) %>%
        extract_workflow()
        

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

glmnet_wflow %>%
        get_glmnet_objs()

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

# trace plot
plot_trace_glmnet(glmnet_wflow)

# coef plot
plot_coef_glmnet(glmnet_wflow)


# variable importance from lightgbm
lightgbm = 
        final_fits %>%
        filter(wflow_id == 'trees_lightgbm') %>%
        pluck("last_fit", 1) %>%
        pluck(".workflow", 1) %>%
        extract_fit_engine()

# variable importance
lightgbm %>%
        lgb.importance() %>%
        pivot_longer(cols = -c("Feature"),
                     names_to = c("type"),
                     values_to = c("value")) %>%
        group_by(type) %>%
        slice_max(order_by = value,
                  n = 25) %>%
        mutate(Feature = present_text(Feature)) %>%
        ggplot(aes(x=value,
                   y = reorder(Feature, value)))+
        geom_col()+
        facet_wrap(type ~.,
                   scales = "free_x")+
        theme_minimal()+
        theme(axis.text.y = element_text(size = 8))+
        ylab("")

# get predictions
final_fits %>%
        get_predictions %>%
        filter(wflow_id == 'splines_glmnet') %>%
        select(yearpublished, game_id, name, .pred_yes, ever_owned) %>%
        print(n=25)


# get predictions
final_fits %>%
        get_predictions %>%
        filter(wflow_id == 'trees_lightgbm') %>%
        select(yearpublished, game_id, name, .pred_yes, ever_owned) %>%
        print(n=25)

final_fits %>%
        get_predictions %>%
        filter(wflow_id == 'trees_lightgbm') %>%
        filter(yearpublished == 2022) %>%
        select(yearpublished, game_id, name, .pred_yes, ever_owned) %>%
        print(n=25)

final_fits %>%
        get_predictions %>%
        filter(wflow_id == 'trees_lightgbm') %>%
        filter(yearpublished == 2023) %>%
        select(yearpublished, game_id, name, .pred_yes, ever_owned) %>%
        print(n=25)


final_fits %>%
        get_predictions %>%
        filter(wflow_id == 'trees_lightgbm') %>%
        filter(yearpublished == 2024) %>%
        select(yearpublished, game_id, name, .pred_yes, ever_owned) %>%
        print(n=25)

