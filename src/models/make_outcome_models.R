# what: train a series of models to predict outcomes on bgg

# dependencies:
# load analysis tables (locally) or run query

# packages ----------------------------------------------------------------

# load core packages needed for modeling
suppressPackageStartupMessages({

        # tidyverse/modeling
        library(tidyverse)
        library(tidymodels)
        library(tidyselect)

        # recipes and workflows
        library(recipes)
        library(textrecipes)
        library(workflows)
        library(workflowsets)

        # finetune
        library(finetune)
        library(bonsai)
        library(plsmod)

        # prediction intervals
        library(spin)

        # pins
        library(pins)

        # vetiver
        library(vetiver)

        # get my own package
        #devtools::install_github("phenrickson/bggUtils")
        library(bggUtils)

}
)

# set conflict preferences
tidymodels_prefer()
conflicted::conflicts_prefer(purrr::flatten)
conflicted::conflicts_prefer(purrr::set_names)


# src ---------------------------------------------------------------------

# functions used for preprocessing data
source(here::here("src", "features", "preprocess_games.R"))

# functions used for making recipes for outcome models
source(here::here("src", "features", "recipes_outcomes.R"))

# functions used in training outcome models
source(here::here("src", "models", "train_outcomes.R"))


# data --------------------------------------------------------------------


# load games with imputed averageweight data for modeling
games_raw =
        pins::pin_read(
                board = pins::board_folder(here::here("data", "processed")),
                name = "games")

# use function to preprocess nested data
games_processed =
        games_raw %>%
        # apply preprocessing from function
        preprocess_games() %>%
        # add outcome for hurdle model
        add_users_threshold()


# parameters
end_train_year = 2020
valid_window = 2

# train model to impute averageweight
averageweight_fit =
        games_processed %>%
        train_outcome_model(
                data = .,
                end_train_year = end_train_year,
                valid_window = valid_window,
                outcome = averageweight,
                models = tree_models(),
          #      models = list("lm" = linear_models() %$% lm),
                metrics = reg_metrics(),
                remove_wflow_ids = "^trees_",
                tune_method = "tune_race_anova"
        )

# impute with model
games_imputed =
        games_processed %>%
        impute_averageweight(
                data = .,
                fit = averageweight_fit$train_fit
        )

# now train average model, updating averageweight as an id var
outcomes = c("average", "log_usersrated")

outcome_fits =
        map(outcomes,
            ~ games_imputed %>%
                            train_outcome_model(
                                    data = .,
                                    end_train_year = end_train_year,
                                    valid_window = valid_window,
                                    outcome = .x,
                                    models = list("lm" = linear_models() %$% lm),
                                    metrics = reg_metrics(),
                                    ids = c(id_vars(), "averageweight"),
                                    predictors = c(predictor_vars(), "est_averageweight"),
                                    splines = c(spline_vars(), "est_averageweight"),
                                    remove_wflow_ids = "^trees_",
                                    tune_method = "tune_race_anova"
                            )
                    )

# set names of list
names(outcome_fits) = outcomes

# combine predictions for geek rating on validation set
training_preds =
        games_imputed %>%
        filter(yearpublished <= end_train_year) %>%
        predict_average(
                workflow = outcome_fits$average$train_fit
        ) %>%
        predict_usersrated(
                workflow = outcome_fits$log_usersrated$train_fit
        ) %>%
        calculate_bayesaverage()

# training_preds %>%
#         select(yearpublished, game_id, name, average, usersrated, bayesaverage, starts_with(".pred")) %>%
#         arrange(desc(.pred_bayesaverage)) %>%
#         ggplot(aes(x=.pred_bayesaverage,
#                    label = name,
#                    y=bayesaverage))+
#         geom_point()+
#         geom_text(check_overlap = T,
#                   vjust = -0.5,
#                   size = 2.5)+
#         coord_obs_pred()+
#         geom_abline()

set_metrics = reg_metrics()

training_preds %>%
        select(.pred_bayesaverage, bayesaverage) %>%
        set_metrics(bayesaverage,
                    .pred_bayesaverage)

# # simulate validation set with linear models
# 
# #doParallel::registerDoParallel(cores = parallel::detectCores())
# # future::plan(strategy = "multisession", workers = 4)
# tictoc::tic()
# sims = games_processed %>%
#         filter(yearpublished > end_train_year & yearpublished <= end_train_year + valid_window) %>%
#         simulate_outcomes(data =.,
#                          averageweight_fit$train_fit,
#                          outcome_fits$average$train_fit,
#                          outcome_fits$log_usersrated$train_fit,
#                          sims = 100)
# tictoc::toc()
# #doParallel::stopImplicitCluster()
# 
# 
# examine performance on validation set
valid_preds =
        games_imputed %>%
        filter(yearpublished > end_train_year & yearpublished <= end_train_year + valid_window) %>%
        mutate(bayesaverage = replace_na(bayesaverage, 5.5)) %>%
        predict_average(
                workflow = outcome_fits$average$train_fit
        ) %>%
        predict_usersrated(
                workflow = outcome_fits$log_usersrated$train_fit
        ) %>%
        calculate_bayesaverage()

# save results/predictions locally


# deploy final models with vetiver to gcs


# 
# 
# library(tidytext)
# 
# summarize_sims = 
#         sims %>%
#         # inner_join(.,
#         #            games_processed %>%
#         #                    filter(yearpublished == 2021) %>%
#         #                    filter(!is.na(bayesaverage)) %>%
#         #                    sample_n(25)) %>%
#         rename(.pred_averageweight = est_averageweight) %>%
#         mutate(.pred_usersrated = round_usersrated(exp(.pred_usersrated))) %>%
#         calculate_bayesaverage() %>%
#         select(sim, game_id, name, starts_with(".pred")) %>%
#         pivot_longer(cols = starts_with(".pred"),
#                      names_to = c("outcome"),
#                      names_prefix = c("^.pred_"),
#                      values_to = c(".pred")) %>%
#         group_by(outcome, game_id, name) %>%
#         reframe(
#                 pred = quantile(.pred, c(0.05, 0.5, 0.95)), interval = c("lwr", "mid", "upr")
#         ) %>%
#         mutate(pred = case_when(outcome == 'usersrated' ~ log1p(pred),
#                                 TRUE ~ pred)) %>%
#         pivot_wider(id_cols = c("outcome", "game_id", "name"),
#                     names_from = c("interval"),
#                     values_from = c("pred")) %>%
#         left_join(.,
#                   games_processed %>%
#                           mutate(game_id,
#                                  average,
#                                  averageweight,
#                                  bayesaverage = replace_na(bayesaverage, 5.5),
#                                  usersrated = case_when(is.infinite(log_usersrated) ~ 0,
#                                                         TRUE ~ log_usersrated),
#                                  .keep = 'none') %>%
#                           pivot_longer(
#                                   cols = c(average, averageweight, bayesaverage, usersrated),
#                                   names_to = c("outcome"),
#                                   values_to = c("actual")
#                           )
#         )
# 
# 
# assess_metrics = reg_metrics()
# 
# summarize_sims %>%
#         left_join(.,
#                   games_imputed %>%
#                           select(game_id, yearpublished)) %>%
#         filter(yearpublished == 2021) %>%
#         ggplot(aes(x=mid,
#                    y=actual))+
#         geom_point(alpha = 0.5,
#                    size = 0.5)+
#         facet_wrap(outcome ~.,
#                    ncol = 2,
#                    scales = "free")+
#         theme(aspect.ratio = 1)
# 
# summarize_sims %>%
#         group_by(outcome) %>%
#         mutate(actual = case_when(outcome == 'bayesaverage' & is.na(actual) ~ 5.5,
#                                   TRUE ~ actual)) %>%
#         filter(!is.na(actual) & !is.nan(actual)) %>%
#         rmse(
#                 truth = actual,
#                 estimate = mid
#         )
# 
# mutate(name = abbreviate(name, 25)) %>%
#         ggplot(aes(ymin = lwr,
#                    ymax = upr,
#                    y=actual,
#                    x=reorder_within(name, mid, outcome)))+
#         geom_linerange(color = 'grey60')+
#         geom_point(color = 'blue')+
#         facet_wrap(outcome ~.,
#                    scales = "free")+
#         coord_flip()+
#         scale_x_reordered()+
#         xlab("")
# 
# 
# # 
# # left_join(.,
# #           games_processed %>%
# #                   select(game_id, bayesaverage) %>%
# #                   mutate(bayesaverage = replace_na(bayesaverage, 5.5))) %>%
# # mutate(name = abbreviate(name, minlength = 40)) %>%
# # # group_by(game_id, name) %>%
# # # summarize(
# # #         
# # # )
# # # filter(name %in% c('Sleeping Gods', 'Cascadia', 'Unfathomable')) %>%
# # # filter(game_id %in%
# # #                (sample(games_processed %>%
# # #                                filter(yearpublished == 2021) %>%
# # #                                pull(game_id), size = 25))) %>%
# # ggplot(aes(y=bayesaverage,
# #            ymin = lwr,
# #            ymax = upr,
# #            x=reorder(name, mid)))+
# # geom_pointrange()+
# # coord_flip(ylim = c(4.5, 9))+
# # xlab("")
# # 
# # 
# # 
# # 
# # 
# # 
# 
# # 