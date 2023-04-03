# save model results; deploy best model to gcs

# connects to gcs with default bucket
source(here::here("src", "data", "connect_to_gcs.R"))


# make local models board
local_models_board = pins::board_folder(here::here("models", "models", "hurdle"))

# xgboost -----------------------------------------------------------------

# get xgb tune
xgb_tune = race_res %>%
        filter(wflow_id == 'all_xgb') %>%
        mutate(best_tune = map(result,
                               select_best, metric = 'roc_auc')) %>%
        select(wflow_id, best_tune) %>%
        pluck("best_tune", 1)

# fit
hurdle_xgb = 
        race_res %>%
        extract_workflow('all_xgb') %>%
        finalize_workflow(parameters = xgb_tune) %>%
        fit(bind_rows(train, 
                      valid %>% filter(yearpublished < end_train_year + 2)))

# pin workflow
hurdle_xgb  %>%
        pin_write(board = models_board,
                  name = 'hurdle_xgb',
                  description = 'xgboost workflow for hurdle')

# vetiver version (for active use)
hurdle_vetiver =
        vetiver::vetiver_model(model = hurdle_xgb,
                               model_name = "hurdle_vetiver",
                               description = paste("xgboost classifiation model trained through",
                                                   end_train_year+ 1 , 
                                                   "to predict whether games will hit user ratings threshold"))

# test that it works
testthat::test_that("vetiver model does not error due to package",
                    testthat::expect_no_error(
                            hurdle_vetiver %>%
                                    predict(train %>%
                                                    sample_n(10)))
)

# pin to gcs
vetiver::vetiver_pin_write(board = deployed_board,
                           hurdle_vetiver)
                  


# lightgbm ----------------------------------------------------------------


### save locally
# save workflow
saveRDS(hurdle_fit, file = here::here("models", "models", "hurdle", "lightgbm_wflow.rds"))

# save booster
saveRDS.lgb.Booster(extract_fit_engine(hurdle_fit), file = here::here("models", "models", "hurdle", "lightgbm_booster.rds"))


### read locally
# read in wflow
hurdle_wflow = readRDS(here::here("models", "models", "hurdle", "lightgbm_wflow.rds"))
# add booster to wflow
hurdle_wflow$fit$fit$fit = readRDS.lgb.Booster(here::here("models", "models", "hurdle", "lightgbm_booster.rds"))

hurdle_wflow %>%
        predict(train %>%
                        sample_n(10),
                type = 'prob')

# pin lightgbm to models board
# # # save workflow 
# hurdle_fit %>%
#         pin_write(board = models_board,
#                   name = 'hurdle_wflow',
#                   description = paste('workflow for light_gbm hurdle model trained through', end_train_year + 2),
#                   tags = c('hurdle', 'model', 'classification', 'light_gbm', 'workflow')
#         )

# # save light_gbm booster
# save(foo %>%
#         extract_fit_engine(),
#      file =
#         pin_write(board = models_board,
#                   name = 'hurdle_booster',
#                   description = paste('booster for light_gbm hurdle model trained through', end_train_year + 2),
#                   tags = c('hurdle', 'model', 'classification', 'light_gbm', 'booster')
#         )
# 
# 
# 
# foo = hurdle_fit
# 
# # load wflow
# hurdle_fit = 
#         pin_read(board = models_board,
#                  name = 'hurdle_wflow'
#         )
# 
# # load booster
# hurdle_fit$fit$fit$fit  = 
#         pin_read(board = models_board,
#                  name = 'hurdle_booster'
#         )
#         
# 
# hurdle_fit %>%
#         predict(train %>%
#                 sample_n(10))
# 
# 
# # read in light_gbm wflow
# 
# 
# 
# # # save booster
# # saveRDS.lgb.Booster(extract_fit_engine(wflow), 
# #                     file = models_board,
# #                     "hurdle_lightgbm_booster.rds")
# #                   
# #                   
# #           
# # 
# # pin_workflow = function(wflow,
# #                         model_name) {
# #         
# #         # if wflow is lightgbm
# #         
# #         # workflow with recipe, etc
# #         saveRDS(wflow, file = "lightgbm_wflow.rds")
# #         # lightgbm booster
# #         saveRDS.lgb.Booster(extract_fit_engine(wflow), "lightgbm_booster.rds")
# #         
# #         # load trained workflow and merge it with lgb.booster
# #         new_lightgbm_wflow <- readRDS("lightgbm_wflw.rds")
# #         new_lightwflow$fit$fit$fit <- readRDS.lgb.Booster("lightgbm_booster.rds")
# #         
# #         
# # }
# #         
# #         
# # 
# # # custom function for saving lightgbm model
# # saveRDS()