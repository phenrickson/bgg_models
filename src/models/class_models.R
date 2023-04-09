# model specifictions and tuning -----------------------------------------------------------

# base logit
glm_class_spec = 
        logistic_reg()

# penalized logistic regression via glmnet
glmnet_class_spec = 
        logistic_reg(penalty = tune::tune(),
                     mixture = 0.5) %>%
        set_engine("glmnet")

# regularization
glmnet_grid = 
        grid_regular(
                penalty(range = c(-4, -0.5)),
                levels = 10
        )

# knn
library(kknn)
knn_class_spec = 
        nearest_neighbor(neighbors = tune::tune()) %>%
        set_mode("classification")

knn_grid = 
        grid_regular(
                neighbors(range = c(5, 200)),
                levels = 10
        )

# mars
mars_class_spec = 
        mars() %>%
        set_mode("classification")

# cart for classification
cart_class_spec <-
        decision_tree(
                cost_complexity = tune(),
                tree_depth = tune(),
                min_n = tune()
        ) %>%
        set_mode("classification") %>%
        set_engine("rpart")

# xgb for class
xgb_class_spec <-
        boost_tree(
                trees = 500,
                min_n = tune(),
                sample_size = tune(),
                learn_rate = tune(),
                tree_depth = tune(),
                stop_iter = 50
        ) %>%
        set_mode("classification") %>%
        set_engine("xgboost",
                   eval_metric = 'logloss')

# # random forest
# rf_class_spec = 
#         rand_forest(trees = 500,
#                     mtry = tune()) %>%
#         set_mode("classification") %>%
#         set_engine("ranger")

# lightgbm
library(bonsai)
lightgbm_class_spec <-
        parsnip::boost_tree(
                mode = "classification",
                trees = 500,
                min_n = tune(),
                tree_depth = tune(),
        ) %>%
        set_engine("lightgbm", objective = "binary")

# create tuning grid
# tune_grid =
#         grid_regular(
#                 penalty(range = c(-4, -.25)),
#                 freq_cut(range = c(100, 1000)),
#                 unique_cut(range = c(10, 10)),
#                 levels = c(penalty = 10,
#                            freq_cut = 4,
#                            unique_cut = 1)
#         )

