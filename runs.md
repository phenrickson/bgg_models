# Model Runs
Phil Henrickson

analysis
2024-03-21

Show metrics for model runs, evaluating bgg outcomes

               branch       outcome yearpublished     rmse     mae    mape   rsq
    1  model/lightgbm  bayesaverage          2020    0.272   0.159   2.634 0.539
    2   model/xgboost  bayesaverage          2020    0.277   0.162   2.668 0.524
    3    model/glmnet  bayesaverage          2020    0.284   0.166   2.738 0.503
    4        model/lm  bayesaverage          2020    0.303   0.172   2.870 0.482
    5  model/lightgbm averageweight          2020    0.456   0.347  19.930 0.665
    6   model/xgboost averageweight          2020    0.458   0.352  20.311 0.660
    7    model/glmnet averageweight          2020    0.487   0.371  21.665 0.612
    8        model/lm averageweight          2020    0.497   0.378  21.965 0.602
    9    model/glmnet       average          2020    0.639   0.466   7.090 0.337
    10 model/lightgbm       average          2020    0.645   0.472   7.149 0.346
    11  model/xgboost       average          2020    0.647   0.473   7.147 0.343
    12       model/lm       average          2020    0.651   0.478   7.272 0.323
    13 model/lightgbm    usersrated          2020 2067.031 534.337 186.965 0.279
    14  model/xgboost    usersrated          2020 2086.898 529.634 192.601 0.304
    15   model/glmnet    usersrated          2020 2149.537 530.949 189.623 0.260
    16       model/lm    usersrated          2020 2639.724 613.264 219.146 0.237

Show metrics for model runs, evaluating ability to classify ‘hits’ -
games that crack into the top 1000.

               branch yearpublished   kap   mcc f_meas precision recall
    1  model/lightgbm          2020 0.553 0.559  0.558     0.655  0.486
    2   model/xgboost          2020 0.540 0.555  0.545     0.702  0.446
    3    model/glmnet          2020 0.528 0.543  0.533     0.696  0.432
    4        model/lm          2020 0.515 0.517  0.522     0.562  0.486
    5    model/glmnet          2021 0.415 0.417  0.423     0.468  0.387
    6        model/lm          2021 0.414 0.416  0.424     0.389  0.467
    7   model/xgboost          2021 0.404 0.406  0.412     0.459  0.373
    8  model/lightgbm          2021 0.403 0.404  0.411     0.439  0.387
    9  model/lightgbm          2022 0.444 0.444  0.452     0.465  0.440
    10   model/glmnet          2022 0.417 0.418  0.426     0.455  0.400
    11  model/xgboost          2022 0.408 0.408  0.417     0.435  0.400
    12       model/lm          2022 0.377 0.377  0.387     0.375  0.400
