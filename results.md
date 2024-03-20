# Model Results
Phil Henrickson

model/glmnet
2024-03-19

# Storage

    <tar_resources_gcp>
      bucket: bgg_data
      prefix: bgg_data/bgg_models/model/glmnet
      predefined_acl: private
      max_tries: NULL
      verbose: FALSE

# Artifacts

                     name type                time size seconds
    1         average_fit stem 2024-03-19 11:58:42 <NA>  15.789
    2   averageweight_fit stem 2024-03-19 11:56:10 <NA>   8.904
    3      usersrated_fit stem 2024-03-19 13:43:43 <NA>  16.586
    4       average_tuned stem 2024-03-19 11:58:16 <NA>  62.295
    5 averageweight_tuned stem 2024-03-19 11:55:53 <NA>  39.167
    6    usersrated_tuned stem 2024-03-19 13:43:16 <NA>  68.123
    7       average_wflow stem 2024-03-19 11:56:48 <NA>   0.046
    8 averageweight_wflow stem 2024-03-19 11:54:54 <NA>   0.061
    9    usersrated_wflow stem 2024-03-19 13:41:41 <NA>   0.304

# Metrics

## Outcomes

Games with at least 25 user ratings

             outcome yearpublished    rmse    mae   mape  rsq
    1        average          2020    0.64   0.47   7.09 0.34
    2        average          2021    0.68   0.50   7.63 0.28
    3        average          2022    0.72   0.53   7.96 0.22
    4  averageweight          2020    0.49   0.37  21.67 0.61
    5  averageweight          2021    0.50   0.38  21.98 0.60
    6  averageweight          2022    0.48   0.37  19.70 0.63
    7   bayesaverage          2020    0.28   0.17   2.74 0.50
    8   bayesaverage          2021    0.31   0.19   3.22 0.40
    9   bayesaverage          2022    0.34   0.19   3.24 0.35
    10    usersrated          2020 2149.54 530.95 189.62 0.26
    11    usersrated          2021 1734.15 535.62 197.97 0.25
    12    usersrated          2022 1822.61 493.03 221.56 0.15

All games

             outcome yearpublished    rmse    mae    mape  rsq
    1        average          2020    1.40   0.98   17.30 0.10
    2        average          2021    1.33   0.94   17.20 0.11
    3        average          2022    1.29   0.92   15.91 0.10
    4  averageweight          2020    0.54   0.42   25.23 0.53
    5  averageweight          2021    0.56   0.43   25.62 0.51
    6  averageweight          2022    0.55   0.42   25.10 0.53
    7   bayesaverage          2020    0.28   0.17    2.74 0.50
    8   bayesaverage          2021    0.31   0.19    3.22 0.40
    9   bayesaverage          2022    0.34   0.19    3.24 0.35
    10    usersrated          2020 1296.17 276.89 3878.69 0.26
    11    usersrated          2021 1027.25 274.86 3735.04 0.27
    12    usersrated          2022 1093.45 260.43 3393.61 0.17

## BGG Hits

Evaluating classification performance where *bgg_hit* is defined as any
game above 6.5 on the geek rating.

      yearpublished threshold bal_accuracy  kap  mcc f_meas precision recall
    1          2020       6.5         0.71 0.53 0.54   0.53      0.70   0.43
    2          2021       6.5         0.69 0.42 0.42   0.42      0.47   0.39
    3          2022       6.5         0.70 0.42 0.42   0.43      0.45   0.40
