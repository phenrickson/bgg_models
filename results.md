# Model Results
Phil Henrickson

model/xgboost
2024-03-19

# Storage

    <tar_resources_gcp>
      bucket: bgg_data
      prefix: bgg_data/bgg_models/model/xgboost
      predefined_acl: private
      max_tries: NULL
      verbose: FALSE

# Artifacts

                     name type                time size  seconds
    1   averageweight_fit stem 2024-03-19 19:04:16 <NA>  115.944
    2      usersrated_fit stem 2024-03-19 23:39:54 <NA> 3354.536
    3         average_fit stem 2024-03-19 23:58:17 <NA> 1080.031
    4 averageweight_tuned stem 2024-03-19 19:02:12 <NA> 5967.283
    5    usersrated_tuned stem 2024-03-19 21:37:47 <NA> 9129.625
    6       average_tuned stem 2024-03-19 22:43:48 <NA> 3958.832
    7 averageweight_wflow stem 2024-03-19 17:22:32 <NA>    0.269
    8    usersrated_wflow stem 2024-03-19 19:04:53 <NA>    0.042
    9       average_wflow stem 2024-03-19 19:05:18 <NA>    0.032

# Metrics

## Outcomes

Games with at least 25 user ratings

             outcome yearpublished    rmse    mae   mape  rsq
    1        average          2020    0.65   0.47   7.15 0.34
    2        average          2021    0.69   0.51   7.64 0.29
    3        average          2022    0.74   0.55   8.06 0.25
    4  averageweight          2020    0.46   0.35  20.31 0.66
    5  averageweight          2021    0.47   0.36  20.79 0.65
    6  averageweight          2022    0.46   0.35  19.41 0.67
    7   bayesaverage          2020    0.28   0.16   2.67 0.52
    8   bayesaverage          2021    0.30   0.19   3.17 0.42
    9   bayesaverage          2022    0.33   0.20   3.28 0.35
    10    usersrated          2020 2086.90 529.63 192.60 0.30
    11    usersrated          2021 1662.58 523.15 225.57 0.31
    12    usersrated          2022 1213.99 473.02 287.42 0.19

All games

             outcome yearpublished    rmse    mae    mape  rsq
    1        average          2020    1.40   0.98   17.12 0.11
    2        average          2021    1.34   0.95   17.01 0.11
    3        average          2022    1.31   0.95   15.88 0.12
    4  averageweight          2020    0.53   0.41   24.52 0.56
    5  averageweight          2021    0.54   0.41   24.62 0.55
    6  averageweight          2022    0.53   0.41   24.58 0.57
    7   bayesaverage          2020    0.28   0.16    2.67 0.52
    8   bayesaverage          2021    0.30   0.19    3.17 0.42
    9   bayesaverage          2022    0.33   0.20    3.28 0.35
    10    usersrated          2020 1240.52 277.68 3973.76 0.32
    11    usersrated          2021  988.12 281.70 4213.65 0.32
    12    usersrated          2022  745.33 274.59 4254.96 0.22

## BGG Hits

Evaluating classification performance where *bgg_hit* is defined as any
game above 6.5 on the geek rating.

      yearpublished threshold bal_accuracy  kap  mcc f_meas precision recall
    1          2020       6.5         0.72 0.54 0.55   0.55      0.70   0.45
    2          2021       6.5         0.68 0.40 0.41   0.41      0.46   0.37
    3          2022       6.5         0.70 0.41 0.41   0.42      0.43   0.40
