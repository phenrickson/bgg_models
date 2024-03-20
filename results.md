# Model Results
Phil Henrickson

model/lm
2024-03-20

# Storage

    <tar_resources_gcp>
      bucket: bgg_data
      prefix: bgg_data/bgg_models/model/lm
      predefined_acl: private
      max_tries: NULL
      verbose: FALSE

# Artifacts

                     name type                time size seconds
    1   averageweight_fit stem 2024-03-20 10:47:11 <NA>  14.392
    2      usersrated_fit stem 2024-03-20 11:01:37 <NA>  26.181
    3         average_fit stem 2024-03-20 11:02:29 <NA>  25.930
    4 averageweight_tuned stem 2024-03-20 10:54:37 <NA>  12.823
    5    usersrated_tuned stem 2024-03-20 11:00:39 <NA>  19.989
    6       average_tuned stem 2024-03-20 11:01:01 <NA>  20.236
    7 averageweight_wflow stem 2024-03-20 10:46:20 <NA>   0.285
    8    usersrated_wflow stem 2024-03-20 10:59:42 <NA>   0.084
    9       average_wflow stem 2024-03-20 11:00:04 <NA>   0.034

# Metrics

## Outcomes

Games with at least 25 user ratings

             outcome yearpublished    rmse    mae   mape  rsq
    1        average          2020    0.65   0.48   7.27 0.32
    2        average          2021    0.70   0.51   7.79 0.27
    3        average          2022    0.74   0.55   8.19 0.21
    4  averageweight          2020    0.50   0.38  21.97 0.60
    5  averageweight          2021    0.51   0.39  22.65 0.59
    6  averageweight          2022    0.49   0.38  20.22 0.62
    7   bayesaverage          2020    0.30   0.17   2.87 0.48
    8   bayesaverage          2021    0.34   0.21   3.50 0.38
    9   bayesaverage          2022    0.37   0.21   3.45 0.35
    10    usersrated          2020 2639.72 613.26 219.15 0.24
    11    usersrated          2021 2085.32 628.85 221.96 0.19
    12    usersrated          2022 2082.52 555.14 237.00 0.19

All games

             outcome yearpublished    rmse    mae    mape  rsq
    1        average          2020    1.40   0.99   17.35 0.10
    2        average          2021    1.34   0.95   17.25 0.10
    3        average          2022    1.29   0.93   15.95 0.10
    4  averageweight          2020    0.55   0.42   25.44 0.53
    5  averageweight          2021    0.57   0.44   26.12 0.51
    6  averageweight          2022    0.56   0.43   25.64 0.52
    7   bayesaverage          2020    0.30   0.17    2.87 0.48
    8   bayesaverage          2021    0.34   0.21    3.50 0.38
    9   bayesaverage          2022    0.37   0.21    3.45 0.35
    10    usersrated          2020 1678.62 311.73 3878.16 0.22
    11    usersrated          2021 1242.25 311.26 3844.42 0.21
    12    usersrated          2022 1253.85 286.94 3482.31 0.20

## BGG Hits

Evaluating classification performance where *bgg_hit* is defined as any
game above 6.5 on the geek rating.

      yearpublished threshold bal_accuracy  kap  mcc f_meas precision recall
    1          2020       6.5         0.74 0.52 0.52   0.52      0.56   0.49
    2          2021       6.5         0.73 0.41 0.42   0.42      0.39   0.47
    3          2022       6.5         0.69 0.38 0.38   0.39      0.38   0.40
