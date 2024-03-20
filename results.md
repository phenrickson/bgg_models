# Model Results
Phil Henrickson

model/lightgbm
2024-03-20

# Storage

    <tar_resources_gcp>
      bucket: bgg_data
      prefix: bgg_data/bgg_models/model/lightgbm
      predefined_acl: private
      max_tries: NULL
      verbose: FALSE

# Artifacts

                     name type                time size seconds
    1   averageweight_fit stem 2024-03-20 13:30:50 <NA>   2.913
    2      usersrated_fit stem 2024-03-20 13:35:38 <NA>   4.210
    3         average_fit stem 2024-03-20 13:36:25 <NA>   4.270
    4 averageweight_tuned stem 2024-03-20 13:30:38 <NA>  58.575
    5    usersrated_tuned stem 2024-03-20 13:34:03 <NA>  77.437
    6       average_tuned stem 2024-03-20 13:35:23 <NA>  78.193
    7 averageweight_wflow stem 2024-03-20 13:29:20 <NA>   0.308
    8    usersrated_wflow stem 2024-03-20 13:31:38 <NA>   0.054
    9       average_wflow stem 2024-03-20 13:32:15 <NA>   0.039

# Metrics

## Outcomes

Games with at least 25 user ratings

             outcome yearpublished    rmse    mae   mape  rsq
    1        average          2020    0.64   0.47   7.15 0.35
    2        average          2021    0.68   0.50   7.51 0.31
    3        average          2022    0.74   0.56   8.12 0.25
    4  averageweight          2020    0.46   0.35  19.93 0.66
    5  averageweight          2021    0.47   0.36  20.36 0.65
    6  averageweight          2022    0.45   0.35  19.23 0.67
    7   bayesaverage          2020    0.27   0.16   2.63 0.54
    8   bayesaverage          2021    0.31   0.19   3.24 0.41
    9   bayesaverage          2022    0.33   0.19   3.28 0.38
    10    usersrated          2020 2067.03 534.34 186.96 0.28
    11    usersrated          2021 2038.07 594.08 229.95 0.25
    12    usersrated          2022 1587.27 525.89 279.88 0.22

All games

             outcome yearpublished    rmse    mae    mape  rsq
    1        average          2020    1.40   0.99   17.13 0.11
    2        average          2021    1.34   0.95   16.91 0.11
    3        average          2022    1.32   0.95   15.91 0.11
    4  averageweight          2020    0.53   0.40   24.06 0.56
    5  averageweight          2021    0.54   0.41   23.94 0.55
    6  averageweight          2022    0.53   0.41   24.26 0.57
    7   bayesaverage          2020    0.27   0.16    2.63 0.54
    8   bayesaverage          2021    0.31   0.19    3.24 0.41
    9   bayesaverage          2022    0.33   0.19    3.28 0.38
    10    usersrated          2020 1228.74 272.13 3615.10 0.30
    11    usersrated          2021 1204.98 302.72 4052.19 0.27
    12    usersrated          2022  959.90 287.02 3938.79 0.25

## BGG Hits

Evaluating classification performance where *bgg_hit* is defined as any
game above 6.5 on the geek rating.

      yearpublished threshold bal_accuracy  kap  mcc f_meas precision recall
    1          2020       6.5         0.74 0.55 0.56   0.56      0.65   0.49
    2          2021       6.5         0.69 0.40 0.40   0.41      0.44   0.39
    3          2022       6.5         0.72 0.44 0.44   0.45      0.46   0.44
