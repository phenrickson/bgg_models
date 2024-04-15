# Model Results
Phil Henrickson

stable/model
2024-04-15

# pipeline

- The project is out-of-sync – use `renv::status()` for details. ✔
  Setting scopes to
  https://www.googleapis.com/auth/devstorage.full_control and
  https://www.googleapis.com/auth/cloud-platform ✔ Successfully
  auto-authenticated via
  /Users/phenrickson/.secrets/gcp-demos-411520-ff4c11ac8b1f.json Set
  default bucket name to ‘bgg_data’

``` mermaid
graph LR
  style Legend fill:#FFFFFF00,stroke:#000000;
  style Graph fill:#FFFFFF00,stroke:#000000;
  subgraph Legend
    direction LR
    x7420bd9270f8d27d([""Up to date""]):::uptodate --- x0a52b03877696646([""Outdated""]):::outdated
    x0a52b03877696646([""Outdated""]):::outdated --- xbf4603d6c2c2ad6b([""Stem""]):::none
  end
  subgraph Graph
    direction LR
    xcf910d7efaac5be7(["predictions"]):::uptodate --> x5b0d23a42f47c814(["bgg_hit_metrics"]):::uptodate
    xe73a0f95bb7c669b(["games_prepared"]):::uptodate --> x6a9d509448f9bd3e(["split"]):::uptodate
    x170105733f313efa(["training_imputed"]):::uptodate --> x634aca9b773632c4(["usersrated_split"]):::uptodate
    x47290a9e3258c6f8(["averageweight_split"]):::uptodate --> x0f34a02bd6ed632f(["averageweight_fit"]):::uptodate
    x04d83687468f3fc4(["averageweight_tuned"]):::uptodate --> x0f34a02bd6ed632f(["averageweight_fit"]):::uptodate
    xa620e5e9c738b77b(["averageweight_wflow"]):::uptodate --> x0f34a02bd6ed632f(["averageweight_fit"]):::uptodate
    x5225ac863fffff5b(["model_spec"]):::uptodate --> x2c80a51924a741af(["usersrated_wflow"]):::uptodate
    x634aca9b773632c4(["usersrated_split"]):::uptodate --> x2c80a51924a741af(["usersrated_wflow"]):::uptodate
    xcf910d7efaac5be7(["predictions"]):::uptodate --> x549b9472ad95b0e6(["outcome_metrics_all"]):::uptodate
    xc0de7cb1ceaaae9a(["average_fit"]):::uptodate --> xcf910d7efaac5be7(["predictions"]):::uptodate
    xd6936e852f1fbe47(["usersrated_fit"]):::uptodate --> xcf910d7efaac5be7(["predictions"]):::uptodate
    x60bd72cb3684ce1d(["validation_imputed"]):::uptodate --> xcf910d7efaac5be7(["predictions"]):::uptodate
    x634aca9b773632c4(["usersrated_split"]):::uptodate --> xd6936e852f1fbe47(["usersrated_fit"]):::uptodate
    x8b18f98f2746942f(["usersrated_tuned"]):::uptodate --> xd6936e852f1fbe47(["usersrated_fit"]):::uptodate
    x2c80a51924a741af(["usersrated_wflow"]):::uptodate --> xd6936e852f1fbe47(["usersrated_fit"]):::uptodate
    x6a9d509448f9bd3e(["split"]):::uptodate --> x47290a9e3258c6f8(["averageweight_split"]):::uptodate
    x80752a9437f8f285(["average_split"]):::uptodate --> xc0de7cb1ceaaae9a(["average_fit"]):::uptodate
    xd0ff2216903615b0(["average_tuned"]):::uptodate --> xc0de7cb1ceaaae9a(["average_fit"]):::uptodate
    x8d1623556cc14734(["average_wflow"]):::uptodate --> xc0de7cb1ceaaae9a(["average_fit"]):::uptodate
    x2b7f0716b8751c70(["games_raw"]):::uptodate --> xe73a0f95bb7c669b(["games_prepared"]):::uptodate
    x170105733f313efa(["training_imputed"]):::uptodate --> x80752a9437f8f285(["average_split"]):::uptodate
    x47290a9e3258c6f8(["averageweight_split"]):::uptodate --> xa620e5e9c738b77b(["averageweight_wflow"]):::uptodate
    x5225ac863fffff5b(["model_spec"]):::uptodate --> xa620e5e9c738b77b(["averageweight_wflow"]):::uptodate
    x5b0d23a42f47c814(["bgg_hit_metrics"]):::uptodate --> xe0fba61fbc506510(["report"]):::outdated
    x549b9472ad95b0e6(["outcome_metrics_all"]):::uptodate --> xe0fba61fbc506510(["report"]):::outdated
    xe114bb7356a422fc(["outcome_metrics_filtered"]):::uptodate --> xe0fba61fbc506510(["report"]):::outdated
    xcf910d7efaac5be7(["predictions"]):::uptodate --> xe0fba61fbc506510(["report"]):::outdated
    xcf910d7efaac5be7(["predictions"]):::uptodate --> xe114bb7356a422fc(["outcome_metrics_filtered"]):::uptodate
    x80752a9437f8f285(["average_split"]):::uptodate --> xd0ff2216903615b0(["average_tuned"]):::uptodate
    x8d1623556cc14734(["average_wflow"]):::uptodate --> xd0ff2216903615b0(["average_tuned"]):::uptodate
    x65f5ea8ba96b7060(["reg_metrics"]):::uptodate --> xd0ff2216903615b0(["average_tuned"]):::uptodate
    xa35bb8a740e7efd3(["tuning_grid"]):::uptodate --> xd0ff2216903615b0(["average_tuned"]):::uptodate
    x0f34a02bd6ed632f(["averageweight_fit"]):::uptodate --> x170105733f313efa(["training_imputed"]):::uptodate
    x6a9d509448f9bd3e(["split"]):::uptodate --> x170105733f313efa(["training_imputed"]):::uptodate
    x0f34a02bd6ed632f(["averageweight_fit"]):::uptodate --> x60bd72cb3684ce1d(["validation_imputed"]):::uptodate
    x6a9d509448f9bd3e(["split"]):::uptodate --> x60bd72cb3684ce1d(["validation_imputed"]):::uptodate
    x47290a9e3258c6f8(["averageweight_split"]):::uptodate --> x04d83687468f3fc4(["averageweight_tuned"]):::uptodate
    xa620e5e9c738b77b(["averageweight_wflow"]):::uptodate --> x04d83687468f3fc4(["averageweight_tuned"]):::uptodate
    x65f5ea8ba96b7060(["reg_metrics"]):::uptodate --> x04d83687468f3fc4(["averageweight_tuned"]):::uptodate
    xa35bb8a740e7efd3(["tuning_grid"]):::uptodate --> x04d83687468f3fc4(["averageweight_tuned"]):::uptodate
    x80752a9437f8f285(["average_split"]):::uptodate --> x8d1623556cc14734(["average_wflow"]):::uptodate
    x5225ac863fffff5b(["model_spec"]):::uptodate --> x8d1623556cc14734(["average_wflow"]):::uptodate
    x65f5ea8ba96b7060(["reg_metrics"]):::uptodate --> x8b18f98f2746942f(["usersrated_tuned"]):::uptodate
    xa35bb8a740e7efd3(["tuning_grid"]):::uptodate --> x8b18f98f2746942f(["usersrated_tuned"]):::uptodate
    x634aca9b773632c4(["usersrated_split"]):::uptodate --> x8b18f98f2746942f(["usersrated_tuned"]):::uptodate
    x2c80a51924a741af(["usersrated_wflow"]):::uptodate --> x8b18f98f2746942f(["usersrated_tuned"]):::uptodate
  end
  classDef uptodate stroke:#000000,color:#ffffff,fill:#354823;
  classDef outdated stroke:#000000,color:#000000,fill:#78B7C5;
  classDef none stroke:#000000,color:#000000,fill:#94a4ac;
  linkStyle 0 stroke-width:0px;
  linkStyle 1 stroke-width:0px;
```

# storage

    <tar_resources_gcp>
      bucket: bgg_data
      prefix: bgg_data/bgg_models/stable/model
      predefined_acl: private
      max_tries: NULL
      verbose: FALSE

# artifacts

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

# metrics

## outcomes

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

all games

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

## bgg hits

Evaluating classification performance where *bgg_hit* is defined as any
game above 6.5 on the geek rating.

      yearpublished threshold bal_accuracy  kap  mcc f_meas precision recall
    1          2020       6.5         0.71 0.53 0.54   0.53      0.70   0.43
    2          2021       6.5         0.69 0.42 0.42   0.42      0.47   0.39
    3          2022       6.5         0.70 0.42 0.42   0.43      0.45   0.40
