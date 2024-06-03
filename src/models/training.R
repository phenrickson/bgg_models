gcs_model_board = function(bucket = googleCloudStorageR::gcs_get_global_bucket(),
                           prefix = "model/",
                           versioned = T,
                           ...) {
    
    pins::board_gcs(bucket = bucket,
                    prefix = prefix,
                    versioned = versioned,
                    ...)
}

finalize_model = function(wflow,
                          data,
                          ratings = 25,
                          weights = 5) {
    
    
    outcome = 
        wflow |>
        extract_workflow_outcome()
        
    processed = 
        data |>
        preprocess_outcome(outcome = outcome,
                           ratings = ratings,
                           weights = weights)
    
    wflow |>
        fit(processed) |>
        bundle::bundle()
}

pin_outcome_model = function(wflow,
                             board,
                             tuning,
                             metrics,
                             data) {
    
    model_outcome = wflow |> extract_workflow_outcome()
    model_name = paste0("bgg_", model_outcome, "_")
    model_metrics = metrics |> filter(outcome == model_outcome)
    model_data = data |> preprocess_outcome(outcome = model_outcome)
    model_tuning = tuning |> pluck("result", 1) |> select(-.predictions)
    
    model_metadata = 
        list("metrics" = model_metrics,
             "data" = model_data,
             "tuning" = model_tuning)
    
    wflow |>
        pin_model(
            board = board,
            model_name = model_name,
            metadata = model_metadata
        )
    
}

pin_model = function(model,
                     model_name,
                     board,
                     metadata,
                     ...) {
    
    
    model |>
        vetiver::vetiver_model(
            model_name = model_name,
            metadata = metadata
        ) |>
        vetiver::vetiver_pin_write(
            board = board
        )

}

# function to fid models by outcome, recipe, and model type
train_outcome_wflow = function(data,
                               outcome,
                               weights = 5,
                               ratings = 25,
                               valid_years,
                               test_years =0,
                               recipe,
                               model_spec,
                               grid,
                               metrics = my_reg_metrics(),
                               ids = id_vars(),
                               predictors = predictor_vars(),
                               ...) {
    
    data |>
        prepare_outcome_split(outcome = outcome,
                              weights = weights,
                              ratings = ratings,
                              valid_years = valid_years,  
                              test_years = test_years) |>
        prepare_outcome_recipe(outcome = outcome,
                               recipe = recipe,
                               ids = ids,
                               predictors = predictors,
                               ...) |>
        prepare_outcome_wflow(model_spec = model_spec) |>
        tune_outcome_wflow(grid = grid,
                           metrics = metrics)
}


convert_to_workflow_set = function(tuned) {
    
    wflow = 
        tuned |>
        pluck("wflow", 1)
    
    wflow_id = 
        tuned |>
        pull(wflow_id)
    
    result = 
        tuned |>
        pluck("result", 1)
    
    wflow_list = list(wflow)
    names(wflow_list) = wflow_id
    
    wflow_set = 
        as_workflow_set(!!!wflow_list)
    
    wflow_set |>
        mutate(result = list(result))
    
}

bundle_wflow = function(tuned) {
    
    tuned |>
        pluck("wflow", 1) |>
        bundle::bundle()
}

prepare_outcome_split = function(data,
                                 outcome,
                                 weights,
                                 ratings,
                                 valid_years,
                                 test_years) {
    
    split = 
        data |>
        outcome_tuning_split(
            outcome = outcome,
            weights = weights,
            valid_years = valid_years,
            test_years = test_years,
            ratings = ratings
        )
    
    settings = tibble(weights = weights,
                      ratings = ratings)
    
    tibble(outcome = outcome,
           settings = list(settings),
           split = list(split))
}

prepare_outcome_recipe = function(split,
                                  outcome,
                                  recipe,
                                  ...) {
    
    training =
        split |>
        pluck("split", 1) |>
        training()
    
    rec =
        training |>
        recipe(outcome = {{outcome}},
               ...)
    
    split |>
        add_column(recipe = list(rec))
}

prepare_outcome_wflow = function(prepared,
                                 model_spec,
                                 wflow_id = NULL,
                                 ...) {
    
    
    rec = 
        prepared |> pluck("recipe", 1)
    
    wflow =
        workflow() |>
        add_recipe(rec) |>
        add_model(model_spec)
    
    if (is.null(wflow_id)) {
        wflow_id = model_spec$engine
    }
    
    prepared |>
        select(-recipe) |>
        add_column(wflow = list(wflow),
                   wflow_id = wflow_id) |>
        select(outcome, wflow_id, everything())
}

tune_outcome_wflow = function(prepared,
                              grid,
                              metrics = my_reg_metrics()) {
    
    valid = 
        prepared |> 
        pluck("split", 1) |> 
        validation_set()
    
    wflow = 
        prepared |>
        pluck("wflow", 1)
    
    result = 
        wflow |>
        tune_workflow(
            resamples = valid,
            metrics = metrics,
            grid = grid
        )
    
    prepared |>
        add_column(result = list(result))
}

finalize_outcome_wflow = function(tuned,
                                  metric = 'rmse') {
    
    result = 
        tuned |>
        pluck("result", 1)
    
    best_params = 
        result |>
        select_best(metric = metric)
    
    wflow = 
        tuned |>
        pluck("wflow", 1) |>
        finalize_workflow(parameters = best_params)
    
    tuned |>
        select(-wflow) |>
        add_column(params = list(best_params),
                   wflow = list(wflow))
    
}

fit_outcome_wflow = function(tuned,
                             data = 'all') {
    
    wflow = 
        tuned |>
        pluck("wflow", 1)
    
    split = 
        tuned |>
        pluck("split", 1)
    
    if (data == "all") {
        dat = 
            tuned |> 
            pluck("split", 1) |>
            pluck("data")
    }
    else if (data == "train") {
        dat = 
            split |>
            training()
    }
    else if (data == "valid") {
        dat = bind_rows(split |> training(),
                        split |> validation())
    } else {
        
        fit = 
            wflow |>
            fit(data)
    }
    
    fit = 
        wflow |>
        fit(dat)
    
    tuned |>
        mutate(wflow = list(fit))
}

# recipes for splits
# create train/valid/test sets based on year
create_year_split = function(data, end_train_year, valid_years) {
    
    tmp = 
        data |>
        mutate(.row_number = row_number())
    
    train_id = 
        tmp |>
        dplyr::filter(yearpublished <= end_train_year) |>
        pull(.row_number)
    
    val_id = 
        tmp |>
        dplyr::filter(yearpublished > end_train_year & yearpublished <= end_train_year + valid_years) |>
        pull(.row_number)
    
    test_id = 
        tmp |>
        filter(yearpublished > end_train_year + valid_years) |>
        pull(.row_number)
    
    res = 
        list(
            data = data,
            train_id = train_id,
            val_id = val_id,
            test_id = test_id
        )
    
    class(res) <- c("initial_validation_split", "three_way_split")
    
    res
}

outcome_tuning_split = function(data, outcome, ratings = 25, weights = 0, valid_years = 1, test_years = 1) {
    
    data |>
        preprocess_outcome(
            outcome = outcome,
            ratings = ratings,
            weights = weights
        ) |>
        # create a train-valid-test split given the input
        # set the end of the training set to 
        create_year_split(
            end_train_year = max(data$yearpublished, na.rm = T)- (valid_years + test_years),
            valid_years = valid_years
        )
}

outcome_assess_split = function(split) {
    
    train = 
        split |>
        training() |>
        bind_rows(
            split |> 
                validation()
        )
    
    test = 
        split |>
        testing()
    
    make_splits(
        list(
            analysis = seq(nrow(train)),
            assessment = nrow(train) + seq(nrow(test))),
        bind_rows(train,
                  test)
    )
}

# recipes for bgg outcomes
predictor_vars= function(vars = 
                             c("minplayers",
                               "maxplayers",
                               "playingtime",
                               "minplaytime",
                               "maxplaytime",
                               "image",
                               "thumbnail",
                               "minage",
                               "categories",
                               "mechanics",
                               "publishers",
                               "designers",
                               "artists",
                               "families",
                               "mechanisms",
                               "components",
                               "themes"
                             )
) {vars}

id_vars = function(vars = 
                       c("game_id",
                         "name",
                         "numweights",
                         "yearpublished",
                         "averageweight",
                         "average",
                         "bayesaverage",
                         "usersrated",
                         "image",
                         "thumbnail",
                         "description")) {vars}

spline_vars = function(vars = c("number_mechanics",
                                "number_categories")) {vars}

# function to predict and calculate bayesaverage given average and usersrated
predict_bayesaverage = function(data,
                                average_model,
                                usersrated_model,
                                ratings = 2000) {
    
    # get predictions
    data |>
        predict_average(
            model = average_model
        ) |>
        predict_usersrated(
            model = usersrated_model
        ) |>
        calculate_bayesaverage(
            ratings = ratings
        ) |>
        mutate(.pred_averageweight = est_averageweight) |>
        select(yearpublished,
               game_id,
               name,
               starts_with(".pred"),
               everything()
        )
    
}

# function to tune model given workflow and resamples
tune_workflow = function(workflow,
                         resamples,
                         metrics,
                         grid,
                         save_pred = T,
                         ...) {
    
    workflow |>
        tune_grid(
            resamples = resamples,
            grid = grid,
            metrics = metrics,
            control = tune::control_grid(save_pred = save_pred,
                                         verbose = T),
            ...
        )
}

# function to build workflow for an outcome given a model specification and a recipe
build_outcome_workflow = 
    function(model,
             recipe) {
        
        workflows::workflow() |>
            workflows::add_model(
                model
            ) |>
            workflows::add_recipe(
                recipe
            )
    }

# recipe for linear model
recipe_linear = function(data,
                         outcome,
                         ids,
                         predictors,
                         splines) {
    
    data |>
        build_recipe(
            outcome = {{outcome}},
            ids = ids,
            predictors = predictors
        ) |>
        # standard preprocessing
        add_preprocessing() |>
        # simple imputation for numeric
        add_imputation() |>
        # create dummies
        add_bgg_dummies() |>
        # add splines for nonlinearities
        # splines with a fourth degree polynomial for year
        add_splines(vars = "year", degree = 4) |>
        # splines with fifth degree polynomials for mechanics/categories
        add_splines(vars = splines) |>
        # remove zero variance
        step_zv(all_numeric_predictors()) |>
        # remove highly correlated
        step_corr(all_numeric_predictors(), threshold = 0.95) |>
        # normalize
        step_normalize(all_numeric_predictors())
}

# recipe for tree based models
recipe_trees = function(data,
                        outcome,
                        ids = id_vars(),
                        predictors = predictor_vars()) {
    
    data |>
        build_recipe(
            outcome = {{outcome}},
            ids = ids,
            predictors = predictors
        ) |>
        # standard preprocessing
        add_preprocessing() |>
        # simple imputation for numeric
        add_imputation() |>
        # create dummies
        add_bgg_dummies() |>
        # remove zero variance
        step_zv(all_numeric_predictors()) |>
        # remove highly correlated
        step_corr(all_numeric_predictors(), threshold = 0.95)
}

recipe_hurdle = function(data,
                        outcome,
                        ids = id_vars(),
                        predictors = predictor_vars(),
                        threshold = 0.001,
                        ...) {
    
    data |>
        build_recipe(
            outcome = {{outcome}},
            ids = ids,
            predictors = predictors
        ) |>
        # standard preprocessing
        add_preprocessing() |>
        # simple imputation for numeric
        add_imputation() |>
        # create dummies
        # include most mechanics
        add_dummies(mechanics) %>%
        # include all categories
        add_dummies(categories) %>%
        # include some families
        add_dummies(families,
                    ...) |>
        # remove zero variance
        step_zv(all_numeric_predictors()) |>
        # remove highly correlated
        step_corr(all_numeric_predictors(), threshold = 0.95)
}

glmnet_spec = function() {
    linear_reg(
        engine = "glmnet",
        penalty = tune::tune(),
        mixture = tune::tune()
    )
}

glmnet_grid = function() {
    
    grid_regular(
        penalty(range = c(-4, -1)),
        mixture(),
        levels = c(mixture = 5,
                   penalty = 10)
    )
}

lightgbm_spec = function(trees = 500, ...) {
        
        
        require(bonsai)
        
        parsnip::boost_tree(
                mode = "regression",
                trees = trees,
                min_n = tune(),
                tree_depth = tune(),
                ...) |>
                set_engine("lightgbm")
}

lightgbm_grid = 
        function(size = 15) {
                
                grid_max_entropy(
                        x = dials::parameters(
                                min_n(), # 2nd important
                                tree_depth() # 3rd most important
                        ),
                        size = size
                )
        }


# function to build a recipe and apply series of steps given an outcome
build_outcome_recipe = 
    function(data,
             outcome,
             ids,
             predictors,
             splines) {
        
        data |>
            build_recipe(
                outcome = {{outcome}},
                ids = ids,
                predictors = predictors
            ) |>
            # standard preprocessing
            add_preprocessing() |>
            # simple imputation for numeric
            add_imputation() |>
            # create dummies
            add_bgg_dummies() |>
            # add splines for nonlinearities
            # splines with a fourth degree polynomial for year
            add_splines(vars = "year", degree = 4) |>
            # splines with fifth degree polynomials for mechanics/categories
            add_splines(vars = splines) |>
            # remove zero variance
            step_zv(all_numeric_predictors()) |>
            # remove highly correlated
            step_corr(all_numeric_predictors(), threshold = 0.95) |>
            # normalize
            step_normalize(all_numeric_predictors())
    }

# basic recipe setup
build_recipe = function(data,
                        outcome,
                        ids,
                        predictors,
                        ...) {
    
    recipe(x=data) %>%
        # set ids
        update_role(
            any_of(ids),
            new_role = "id"
        ) %>%
        # set predictors
        update_role(
            any_of(predictors),
            new_role = "predictor"
        ) %>%
        # set outcome
        update_role(
            {{ outcome }},
            new_role = "outcome"
        ) %>%
        # set anything else as id
        update_role(
            -has_role("predictor"),
            -has_role("outcome"),
            new_role = "id"
        ) 
}

# standardized steps for preprocessing bgg games
add_bgg_preprocessing = function(recipe,
                                 ...) {
    
    recipe |>
        step_rm(has_role("extras")) |>
        add_preprocessing() |>
        add_imputation() |>
        add_bgg_dummies(...)
}

# standardized steps for preprocessing bgg games for linear models
# splines + normalization
add_linear_preprocessing = function(recipe) {
    
    recipe |>
        # spline for year
        add_splines(vars = "year", degree = 4) |>
        # splines with fifth degree polynomials for mechanics/categories
        add_splines(c("number_mechanics", "number_categories")) |>
        # remove zero variance
        add_zv() |>
        add_normalize()
}


# function for extracting dummies from nominal features
add_dummies = function(recipe,
                       variable,
                       threshold = 100) {
    
    variable = enquo(variable)
    
    recipe %>%
        # tokenize 
        step_dummy_extract(!!variable,
                           sep = ", ",
                           other = "remove_other_field",
                           threshold = threshold) %>%
        # remove other var
        step_rm(contains("remove_other_field"))
    
    
}

# standard dummy recipes
add_bgg_dummies = function(recipe,
                           mechanics_threshold = 1,
                           categories_threshold = 1,
                           families_threshold = 100,
                           publishers_threshold = 15,
                           designers_threshold = 10,
                           artists_threshold = 15,
                           components_threshold = 25,
                           themes_threshold = 25,
                           mechanisms_threshold = 25
) {
    
    recipe %>%
        # include most mechanics
        add_dummies(mechanics,
                    threshold = mechanics_threshold) %>%
        # include all categories
        add_dummies(categories,
                    threshold = categories_threshold) %>%
        # families
        add_dummies(families,
                    threshold = families_threshold) %>%
        # publishers
        add_dummies(publishers,
                    threshold = publishers_threshold) %>%
        # designers
        add_dummies(designers,
                    threshold = designers_threshold) %>%
        # artists
        add_dummies(artists,
                    threshold = artists_threshold) %>%
        # components
        add_dummies(components,
                    threshold = components_threshold) %>%
        # themes
        add_dummies(themes,
                    threshold = themes_threshold) %>%
        # mechanisms
        add_dummies(mechanisms,
                    threshold = mechanisms_threshold)
}


# standardized preprocessing
add_preprocessing = function(recipe) {
    
    recipe %>%
        # indicate missingness in numeric features
        step_indicate_na(all_numeric_predictors(),
                         prefix = "missing") %>%
        # indicate missingness in image, description, or thumbnail
        step_indicate_na(image, thumbnail,
                         prefix = "missing") %>%
        update_role(image, thumbnail,
                    new_role = "id") %>%
        # make time per player variable
        step_mutate(time_per_player = playingtime/ maxplayers) %>% 
        # remove zero variance predictors
        step_zv(all_predictors()) %>%
        # number_mechanics
        step_mutate(number_mechanics = 
                        dplyr::case_when(
                            is.na(mechanics) ~ 0,
                            TRUE ~ stringr::str_count(mechanics, ',') + 1
                        )
        ) %>%
        # number categories
        step_mutate(number_categories = 
                        dplyr::case_when(
                            is.na(categories) ~ 0,
                            TRUE ~ stringr::str_count(categories, ',') + 1
                        )
        ) %>%
        # log time per player and playingtime
        step_log(time_per_player,
                 playingtime,
                 offset = 1) %>%
        # truncate yearpublished
        step_mutate(year = dplyr::case_when(yearpublished < 1900 ~ 1900,
                                            TRUE ~ yearpublished),
                    role = "predictor") %>%
        # indicator for published before 1900
        step_mutate(published_before_1900 = dplyr::case_when(yearpublished < 1900 ~ 1,
                                                             TRUE ~ 0)) %>%
        # solo game
        # big box/deluxe/anniversary edition
        step_mutate(deluxe_edition = dplyr::case_when(grepl("kickstarter|big box|deluxe|mega box", tolower(name))==T ~ 1,
                                                      TRUE ~ 0)) %>%
        # description word count
        step_mutate(word_count = stringi::stri_count_words(description)) %>%
        step_mutate(word_count = tidyr::replace_na(word_count, 0)) %>%
        # magical phrase in description
        step_mutate(description_from_publisher = dplyr::case_when(grepl("description from publisher", tolower(description))==T ~ 1,
                                                                  TRUE ~ 0))
}


# imputation
add_imputation = function(recipe) {
    
    recipe %>%
        # impute missingness in selected features with median
        step_impute_median(playingtime,
                           minplayers,
                           maxplayers,
                           minage,
                           time_per_player) %>% # medianimpute numeric predictors
        # truncate minage to no greater than 18
        step_mutate(minage = dplyr::case_when(minage > 18 ~ 18,
                                              minage < 0 ~ 0,
                                              TRUE ~ minage)) %>%
        # truncate player counts
        step_mutate(minplayers = dplyr::case_when(minplayers > 10 ~ 10,
                                                  TRUE ~ minplayers)) %>%
        step_mutate(maxplayers = dplyr::case_when(maxplayers > 20 ~ 10,
                                                  maxplayers <=1 ~ 1,
                                                  TRUE ~ maxplayers)) %>%
        step_rm(minplaytime, maxplaytime)
}


# normalize all numeric predictors
add_normalize = function(recipe) {
    
    recipe %>%
        step_normalize(all_numeric_predictors())
}


# add pca
add_pca = function(recipe,
                   ...) {
    
    recipe %>%
        step_pca(all_numeric_predictors(),
                 ...)
}

# add corr
add_corr = function(recipe,
                    my_threshold = 0.9) {
    
    recipe %>%
        step_corr(all_numeric_predictors(),
                  threshold = my_threshold)
    
}


# step to remove zero variance
add_zv = function(recipe) {
    
    recipe %>%
        step_zv(all_numeric_predictors())
    
}

# discretize variables with cart
add_discrete = function(recipe,
                        outcome,
                        vars = c("year",
                                 "number_mechanics",
                                 "number_categories")) {
    
    outcome = enquo(outcome)
    
    # embed categorical feature via mixed
    step_discretize_feature = function(recipe,
                                       feature,
                                       outcome) {
        
        feature = enquo(feature)
        outcome = enquo(outcome)
        
        recipe %>%
            embed::step_discretize_cart(
                !!feature,
                outcome = vars(!!outcome))
    }
    
    for (i in 1 :length(vars)) {
        
        recipe = recipe %>%
            step_discretize_feature(feature = !!vars[i],
                                    outcome = !!outcome)
        # add_role(!!vars[i],
        #          new_role = "discrete")
        
    }
    
    recipe %>%
        step_dummy(all_nominal_predictors())
    # recipe %>%
    #         step_novel(any_of(has_role("discrete"))) %>%
    #         step_dummy(has_role("discrete"))
    
}

# splines
# add splines for nonlinear effects for linear models
add_splines= function(recipe,
                      vars = c("year",
                               "number_mechanics",
                               "number_categories"),
                      degree = 5) {
    
    
    step_spline_feature = function(recipe,
                                   feature,
                                   ...) {
        
        feature = enquo(feature)
        
        recipe %>%
            # add splines
            step_ns(
                !!feature,
                deg_free = degree)
    }
    
    for (i in 1 :length(vars)) {
        
        recipe = recipe %>%
            step_spline_feature(feature = !!vars[i])
        
    }
    
    recipe
    
    
}



# 
bgg_outcomes = function() {
    
    c("average", "averageweight", "bayesaverage", "averageweight")
}

# simulate from a linear model
predict_sim = function(fit,
                       new_data,
                       nsims = 1) {
    
    simulateX <- function(object, nsim = 1, seed = NULL, X, ...) {
        
        object$fitted.values <- predict(object, X)
        simulate(object = object, nsim = nsim, seed = seed, ...)
    }
    
    mod = 
        extract_fit_engine(fit)
    
    recipe =
        extract_recipe(fit)
    
    prepped =
        recipe %>%
        bake(new_data = new_data)
    
    simulateX(mod,
              nsim = nsims,
              X = prepped) %>%
        tibble()
    
}

# define training and valid split
make_train_valid_split = function(data,
                                  end_train_year,
                                  valid_window = 2) { 
    
    # training set: games published before end train year
    train =
        data %>%
        filter(yearpublished <= end_train_year)
    
    # valid set: games that have been out for at least 2 years post end train year
    valid =
        data %>%
        filter(yearpublished > end_train_year & yearpublished <= end_train_year + valid_window)
    
    # create custom split
    # make a split for validating on a specific set of years
    make_splits(
        list(analysis =
                 seq(nrow(train)),
             assessment =
                 nrow(train) + seq(nrow(valid))),
        bind_rows(train,
                  valid)
    )
    
}

# make resamples given data
make_resamples = function(data,
                          outcome,
                          seed = 1999) {
    
    missing = data %>%
        filter(is.na( {{ outcome }} )) %>%
        pull({{ outcome}})
    
    # check for missingness
    if (length(missing) > 0) {
        
        stop(rlang::englue(paste(length(missing), "missing observations in {{outcome}}")))
        
    }
    
    # otherwise, create folds
    resamples = vfold_cv(data,
                         v = 5,
                         strata = {{outcome}})
    
    message(rlang::englue(paste("creating training and resamples for {{outcome}} with",  nrow(data), "obs")))
    
    resamples
    
}


# create workflow sets given data, recipes, outcome, and models
create_workflows = function(data,
                            recipes, 
                            models) {
    
    # create workflow set
    workflow_set(
        preproc = recipes,
        models = models,
        cross = T
    )
    
}

# create function for tuning a workflow set via race
tune_workflows = function(wflows,
                          resamples,
                          seed = 1999,
                          method = "tune_race_anova",
                          ...) {
    
    if (method == 'tune_race_anova') {
        
        wflows %>%
            workflow_map(
                "tune_race_anova",
                seed = seed,
                control = control_race(),
                resamples = resamples,
                metrics = reg_metrics()
            )
    } else {
        
        wflows %>%
            workflow_map(
                "tune_grid",
                seed = seed,
                control = control_grid(),
                resamples = resamples,
                metrics = reg_metrics()
            )
    }
    
}

# control for resamples
control_grid = function() {
    
    control_resamples(save_pred = TRUE, 
                      save_workflow = T,
                      allow_par = T,
                      verbose = T,
                      parallel_over = "resamples")
    
}

# control for racing
control_race = function() {
    
    finetune::control_race(
        save_pred = TRUE,
        parallel_over = "resamples",
        verbose = TRUE,
        verbose_elim = TRUE,
        save_workflow = T
    )
}

# preprocess data prior to training
preprocess_outcome = function(data,
                              outcome,
                              ratings = 25,
                              weights = 5) {
    
    # remove missingness on outcome
    temp = 
        data %>%
        # remove observations with missing outcomes
        filter(!is.na({{outcome}})) %>%
        # filter games above minimum number of ratings
        filter(usersrated >= ratings)
    
    if (outcome == 'usersrated') {
        
        temp %>%
            mutate(usersrated = log(usersrated))
        
    } else if (outcome == 'averageweight') { 
        
        temp %>%
            filter(numweights > weights)
    } else {
        
        temp
    }
}


# impute averageweight
impute_averageweight = function(data,
                                model) {
    
    model %>%
        augment(new_data = data) %>%
        mutate(.pred = truncate_averageweight(.pred)) %>%
        rename(est_averageweight = .pred)
    
}

# truncate
truncate_averageweight = function(x) {
    
    
    case_when(x > 5 ~ 5,
              x < 1 ~ 1,
              TRUE ~ x)
    
}

# sim averageweight
sim_averageweight = function(data,
                             fit,
                             sims = 1) {
    
    sims = fit %>%
        predict_sim(new_data = data,
                    nsims = sims) %>%
        mutate(across(starts_with("sim_"),
                      truncate_averageweight)) %>%
        bind_cols(., data)
    
    sims %>%
        pivot_longer(cols = starts_with("sim_"),
                     names_to = "sim",
                     values_to = "est_averageweight") %>%
        nest(data = -sim)
    
}


round_usersrated = function(log_usersrated,
                            round = 50) {
    
    plyr::round_any(log_usersrated, accuracy = round, f = ceiling)
    
    
}


calculate_bayesaverage = function(data, ratings = 2000) {
    
    data %>%
        mutate(.pred_bayesaverage = 
                   # numerator
                   ((ratings * 5.5) + (.pred_average * .pred_usersrated)) /
                   # denominator
                   (ratings + .pred_usersrated)
        )
}

predict_outcomes = function(data,
                            averageweight_model,
                            average_model,
                            usersrated_model) {
    
    data %>%
        impute_averageweight(
            model = averageweight_model
        ) %>%
        mutate(.pred_hurdle = NA) %>%
        mutate(bayesaverage = replace_na(bayesaverage, 5.5)) %>%
        predict_average(
            model = average_model
        ) %>%
        predict_usersrated(
            model = usersrated_model
        ) %>%
        calculate_bayesaverage() %>%
        mutate(.pred_averageweight = est_averageweight) 
    
}

predict_average = function(data,
                           model) {
    
    model %>%
        augment(data) %>%
        rename(.pred_average = .pred)
    
}

predict_usersrated = function(data,
                              model,
                              round = 50) {
    
    model %>%
        augment(data) %>%
        mutate(.pred = round_usersrated(exp(.pred), round)) %>%
        rename(.pred_usersrated = .pred)
    
}

predict_hurdle = 
    function(data,
             workflow) {
        
        workflow %>%
            # predict with hurdle
            augment(data,
                    type = 'prob') %>%
            # rename pred to hurdle
            rename(pred_hurdle = .pred_yes) %>%
            # remove extraneous predictions
            select(-.pred_no,
                   -.pred_class)
        
    }


simulate_outcomes = function(data,
                             averageweight_fit,
                             average_fit,
                             usersrated_fit,
                             sims = 1,
                             ...) {
    
    
    simulateX <- function(object, nsim = 1, seed = NULL, X, ...) {
        
        object$fitted.values <- predict(object, X)
        simulate(object = object, nsim = nsim, seed = seed, ...)
    }
    
    averageweight_mod = 
        extract_fit_engine(averageweight_fit)
    
    average_mod =
        extract_fit_engine(average_fit)
    
    usersrated_mod = 
        extract_fit_engine(usersrated_fit)
    
    prepped =
        extract_recipe(averageweight_fit) %>%
        bake(new_data = data)
    
    # imputed = 
    #         data  %>%
    #         impute_averageweight(
    #                 data = .,
    #                 fit = averageweight_fit
    #         )
    
    # prepped_outcome = 
    #         extract_recipe(average_fit) %>%
    #         bake(new_data = imputed)
    
    simulate_averageweight = function(...) {
        
        simulateX(averageweight_mod,
                  nsim = sims,
                  X = prepped) %>%
            mutate(across(everything(), truncate_averageweight))
        
    }
    
    message("simulating averageweight...")
    
    sims_averageweight = simulate_averageweight()
    
    simulate_outcomes = function(...) {
        
        foreach(i=1:ncol(sims_averageweight), .combine = 'bind_rows') %do% {
            
            message(i)
            
            prepped_outcome =
                extract_recipe(average_fit) %>%
                bake(new_data = data %>%
                         mutate(est_averageweight = sims_averageweight[,i])
                )
            
            tibble(
                .pred_average = 
                    simulateX(average_mod,
                              nsim = 1,
                              X = prepped_outcome) %>%
                    pull(),
                .pred_usersrated = 
                    simulateX(usersrated_mod,
                              nsim = 1,
                              X = prepped_outcome) %>%
                    pull()
            ) %>%
                mutate(sim = paste("sim", i, sep = "_"),
                       .row = row_number())
            
        }
        
    }
    
    message("simulating average and usersrated...")
    
    simulate_outcomes() %>%
        bind_cols(.,
                  sims_averageweight %>%
                      pivot_longer(cols = starts_with("sim_"),
                                   names_to = "sim",
                                   values_to = "est_averageweight") %>%
                      select(-sim)
        ) %>%
        left_join(.,
                  data %>%
                      mutate(.row = row_number()) %>%
                      select(.row, game_id, name, yearpublished)) %>%
        select(.row, yearpublished, game_id, name, everything())
    #                      names_to = "sim",
    #                      values_to = "est_averageweight") %>%
    
    
    # simulate_outcome = function(mod,
    #                             prepped_data,
    #                             ...) {
    # 
    #         # get coefs
    #         coefs = coef(mod) %>% replace_na(., 0)
    # 
    #         # get variance-covariance matrix
    #         cov = vcov(mod)
    #         cov[is.na(cov)]=0
    # 
    #         # simulate coefficients
    #         betas = MASS::mvrnorm(n = 1,
    #                               mu = coefs,
    #                               Sigma = cov) %>%
    #                 data.frame() %>%
    #                 as.matrix()
    # 
    #         foreach(i=1:ncol(sims_averageweight)) %do% {
    # 
    #                 message(i)
    # 
    #                 xi = prepped_data %>%
    #                         mutate(est_averageweight = sims_averageweight[,i]) %>%
    #                         select(any_of(names(coefs))) %>%
    #                         as.matrix() %>%
    #                         cbind(1, .)
    # 
    #                 xi %*% betas
    # 
    #         }
    # 
    # }
    # 
    # sims_averageweight = simulate_averageweight()
    # 
    # simulate_outcome(average_mod,
    #                  prepped_data = prepped_outcome)
    
    # simulate_outcome = function(mod,
    #                             ...) {
    #
    #         foreach(i=1:ncol(sims_averageweight),
    #                 .combine = 'bind_rows') %dopar% {
    #
    #                         message(i)
    #
    #                         simulateX(mod,
    #                                   nsim = 1,
    #                                   X = prepped_average %>%
    #                                           mutate(est_averageweight = sims_averageweight[i,])
    #                         ) %>%
    #                                 mutate(sim = paste("sim_i"))
    #                 }
    #
    # }
    
    # sims_average = simulate_outcome(mod = average_mod)
    # sims_usersrated = simulate_outcome(mod = usersrated_mod)
    # 
    # bind_cols(sims_average,
    #           sims_usersrated)
    
    # 
    # simulate_average = function(...) {
    #         
    #                 simulateX(average_mod,
    #                           nsim = 1,
    #                           X = prepped_average %>%
    #                                   mutate(est_averageweight = sim_averageweight))
    #         
    #         sim_usersrated =
    #                 simulateX(average_mod,
    #                           nsim = 1,
    #                           X = prepped_average %>%
    #                                   mutate(est_averageweight = sim_averageweight)) %>%
    #                 pull()
    #         
    #         sim_usersrated
    #         
    #         # bind_cols(
    #         #         sim_averageweight,
    #         #         sim_average,
    #         #         sim_usersrated
    #         # )
    #         # calculate_bayesaverage() %>%
    #         # pull(.pred_bayesaverage)
    #         
    # }
    # 
    # simulate_averageweight()
    
    # out = list()
    # 
    # for (i in 1:sims) {
    #         
    #         message(i)
    #         
    #         out[[i]] = simulate_bayesaverage()
    #         
    # }
    
    #future.apply::future_replicate(sims, simulate_bayesaverage())
    
    # sim_average
    #                        
    #         
    # list(mod = average_fit,
    #      prepped = prepped_average %>%
    #              mutate(est_averageweight = sim_averageweight)
    # )
    
    # sims %>%
    #         
    
    # sims_averageweight = 
    #         simulateX(averageweight_mod,
    #                   nsim = sims,
    #                   X = prepped)
    # 
    # sims_imputed =
    #         sims_averageweight %>%
    #         bind_cols(.,
    #                   prepped_average) %>%
    #         mutate(across(starts_with("sim_"),
    #                       truncate_averageweight)) %>%
    #         pivot_longer(cols = starts_with("sim_"),
    #                      names_to = "sim",
    #                      values_to = "est_averageweight") %>%
    #         nest(data = -sim)
    # # 
    # sims_average = 
    #         sims_imputed %>%
    #         mutate(average = furrr::future_map(data,
    #                               ~ simulateX(average_mod,
    #                                           nsim = 1,
    #                                           X = .x) %>%
    #                                       rename(.pred_average = sim_1)))
    # 
    # sims_usersrated = 
    #         sims_imputed %>%
    #         mutate(usersrated = furrr::future_map(data,
    #                                  ~ simulateX(usersrated_mod,
    #                                              nsim = 1,
    #                                              X = .x) %>%
    #                                          rename(.pred_usersrated = sim_1)))
    # 
    # list("sims_averageweight" = 
    #              sims_averageweight %>%
    #              mutate(across(starts_with("sim_"),
    #                            truncate_averageweight)) %>%
    #              pivot_longer(cols = starts_with("sim_"),
    #                           names_to = "sim",
    #                           values_to = ".pred_averageweight") %>%
    #              nest(data = -sim) %>%
    #              rename(averageweight = data) %>%
    #              select(sim, averageweight),
    #      "sims_average" = 
    #              sims_average %>%
    #              select(sim, average),
    #      "sims_usersrated" = 
    #              sims_usersrated %>%
    #              select(sim, usersrated),
    #      "data" = data)
    
    # sims_averageweight
    
}
