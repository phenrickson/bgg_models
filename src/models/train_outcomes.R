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

train_outcome_model = function(data,
                               outcome,
                               models = linear_models(),
                               metrics = reg_metrics(),
                               remove_wflow_ids = "^trees_",
                               tune_method = "tune_race_anova",
                               end_train_year = 2020,
                               valid_window = 2,
                               predictors = predictor_vars(),
                               ids = id_vars(),
                               splines = spline_vars(),
                               ...) {
        
        # prep data given type of outdcome being trained
        processed_data = 
                data %>%
                preprocess_outcome(outcome = {{outcome}})
        
        # create split
        split = 
                processed_data %>%
                make_train_valid_split(
                        end_train_year = end_train_year,
                        valid_window = valid_window
                )
        
        # extract training and validation
        # analysis
        train = 
                analysis(split)
        
        # assessment
        valid = 
                assessment(split)
        
        # all other observations
        other = 
                processed_data %>%
                filter(!(game_id %in% (bind_rows(train, valid) %>% pull(game_id))))
        
        # resamples for tuning on training set
        resamples =
                train %>%
                make_resamples(
                        outcome = {{outcome}}
                )
        
        # build basic recipe
        recipe = 
                train %>%
                make_recipe(
                        outcome = {{outcome}},
                        predictors = predictors,
                        metrics = metrics,
                ) %>%
                add_bgg_dummies() %>%
                add_preprocessing()
        
        # build other recipes
        recipes = 
                make_outcome_recipes(recipe,
                                     spline_vars = splines)
        
        # make workflows
        workflows =
                workflow_set(
                        preproc =
                                recipes,
                        models =
                                models,
                ) %>%
                # remove ids based on pattern
                filter(!grepl(remove_wflow_ids, wflow_id))
        
        message(paste0("tuning workflows:", 
                      paste(workflows$wflow_id, sep = "\n"))
        )

        # tune
        tuning_results =
                workflows %>%
                tune_workflows(
                        method = tune_method,
                        resamples = resamples,
                )

        # get tuning metrics across all
        tuning_metrics =
                tuning_results %>%
                rank_results(select_best = T)

        # get best fit
        train_fit =
                tuning_results %>%
                fit_best()

        # predict validation
        valid_preds = train_fit %>%
                augment(valid)

        # assess
        valid_metrics =
                valid_preds %>%
                metrics(
                        truth = {{outcome}},
                        estimate = .pred)

        # predict other
        other_preds =
                train_fit %>%
                augment(other)

        # get final fit
        final_fit =
                train_fit %>%
                fit(bind_rows(train,
                              valid))

        # return results
        list(
                "tuning_results" = tuning_results,
                "tuning_metrics" = tuning_metrics,
                "valid_preds" = valid_preds,
                "valid_metrics" = valid_metrics,
                "other_preds" = other_preds,
                "train_fit" = train_fit,
                "final_fit" = final_fit
        )
        
}



# standard metrics used for evaluating regression
reg_metrics = function() {
        
        # # specify regression metrics
        metric_set(yardstick::rmse,
                   yardstick::mae,
                   yardstick::mape,
                   yardstick::rsq)
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
        
        if (rlang::englue("{{outcome}}") == 'averageweight') { 
                
                temp %>%
                        filter(numweights > weights)
        } else {
                
                temp
        }
}

linear_models = function(mixture = 0) {
        
        list(
                "lm" = 
                        linear_reg(mode = "regression") %>%
                        set_engine("lm"),
                "glmnet" = 
                        linear_reg(mode = "regression",
                                   penalty = tune::tune(),
                                   mixture = !!mixture) %>%
                        set_engine("glmnet"),
                "pls" = 
                        pls(
                                mode = "regression",
                                predictor_prop = tune::tune(),
                                num_comp = tune::tune(),
                                engine = "mixOmics"
                        )
        )
}


tree_models = function() {
        
        list(
                "xgbTree" = 
                        boost_tree(
                                trees = 500,
                                min_n = tune(),
                                sample_size = tune(),
                                learn_rate = tune(),
                                tree_depth = tune(),
                                stop_iter = 50
                        ) %>%
                        set_mode("regression") %>%
                        set_engine("xgboost",
                                   eval_metric = 'rmse')
        )
                #,
                
        #         "lightgbm" = 
        #                 parsnip::boost_tree(
        #                         mode = "regression",
        #                         trees = 500,
        #                         min_n = tune(),
        #                         tree_depth = tune(),
        #                 ) %>%
        #                 set_engine("lightgbm")
        # )
}

# impute averageweight
impute_averageweight = function(data,
                                fit) {
        
        fit %>%
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

predict_average = function(data,
                           workflow) {
        
        workflow %>%
                augment(data) %>%
                rename(.pred_average = .pred)
        
}

predict_usersrated = function(data,
                              workflow,
                              round = 50) {
        
        workflow %>%
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
