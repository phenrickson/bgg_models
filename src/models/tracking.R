write_tracking = function(metrics, details, file = 'targets-runs/tracking.csv') {
    
    targets_tracking_details(metrics = metrics,
                             details = details) |>
        mutate_if(is.numeric, round, 3) |>
        add_column(time = Sys.time(),
                   user = system('git config user.email', intern = T)) |>
        select(time, user, everything()) |>
        write.csv(file = file)
}

targets_tracking_details = function(metrics, details) {
    
    bayesaverage_method = 
        get_bayesaverage_method(details)
    
    metrics |>
        pivot_wider(
            names_from = c(".metric"),
            values_from = c(".estimate")
        ) |>
        mutate_if(is.numeric, round, 3) |>
        left_join(
            details,
            by = join_by(outcome)
        ) |>
        select(model = wflow_id,
               params,
               everything()
        ) |>
        unnest(params, keep_empty = T) |>
        mutate(model = replace_na(model, bayesaverage_method)) |>
        select(-.estimator)
    
}

check_workflow_class = function(workflow) {
    
    if (class(workflow) == "vetiver_model") {
        
        wflow = workflow$model |>
            bundle::unbundle()
        
    } else {
        wflow = workflow
    }
    
    wflow
    
}

extract_workflow_params = function(workflow) {
    
    workflow = check_workflow_class(workflow)
    
    spec <- workflows::extract_spec_parsnip(workflow)
    parameter_values <- lapply(spec$args, rlang::get_expr)
    
    parameter_values |> 
        enframe() |> 
        unnest(c(value), keep_empty = F) |>
        pivot_wider()
}

extract_workflow_outcome = function(workflow) {
    
    workflow |> 
        check_workflow_class() |>
        extract_mold() |>
        pluck("blueprint") |>
        pluck("ptypes") |>
        pluck("outcomes") |>
        names()
    
}

get_bayesaverage_method = function(details) {
    
    if (
        (nrow(details |> filter(outcome == 'bayesaverage')) == 0)
    ) {
        bayesaverage_method = 
            paste(
                details |>
                    filter(outcome == 'average') |>
                    pull(wflow_id),
                details |>
                    filter(outcome == 'usersrated') |>
                    pull(wflow_id),
                sep = "+"
            )
    } else {
        bayesaverage_method = 
            details |> 
            filter(outcome == 'bayesaverage') |>
            pull(wflow_id)
    }
    
    bayesaverage_method
}

extract_workflow_outcome = function(workflow) {
    
    workflow |>
        extract_mold() |>
        pluck("outcomes") |>
        names()
}

extract_workflow_engine = function(workflow) {
    
    workflow |>
        check_workflow_class() |>
        extract_spec_parsnip() |>
        pluck("engine")
}

extract_workflow_details = function(workflow) {
    
    outcome = 
        workflow |>
        extract_workflow_outcome()
    
    engine = 
        workflow |>
        extract_workflow_engine()
    
    params = 
        workflow |> 
        extract_workflow_params()
    
    tibble(
        outcome = outcome,
        engine = engine,
        params = list(params)
    )
    
}
