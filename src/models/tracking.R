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
    parameter_names <- names(spec$args)
    parameter_values <- lapply(spec$args, rlang::get_expr)
    
    parameter_values |>
        as_tibble()
    
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
