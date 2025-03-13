runInEnvironment <- function(env, expr) {
    envPath <- file.path(".envs", env)
    if (!dir.exists(envPath)) {
        stop("Environment does not exist: ", env)
    }
    if (typeof(expr) == "list") expr <- substitute(expr)
    result <- callr::r(
        function(envPath, expr) {
            library(renv)

            renv::load(project = envPath)

            # add local path to .libPaths()
            libPath <- c(.libPaths(), renv::paths$library(project = envPath))
            .libPaths(libPath)

            eval(expr, envir = .GlobalEnv)
        },
        args = list(envPath = envPath, expr = expr)
    )
    return(result)
}

runInEnvironments <- function(expr, envs = listEnvironments()) {
    results <- list()
    for (env in envs) {
        cat("Running expression in environment:", env, "\n")
        result <- tryCatch({
            runInEnvironment(env, substitute(expr))
        }, error = function(e) {
            message("Error in environment ", env, ": ", conditionMessage(e))
            return(NULL)
        })
        if (!is.null(result)) {
            results[[env]] <- result
        }
    }
    return(results)
}
