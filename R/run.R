#' Run expressions in environments
#'
#' These functions allow execution of R expressions
#'  within specified environments.
#'\itemize{
#'   \item `runInEnvironment(expr, env)`: Runs an expression in a
#'    specific environment.
#'   \item `runInEnvironments(expr, envs)`: Runs an expression across
#'    multiple environments.
#' }
#' @param expr An R expression to be evaluated.
#' @param env A `character(1)` string specifying the environment name.
#'
#' @return
#' \itemize{
#'   \item The result of the evaluated expression for `runInEnvironment`.
#'   \item A list of results for each environment for `runInEnvironments`.
#' }
#' @examples
#' createEnvironment("my_env", packages = c("digest@0.6.18"))
#' runInEnvironment(packageVersion("digest"), "my_env")
#' runInEnvironments(packageVersion("digest"))
#'
#' @importFrom callr r
#' @importFrom renv load
#' @rdname run_in_environment
#' @export
runInEnvironment <- function(expr, env) {
    envPath <- file.path(".envs", env)
    if (!dir.exists(envPath)) {
        stop("Environment does not exist: ", env)
    }
    if (typeof(expr) == "list") expr <- substitute(expr)
    result <- callr::r(
        function(envPath, expr) {
            library(renv)
            renv::load(project = envPath)
            libPath <- renv::paths$library(project = envPath)
            .libPaths(libPath)
            eval(expr, envir = .GlobalEnv)
        },
        args = list(envPath = envPath, expr = expr)
    )
    return(result)
}

#' @param envs A `character()` vector of environment names.
#'  Default is `listEnvironments()`.
#'
#' @importFrom callr r
#' @importFrom renv load
#' @rdname run_in_environment
#' @export
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
