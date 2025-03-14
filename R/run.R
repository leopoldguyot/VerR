#' Run expressions in environments
#'
#' These functions allow execution of R expressions
#'  within specified environments.
#'\itemize{
#'   \item `runInEnv(expr, env)`: Runs an expression in a
#'    specific environment.
#'   \item `runInEnvs(expr, envs)`: Runs an expression across
#'    multiple environments.
#' }
#' @param expr An R expression to be evaluated.
#' @param env A `character(1)` string specifying the environment name.
#'
#' @return
#' \itemize{
#'   \item The result of the evaluated expression for `runInEnv`.
#'   \item A list of results for each environment for `runInEnvs`.
#' }
#' @examples
#' createEnv("my_env", packages = c("digest@0.6.18"))
#' runInEnv(packageVersion("digest"), "my_env")
#' runInEnvs(packageVersion("digest"))
#'
#' @importFrom callr r
#' @importFrom renv load
#' @rdname run_in_environment
#' @export
runInEnv <- function(expr, env) {
    globalWd <- getwd()
    on.exit(setwd(globalWd), add = TRUE)
    envPath <- file.path(".envs", env)
    if (!dir.exists(envPath)) {
        stop("Environment does not exist: ", env)
    }
    setwd(envPath)
    if (typeof(expr) == "list") expr <- substitute(expr)
    result <- callr::r(
        function(envPath, expr) {
            library(renv)
            renv::load(project = ".")
            eval(expr)
        },
        args = list(envPath = envPath, expr = expr),
        stdout = "", stderr = ""
    )
    setwd(globalWd)
    return(result)
}

#' @param envs A `character()` vector of environment names.
#'  Default is `listEnvs()`.
#'
#' @importFrom callr r
#' @importFrom renv load
#' @rdname run_in_environment
#' @export
runInEnvs <- function(expr, envs = listEnvs()) {
    results <- list()
    for (env in envs) {
        cat("Running expression in environment:", env, "\n")
        result <- tryCatch({
            runInEnv(env, substitute(expr))
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
