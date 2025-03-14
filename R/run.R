#' Run expressions in environments
#'
#' These functions allow execution of R expressions
#'  within specified environments.
#'\itemize{
#'   \item `runInEnv(expr, envName)`: Runs an expression in a
#'    specific environment.
#'   \item `runInEnvs(expr, envNames)`: Runs an expression across
#'    multiple environments.
#' }
#' @param expr An R expression to be evaluated.
#' @param envName A `character(1)` string specifying the environment name.
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
runInEnv <- function(expr, envName) {
    globalWd <- getwd()
    on.exit(setwd(globalWd), add = TRUE)
    envPath <- file.path(".envs", envName)
    if (!dir.exists(envPath)) {
        stop("Environment does not exist: ", envName)
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

#' @param envNames A `character()` vector of environment names.
#'  Default is `listEnvs()`.
#'
#' @importFrom callr r
#' @importFrom renv load
#' @rdname run_in_environment
#' @export
runInEnvs <- function(expr, envNames = listEnvs()) {
    results <- list()
    for (env in envNames) {
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
