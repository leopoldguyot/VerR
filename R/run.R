#' Run expressions in environments
#'
#' Execute R expressions in multiple environments.
#' @param expr An R expression to be evaluated.
#' @param envName A `character()` specifying the environment(s) name(s).
#'
#' @return
#' \itemize{
#'   \item The result of the evaluated expression when length(envName) == 1.
#'   \item A list of results for each environment when length(envName) > 1.
#' }
#' @examples
#' createEnv("my_env", packages = c("digest@0.6.18"))
#' runInEnv(packageVersion("digest"), "my_env")
#' runInEnv(packageVersion("digest"))
#'
#' @importFrom callr r
#' @importFrom renv load
#' @export
runInEnv <- function(expr, envNames = listEnvs()) {
    results <- list()
    if (length(envNames) == 1) {
        return(.runInSingleEnv(substitute(expr), envNames[1]))
    }
    for (env in envNames) {
        cat("Running expression in environment:", env, "\n")
        result <- tryCatch(
            {
                .runInSingleEnv(substitute(expr), env)
            },
            error = function(e) {
                message("Error in environment ", env, ": ", conditionMessage(e))
                return(NULL)
            }
        )
        if (!is.null(result)) {
            results[[env]] <- result
        }
    }
    return(results)
}

#' Run expression in a specified environment
#'
#' @param expr An R expression to be evaluated.
#' @param envName A `character(1)` specifying the environment name.
#'  The environment should be present in the ".envs/" directory.
#'
#' @return The value returned by the evaluation of the expression
#'
#' @importFrom callr r
#' @importFrom renv load
#' @noRd
.runInSingleEnv <- function(expr, envName) {
    envPath <- file.path(".envs", envName)
    if (!dir.exists(envPath)) {
        stop("Environment does not exist: ", envName)
    }
    if (typeof(expr) == "list") expr <- substitute(expr)
    result <- callr::r(
        function(envPath, expr) {
            if (!requireNamespace("renv", quietly = TRUE)) {
                stop("The 'renv' package is required. Install it using install.packages('renv').")
            }
            setwd(envPath)
            renv::load(project = ".")
            eval(expr)
        },
        args = list(envPath = envPath, expr = expr),
        stdout = "", stderr = ""
    )
    result
}
