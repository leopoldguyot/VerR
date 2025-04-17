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
#' envCreate("my_env", packages = c("digest@0.6.18"))
#' runInEnv(packageVersion("digest"), "my_env")
#' runInEnv(packageVersion("digest"))
#'
#' @importFrom callr r
#' @importFrom renv load
#' @export
runInEnv <- function(expr, envName = envList()) {
    results <- list()
    if (length(envName) == 1) {
        return(.runInSingleEnv(substitute(expr), envName[1]))
    }
    for (env in envName) {
        cat("Running expression in environment:", env, "\n")
        result <- tryCatch(
            {
                .runInSingleEnv(substitute(expr), env)
            },
            error = function(e) {
                message("Error in environment ", env, ": ", conditionMessage(e))
                NULL
            }
        )
        results[[env]] <- result
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




#' Benchmark multiple expressions in multiple environments
#'
#' Measures the execution time for an R expression across multiple environments.
#' The function runs the expression multiple times
#' (controlled by `rep` parameter) and returns the execution times for each run
#' in the specified environments.
#'
#' @param expr An R expression to be evaluated.
#' @param envName A `character()` vector specifying the environment(s) name(s).
#'                If more than one environment is specified, the expression will
#'                be evaluated in each one.
#' @param rep An `integer(1)` specifying the number of repetitions for
#'  each expression. Default is 3.
#' @param setup An optional `expression` to be evaluated before the
#' benchmarked expression. For instance, it can be used to load libraries or
#' retrieve data.
#'
#' @param resultAggregation A function to aggregate the results.
#' By default no aggregation is done, the results will be returned as a numeric
#' vectors for each environment.
#'
#' @return
#' A list of execution times for each environment:
#' \itemize{
#'   \item A numeric vector of execution times for the evaluated expression
#' if `length(envName) == 1`.
#'   \item A list of numeric vectors of execution times for each
#' environment if `length(envName) > 1`.
#' }
#' The result contains the user time (in seconds) for each repetition of
#' the expression.
#'
#' @note This function is **experimental** and may change in future versions.
#' Use at your own risk.
#'
#' @examples
#' envCreate("my_env", packages = c("digest@0.6.18"))
#' benchInEnv(Sys.sleep(1), "my_env", rep = 3, resultAggregation = mean)
#'
#' @importFrom callr r
#' @importFrom renv load
#' @export
benchInEnv <- function(
        expr,
        envName = envList(),
        rep = 3,
        setup = NULL,
        resultAggregation = NULL) {
    results <- list()
    if (length(envName) == 1) {
        return(.benchInSingleEnv(substitute(expr),
            envName = envName[1],
            rep = rep,
            setup = substitute(setup)
        ))
    }
    for (env in envName) {
        cat("\nBenchmarking expression in environment:", env, "\n")
        result <- tryCatch(
            {
                .benchInSingleEnv(substitute(expr),
                    envName = env,
                    rep = rep,
                    setup = substitute(setup)
                )
            },
            error = function(e) {
                message("Error in environment ", env, ": ", conditionMessage(e))
                NULL
            }
        )
        results[[env]] <- result
    }
    if (is.null(resultAggregation)) {
        return(results)
    }
    return(lapply(results, resultAggregation))
}

#' Run expression in a specified environment and benchmark its execution
#'
#' Executes an R expression within a specific environment,
#' benchmarks its execution time,
#' and returns the elapsed user time for each repetition.
#'
#' @param expr An R expression to be evaluated.
#' @param envName A `character(1)` specifying the environment name.
#'                The environment should be present in the ".envs/" directory.
#' @param rep An `integer(1)` specifying the number of repetitions
#' for the expression. Default is 3.
#'
#' @return A numeric vector of user times (in seconds) for each repetition of
#' the expression.
#'
#' @importFrom callr r
#' @importFrom renv load
#' @noRd
.benchInSingleEnv <- function(expr, envName, rep, setup) {
    envPath <- file.path(".envs", envName)
    if (!dir.exists(envPath)) {
        stop("Environment does not exist: ", envName)
    }
    if (typeof(expr) == "list") expr <- substitute(expr)
    if (typeof(setup) == "list") setup <- substitute(setup)
    result <- callr::r(
        function(envPath, expr, rep, setup) {
            if (!requireNamespace("renv", quietly = TRUE)) {
                stop("The 'renv' package is required. Install it using install.packages('renv').")
            }
            setwd(envPath)
            renv::load(project = ".")
            eval(setup)
            vapply(
                1:rep,
                function(i) {
                    system.time(eval(expr))[3]
                },
                numeric(1)
            )
        },
        args = list(envPath = envPath, expr = expr, rep = rep, setup = setup),
        stdout = NULL, stderr = NULL
    )
    result
}
