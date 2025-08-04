#' Run expressions in environments
#'
#' Execute R expressions in multiple environments.
#' @param expr An R expression or a `character()` (deparsed
#'  R expression) that will be evaluated.
#' @param envName A `character()` specifying the environment(s) name(s).
#' @param parallel A `logical(1)` indicating whether to run
#'  expressions in parallel. Default is `FALSE`.
#' @param ncores An `integer(1)` specifying the number of cores to use
#' for parallel execution. Default is `parallel::detectCores() - 1`.
#'
#' @return
#' \itemize{
#'   \item The result of the evaluated expression when length(envName) == 1.
#'   \item A list of results for each environment when length(envName) > 1.
#' }
#' @examples
#' envCreate("my_env", packages = c("jsonlite@1.9.1"))
#' runInEnv(packageVersion("jsonlite"), "my_env")
#' runInEnv(packageVersion("jsonlite"))
#' envDelete(force = TRUE)
#'
#' @importFrom callr r
#' @importFrom renv load
#' @importFrom parallel makeCluster parLapply stopCluster detectCores
#' @export
runInEnv <- function(
        expr,
        envName = envList(),
        parallel = FALSE,
        ncores = parallel::detectCores() - 1) {
    expr_sub <- substitute(expr)
    if (is.character(expr_sub) && length(expr_sub) == 1) {
        expr_chr <- expr_sub
    } else if (is.symbol(expr_sub)) {
        val <- eval(expr_sub, parent.frame())
        if (!is.character(val)) stop("Symbol must evaluate to a character string.")
        expr_chr <- val
    } else {
        expr_chr <- deparse(expr_sub)
    }

    if (length(envName) == 1) {
        return(.runInSingleEnv(expr_chr, envName[1]))
    }

    if (parallel) {
        # Use parallel execution
        cl <- makeCluster(min(ncores, length(envName)))
        on.exit(stopCluster(cl)) # Ensure the cluster is stopped after execution

        results <- parLapply(cl, envName, function(env) {
            tryCatch(
                {
                    .runInSingleEnv(expr_chr, env)
                },
                error = function(e) {
                    message(
                        "Error in environment ",
                        env, ": ", conditionMessage(e)
                    )
                    NULL
                }
            )
        })
        names(results) <- envName
    } else {
        # Sequential execution
        results <- list()
        for (env in envName) {
            cat("Running expression in environment:", env, "\n")
            result <- tryCatch(
                {
                    print(expr_chr)
                    .runInSingleEnv(expr_chr, env)
                },
                error = function(e) {
                    message("Error in environment ", env, ": ", conditionMessage(e))
                    NULL
                }
            )
            results[[env]] <- result
        }
    }

    return(results)
}

#' Run expression in a specified environment
#'
#' @param expr_chr A `character()`, an deparsed R expression that will
#' be evaluated.
#' @param envName A `character(1)` specifying the environment name.
#'  The environment should be present in the ".envs/" directory.
#'
#' @return The value returned by the evaluation of the expression
#'
#' @importFrom callr r
#' @importFrom renv load
#' @noRd
.runInSingleEnv <- function(expr_chr, envName) {
  envPath <- file.path(".envs", envName)
  if (!dir.exists(envPath)) {
    stop("Environment does not exist: ", envName)
  }

  # Escape quotes and create a properly formatted character vector for R script
  expr_parsed <- chrExprVectorToFormatedChr(expr_chr)
  result_file <- file.path(tempdir(), "result.rds")
  temp_script <- tempfile(fileext = ".R")

  writeLines(sprintf(
    '
    tryCatch({
      setwd("%s")
      if (!requireNamespace("renv", quietly = TRUE)) {
        stop("renv not available")
      }
      renv::load(".")
      result <- withCallingHandlers(
        {
          %s
        },
        warning = function(w) {
          message("Warning: ", conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      )
      saveRDS(list(success = TRUE, result = result), file = "%s")
    }, error = function(e) {
      saveRDS(list(success = FALSE, error = conditionMessage(e)), file = "%s")
    })
    ',
    normalizePath(envPath, winslash = "/"),
    expr_parsed,
    result_file,
    result_file
  ), con = temp_script)

  # Run the script in a separate R process
  callr::rscript(temp_script, stdout = "", stderr = "")

  if (!file.exists(result_file)) {
    stop("Execution failed: result file not created.")
  }

  result <- readRDS(result_file)

  if (!result$success) {
    return(paste0("Error in remote execution: ", result$error))
  }

  return(result$result)
}




#' Benchmark expression in multiple environments
#'
#' Measures the execution time for an R expression across multiple environments.
#' The function runs the expression multiple times
#' (controlled by `rep` parameter) and returns the execution times for each run
#' in the specified environments.
#'
#' @param expr An R expression or a `character()` (deparsed
#'  R expression) to be benchmarked.
#' @param envName A `character()` vector specifying the environment(s) name(s).
#'                If more than one environment is specified, the expression will
#'                be evaluated in each one.
#' @param rep An `integer(1)` specifying the number of repetitions for
#'  each expression. Default is 3.
#' @param warmup An `integer(1)` specifying how many initial repetitions to run
#' and discard (to remove warm-up bias). Default is `0`.
#' The total number of executions will be `warmup + rep`.
#' @param setup An optional R expression or a `character()` (deparsed
#'  R expression) to be evaluated before the benchmarked expression.
#'  For instance, it can be used to load libraries or retrieve data.
#' @param returnDataframe A `logical(1)` indicating whether to return
#' the results as a data frame (`TRUE`) or as a list (`FALSE`).
#'
#' @return
#'
#' With `returnDataframe == TRUE` (default):
#' A data frame with columns `envName`, `rep`, and `time`
#'
#' With `returnDataframe == FALSE`:
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
#'
#' @examples
#' envCreate("my_env", packages = c("jsonlite"))
#' benchInEnv(Sys.sleep(1), "my_env",
#'     rep = 3,
#'     setup = library(jsonlite),
#'     returnDataframe = TRUE
#' )
#'
#' @importFrom callr r
#' @importFrom renv load
#' @export
benchInEnv <- function(
        expr,
        envName = envList(),
        rep = 3,
        warmup = 0,
        setup = NULL,
        returnDataframe = TRUE
        ) {

    stopifnot(is.numeric(warmup), length(warmup) == 1, warmup >= 0)

    total_reps <- warmup + rep
    results <- list()

    expr_sub <- substitute(expr)
    if (is.character(expr_sub) && length(expr_sub) == 1) {
        expr_chr <- expr_sub
    } else if (is.symbol(expr_sub)) {
        val <- eval(expr_sub, parent.frame())
        if (!is.character(val)) stop("Symbol must evaluate to a character string.")
        expr_chr <- val
    } else {
        expr_chr <- deparse(expr_sub)
    }

    setup_sub <- substitute(setup)
    if (is.character(setup_sub) && length(setup_sub) == 1) {
        setup_chr <- setup_sub
    } else if (is.symbol(setup_sub)) {
        val <- eval(setup_sub, parent.frame())
        if (!is.character(val)) stop("Symbol must evaluate to a character string.")
        setup_chr <- val
    } else {
        setup_chr <- deparse(setup_sub)
    }

    for (env in envName) {
        cat("\nBenchmarking expression in environment:", env, "\n")
        result <- tryCatch(
            {
                times <- .benchInSingleEnv(
                    expr_chr = expr_chr,
                    envName = env,
                    rep = total_reps,
                    setup_chr = setup_chr
                )
                times[(warmup + 1):total_reps]
            },
            error = function(e) {
                message("Error in environment ", env, ": ", conditionMessage(e))
                NULL
            }
        )
        results[[env]] <- result
    }

    if (returnDataframe) {
        return(.namedListToDf(results))
    }
    return(results)
}

#' Run expression in a specified environment and benchmark its execution
#'
#' Executes an R expression within a specific environment,
#' benchmarks its execution time,
#' and returns the elapsed user time for each repetition.
#'
#' @param expr_chr A `character()`, an deparsed R expression that will
#' be benchmarked.
#' @param envName A `character(1)` specifying the environment name.
#'                The environment should be present in the ".envs/" directory.
#' @param rep An `integer(1)` specifying the number of repetitions
#' for the expression. Default is 3.
#'
#' @param setup_chr A `character()`, an deparsed R expression that will
#' be evaluated before `expr_chr`.
#'
#' @return A numeric vector of user times (in seconds) for each repetition of
#' the expression.
#'
#' @importFrom callr r
#' @importFrom renv load
#' @noRd
.benchInSingleEnv <- function(expr_chr, envName, rep, setup_chr) {
    envPath <- file.path(".envs", envName)
    if (!dir.exists(envPath)) {
        stop("Environment does not exist: ", envName)
    }
    result <- callr::r(
        function(envPath, expr_chr, rep, setup_chr) {
            if (!requireNamespace("renv", quietly = TRUE)) {
                stop("The 'renv' package is required. Install it using install.packages('renv').")
            }
            setwd(envPath)
            renv::load(project = ".")
            eval(parse(text = setup_chr))
            vapply(
                1:rep,
                function(i) {
                    start <- proc.time()
                    eval(parse(text = expr_chr))
                    end <- proc.time()
                    res <- end - start
                    res[[3]]
                },
                numeric(1)
            )
        },
        args = list(envPath = envPath, expr_chr = expr_chr, rep = rep, setup_chr = setup_chr),
        stdout = "", stderr = ""
    )
    result
}
