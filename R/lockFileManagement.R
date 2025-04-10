#' Export environment lockfile
#'
#' Export an environment's lockfile to a specified location.
#'
#' @param envName A `character()` string specifying the name(s) of the
#'  environment(s). Default is all environments.
#' @param exportPath A `character()` string specifying the path(s) where the
#' lockfile(s) should be exported. Default is an auto-generated path.
#' @param quiet A `logical()` indicating whether messages should be suppressed.
#' Default is `FALSE`.
#' @details envName and exportPath must have the same length.
#' @export
lockFileExport <- function(envName = envList(), exportPath = NULL, quiet = FALSE) {
    if (length(envName) == 0) {
        stop("No environment names provided.")
    }
    # If exportPaths is not provided, generate default paths for each envName
    if (is.null(exportPath)) {
        exportPath <- sapply(envName, .lockFileStorageFromEnv)
    }
    if (length(envName) != length(exportPath)) {
        stop("Number of environment names and export paths must match.")
    }
    for (i in seq_along(envName)) {
        env <- envName[i]
        path <- exportPath[i]
        envLockfilePath <- file.path(".envs", env, "renv.lock")
        if (!file.exists(envLockfilePath)) {
            warning("Lockfile does not exist in environment: ", env)
            next
        }
        dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
        file.copy(envLockfilePath, path, overwrite = TRUE)
        if (!quiet) message("Exported lockfile from ", env, " to ", path)
    }
}


#' Update lockFiles of specified environments
#'
#' @param envName A `character()` string specifying the name(s) of the
#'  environment(s) for which to update the lockFiles.
#'  Default is all environments.
#' @param quiet A `logical()` indicating whether messages should be suppressed.
#' Default is `FALSE`.
#' @export
lockFileUpdate <- function(envName = envList(), quiet = FALSE) {
    if (length(envName) == 0) {
        stop("No environment names provided.")
    }
    envPaths <- file.path(".envs", envName)
    for (envPath in envPaths) {
        callr::r(
            function(envPath, quiet) {
                if (!requireNamespace("renv", quietly = TRUE)) {
                    stop("The 'renv' package is required. Install it using install.packages('renv').")
                }
                setwd(envPath)
                renv::load()
                renv::snapshot()
                if (!quiet) message("Updated lockfile in environment: ", envPath)
            },
            args = list(envPath, quiet),
            stdout = if (quiet) NULL else "",
            stderr = if (quiet) NULL else ""
        )
    }
}
