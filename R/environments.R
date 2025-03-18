#' Create a new environment
#'
#' Create an environment directory and installs the
#' specified packages.
#'
#' @param envName A `character(1)` string specifying the name of
#' the environment.
#' @param packages A `character()` vector of package names to install
#' in the environment. Default is NULL.
#' @param lockfile A `character()` string specifying the path to a lockfile.
#' Default is NULL.
#' @details
#' Note that at least of the `packages` and `lockfile` should be present in
#' order to create the environment.
#'
#' @export
createEnv <- function(envName, packages = NULL, lockfile = NULL) {
    envPath <- file.path(".envs", envName)
    if (!dir.exists(envPath)) {
        dir.create(envPath, recursive = TRUE)
    }
    if (!is.null(packages) && !is.null(lockfile)) {
        stop("Specify either 'packages' or 'lockfile', not both.")
    } else if (!is.null(packages)) {
        .createEnvFromPackagesList(envPath, packages)
    } else if (!is.null(lockfile)) {
        .createEnvFromLockFile(envPath, lockfile)
    } else {
        stop("Either 'packages' or 'lockfile' must be provided.")
    }
}

#' Clear a specific environment
#'
#' Delete a specific environment directory.
#'
#' @param envName A `character(1)` string specifying the name of
#' the environment.
#' @export
clearEnv <- function(envName) {
    envPath <- file.path(".envs", envName)
    if (!dir.exists(envPath)) {
        stop("Environment directory does not exist: ", envPath)
    }
    unlink(envPath, recursive = TRUE, force = TRUE)
    message("Cleared environment: ", envName)
}

#' Clear all environments
#'
#' Delete all environment directories.
#'
#' @export
clearEnvs <- function() {
    unlink(".envs", recursive = TRUE, force = TRUE)
    message("All environments cleared")
}

#' List existing environments
#'
#' Return a character vector containing the names of all
#' existing environments.
#'
#' @return A `character()` vector of environment names.
#' @export
listEnvs <- function() {
    envs <- list.dirs(".envs", recursive = FALSE, full.names = FALSE)
    return(envs)
}

#' Copy files to environments
#'
#' Copy a file or directory into multiple environments.
#'
#' @param sourcePath A `character(1)` string specifying the path of the
#' file or directory to copy.
#' @param envNames A `character()` vector specifying the environments' names
#' where the file should be copied. Default is all environments.
#' @param targetPath A `character(1)` string specifying the relative path
#' within each environment where the file should be copied.
#' @export
copyToEnvs <- function(sourcePath, envNames = listEnvs(), targetPath = "") {
    if (!file.exists(sourcePath)) {
        stop("Source file or directory does not exist: ", sourcePath)
    }
    if (length(envNames) == 0) {
        stop("No environments found.")
    }
    for (env in envNames) {
        envTargetPath <- file.path(".envs", env, targetPath)
        dir.create(dirname(envTargetPath), recursive = TRUE, showWarnings = FALSE)
        if (file.info(sourcePath)$isdir) {
            file.copy(sourcePath, envTargetPath, recursive = TRUE, overwrite = TRUE)
        } else {
            file.copy(sourcePath, envTargetPath, overwrite = TRUE)
        }
        message("Copied ", sourcePath, " to ", envTargetPath)
    }
}

#' Remove files from environments
#'
#' Remove a file or directory from multiple environments.
#'
#' @param targetPath A `character(1)` string specifying the relative path
#' within each environment where the file should be removed.
#' @param envNames A `character()` vector specifying the environments' names
#' where the file should be removed. Default is all environments.
#' @export
removeFromEnvs <- function(targetPath, envNames = listEnvs()) {
    if (length(envNames) == 0) {
        stop("No environments found.")
    }
    for (env in envNames) {
        envTargetPath <- file.path(".envs", env, targetPath)
        if (file.exists(envTargetPath)) {
            unlink(envTargetPath, recursive = TRUE, force = TRUE)
            message("Removed ", envTargetPath)
        } else {
            message("Path does not exist in ", env, ": ", envTargetPath)
        }
    }
}

#' Export environment lockfile
#'
#' Export an environment's lockfile to a specified location.
#'
#' @param envName A `character()` string specifying the name(s) of the
#'  environment(s).
#' @param exportPath A `character(1)` string specifying the path where the
#' lockfile should be exported. Default is an auto-generated path.
#' @export
exportLockfile <- function(envNames = listEnvs(), exportPaths = NULL) {
    if (length(envNames) == 0) {
        stop("No environment names provided.")
    }
    # If exportPaths is not provided, generate default paths for each envName
    if (is.null(exportPaths)) {
        exportPaths <- sapply(envNames, .lockFileStorageFromEnv)
    }
    if (length(envNames) != length(exportPaths)) {
        stop("Number of environment names and export paths must match.")
    }
    for (i in seq_along(envNames)) {
        envName <- envNames[i]
        exportPath <- exportPaths[i]
        envLockfilePath <- file.path(".envs", envName, "renv.lock")
        if (!file.exists(envLockfilePath)) {
            warning("Lockfile does not exist in environment: ", envName)
            next
        }
        dir.create(dirname(exportPath), recursive = TRUE, showWarnings = FALSE)
        file.copy(envLockfilePath, exportPath, overwrite = TRUE)
        message("Exported lockfile from ", envName, " to ", exportPath)
    }
}

#' @importFrom callr r
#' @importFrom renv init load install snapshot
.createEnvFromPackagesList <- function(envPath, packages) {
    envPath <- normalizePath(envPath, mustWork = FALSE)
    callr::r(function(envPath, packages) {
        if (!requireNamespace("renv", quietly = TRUE)) {
            stop("The 'renv' package is required. Install it using install.packages('renv').")
        }
        setwd(envPath)
        renv::init(project = ".", bare = TRUE)
        for (pkg in packages) {
            renv::install(pkg, project = ".")
        }
        renv::snapshot(project = ".", type = "all", prompt = FALSE, force = TRUE)
        message("Environment created from package list in: ", envPath)
    }, args = list(envPath, packages), stdout = "", stderr = "")
}

#' @importFrom callr r
#' @importFrom renv init restore
.createEnvFromLockFile <- function(envPath, lockfile) {
    envPath <- normalizePath(envPath, mustWork = FALSE)
    if (!file.exists(lockfile)) {
        stop("Lockfile does not exist: ", lockfile)
    }
    file.copy(lockfile, file.path(envPath, "renv.lock"), overwrite = TRUE)
    callr::r(function(envPath, lockfile) {
        if (!requireNamespace("renv", quietly = TRUE)) {
            stop("The 'renv' package is required. Install it using install.packages('renv').")
        }
        setwd(envPath)
        renv::init(project = envPath, bare = TRUE)
        renv::restore(project = envPath, prompt = FALSE)
        message("Environment loaded from lockfile in: ", envPath)
    }, args = list(envPath, lockfile), stdout = "", stderr = "")
}
