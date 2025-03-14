#' Environment Management Functions
#'
#' These functions allow users to create, clear, and list environments
#'  stored in the ".envs" directory.
#'
#' @name environment_management
#' @rdname environment_management
#' @param envName A `character(1)` string specifying the name of the
#'  environment.
#' @param packages A `character()` vector of package names to install in the
#'  environment. Default is NULL.
#' @param lockfile A `character()` string specifying the path to a lockfile.
#'  Default is NULL.
#' @details Description of functions:
#' \itemize{
#'      \item `createEnv()`: Creates an environment directory and
#'       installs the specified packages or uses the lockfile.
#'      \item `clearEnv()`: Deletes a specific environment directory.
#'      \item `clearEnvs()`: Deletes all environment directories.
#'      \item `listEnvs()`: Returns a character vector containing
#'       the names of all existing environments.
#' }
#' @examples
#' createEnv("my_first_env", packages = c("digest@0.6.18"))
#' createEnv("my_second_env", packages = c("digest@0.6.17"))
#' listEnvs()
#' clearEnv("my_first_env")
#' listEnvs()
#' clearEnvs()
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

#' @rdname environment_management
#' @export
clearEnv <- function(envName) {
    envPath <- file.path(".envs", envName)
    if (!dir.exists(envPath)) {
        stop("Environment directory does not exist: ", envPath)
    }
    unlink(envPath, recursive = TRUE, force = TRUE)
    message("Cleared environment: ", envName)
}

#' @rdname environment_management
#' @export
clearEnvs <- function() {
    unlink(".envs", recursive = TRUE, force = TRUE)
    message("All environments cleared")
}

#' @rdname environment_management
#' @return `listEnvs()`: A `character()` vector that contains
#'  the environments' names
#' @export
listEnvs <- function() {
    envs <- list.dirs(".envs", recursive = FALSE, full.names = FALSE)
    return(envs)
}

#' @rdname environment_management
#' @param sourcePath A `character(1)` string specifying the path of the file
#'  or directory to copy.
#' @param targetPath A `character(1)` string specifying the relative path
#' within each environment where the file or directory should be copied.
#' @param envs A `character()` that specify the environments' name in which
#' the file/directory should be copied. Default is `listEnvs()` to copy the
#' file/directory to all the environments.
#' @export
copyToEnvs <- function(sourcePath, envs = listEnvs(), targetPath = "") {
    if (!file.exists(sourcePath)) {
        stop("Source file or directory does not exist: ", sourcePath)
    }
    if (length(envs) == 0) {
        stop("No environments found.")
    }
    for (env in envs) {
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

#' @rdname environment_management
#' @param targetPath A `character(1)` string specifying the relative path within
#'  each environment where the file or directory should be removed.
#' @param envs A `character()` that specify the environments' name in which
#' the file/directory should be removed. Default is `listEnvs()` to remove the
#' file/directory in all the environments.
#' @export
removeFromEnvs <- function(targetPath, envs = listEnvs()) {
    envs <- listEnvs()
    if (length(envs) == 0) {
        stop("No environments found.")
    }
    for (env in envs) {
        envTargetPath <- file.path(".envs", env, targetPath)
        if (file.exists(envTargetPath)) {
            unlink(envTargetPath, recursive = TRUE, force = TRUE)
            message("Removed ", envTargetPath)
        } else {
            message("Path does not exist in ", env, ": ", envTargetPath)
        }
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
        renv::init(project = envPath, bare = TRUE)
        renv::load(project = envPath, restart = FALSE)
        renv::install("rlang")
        for (pkg in packages) {
            renv::install(pkg, project = envPath, lock = TRUE)
        }
        renv::snapshot(project = envPath, prompt = FALSE)
        message("Environment created from package list in: ", envPath)
    }, args = list(envPath, packages), stdout = "", stderr = "")
}

#' @importFrom callr r
#' @importFrom renv init restore
.createEnvFromLockFile <- function(envPath, lockfile) {
    envPath <- normalizePath(envPath, mustWork = FALSE)
    callr::r(function(envPath, lockfile) {
        if (!requireNamespace("renv", quietly = TRUE)) {
            stop("The 'renv' package is required. Install it using install.packages('renv').")
        }
        if (!file.exists(lockfile)) {
            stop("Lockfile does not exist: ", lockfile)
        }
        renv::init(project = envPath, bare = TRUE)
        file.copy(lockfile, file.path(envPath, "renv.lock"), overwrite = TRUE)
        renv::restore(project = envPath, prompt = FALSE)
        message("Environment loaded from lockfile in: ", envPath)
    }, args = list(envPath, lockfile), stdout = "", stderr = "")
}

