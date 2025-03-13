#' Environment Management Functions
#'
#' These functions allow users to create, clear, and list environments stored in the ".envs" directory.
#'
#' @name environment_management
#' @rdname environment_management
#' @param envName A `character(1)` string specifying the name of the environment.
#' @param packages A `character()` vector of package names to install in the environment. Default is NULL.
#' @param lockfile A `character()` string specifying the path to a lockfile. Default is NULL.
#' @details Description of functions:
#' \itemize{
#'      \item `createEnvironment()`: Creates an environment directory and
#'       installs the specified packages or uses the lockfile.
#'      \item `clearEnvironment()`: Deletes a specific environment directory.
#'      \item `clearEnvironments()`: Deletes all environment directories.
#'      \item `listEnvironments()`: Returns a character vector containing
#'       the names of all existing environments.
#' }
#' @examples
#' createEnvironment("my_first_env", packages = c("digest@0.6.18"))
#' createEnvironment("my_second_env", packages = c("digest@0.6.17"))
#' listEnvironments()
#' clearEnvironment("my_first_env")
#' listEnvironments()
#' clearEnvironments()
#'
#' @export
createEnvironment <- function(envName, packages = NULL, lockfile = NULL) {
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
clearEnvironment <- function(envName) {
    envPath <- file.path(".envs", envName)
    if (!dir.exists(envPath)) {
        stop("Environment directory does not exist: ", envPath)
    }
    unlink(envPath, recursive = TRUE, force = TRUE)
    message("Cleared environment: ", envName)
}

#' @rdname environment_management
#' @export
clearEnvironments <- function() {
    unlink(".envs", recursive = TRUE, force = TRUE)
    message("All environments cleared")
}

#' @rdname environment_management
#' @return `listEnvironments()`: A `character()` vector that contains
#'  the environments' names
#' @export
listEnvironments <- function() {
    envs <- list.dirs(".envs", recursive = FALSE, full.names = FALSE)
    return(envs)
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
    }, args = list(envPath, packages))
}

#' @importFrom callr r
#' @importFrom renv init restore
.createEnvFromLockFile <- function(envPath, lockfile) {
    if (!requireNamespace("renv", quietly = TRUE)) {
        stop("The 'renv' package is required. Install it using install.packages('renv').")
    }
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
    }, args = list(envPath, lockfile))
}
