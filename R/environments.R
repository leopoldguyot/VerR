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

clearEnvironment <- function(envName) {
    envPath <- paste0(".envs/", envName)
    if (!dir.exists(envPath)) {
        stop("Environment directory does not exist: ", envPath)
    }
    unlink(envPath, recursive = TRUE, force = TRUE)
    message("Cleared environment: ", envPath)
}

clearEnvironments <- function() {
    dir <- ".envs"
    if (!dir.exists(dir)) {
        stop("Directory does not exist: ", dir)
    }
    envs <- list.dirs(dir, recursive = FALSE, full.names = FALSE)
    if (length(envs) == 0){
        warning("No environments to clear")
    } else {
        for (env in envs) {
            clearEnvironment(env)
        }
        message("All environments cleared")
    }

}

listEnvironments <- function() {
    envs <- list.dirs(".envs", recursive = FALSE, full.names = FALSE)
    return(envs)
}
