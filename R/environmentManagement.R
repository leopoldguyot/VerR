#' Create a new environment
#'
#' Create an environment directory and installs the
#' specified packages.
#'
#' @param envName A `character(1)` string specifying the name of
#' the environment. Default is "new_env".
#' @param packages A `character()` vector of package names to install
#' in the environment. Default is NULL.
#'
#' To specify the packages you want to install within the environment,
#' you can use different syntax:
#'
#' \itemize{
#'   \item \code{pkg}: install the latest version of the package from CRAN.
#'   \item \code{pkg@version}: install a specific version of the package
#' from CRAN.
#'   \item \code{username/repo}: install the package from GitHub.
#'      You can also specify the commit with \code{username/repo@commitId}.
#'   \item \code{bioc::pkg}: install the package from Bioconductor.
#' }
#'
#' Note that to install a specific version of a Bioconductor package, it is
#' recommended to install it via GitHub.
#'
#' You can find more information on package installation in the
#' \code{\link[renv:install]{renv::install}} documentation.
#'
#' @param lockFile A `character()` string specifying the path to a lockFile.
#' Default is NULL.
#'
#' @param quiet A `logical()` indicating whether messages should be suppressed.
#'  Default is `FALSE`.
#' @details
#' Only one of the `packages` and `lockfile` parameters should be
#' present in order to create the environment.
#'
#' When a new environment is initialized, a new directory is
#' created inside the `.envs/` directory. This new directory will contain
#' all the installed packages specific to this environment.
#' @importFrom jsonlite fromJSON
#' @export
envCreate <- function(envName = "new_env", packages = NULL, lockFile = NULL, quiet = FALSE) {
    envPath <- file.path(".envs", envName)
    if (!dir.exists(envPath)) {
        dir.create(envPath, recursive = TRUE)
    }
    if (!is.null(packages) && !is.null(lockFile)) {
        stop("Specify either 'packages' or 'lockFile', not both.")
    } else if (!is.null(packages)) {
        pkgInfo <- .createEnvFromPackagesList(envPath, packages, quiet)
        pkgNames <- sapply(pkgInfo, `[[`, "Package")
    } else if (!is.null(lockFile)) {
        .createEnvFromLockFile(envPath, lockFile, quiet)
        lockFileData <- jsonlite::fromJSON(lockFile)
        pkgNames <- names(lockFileData$Packages)
    } else {
        stop("Either 'packages' or 'lockFile' must be provided.")
    }
    .createDESCRIPTIONFile(file.path(envPath, "DESCRIPTION"), pkgNames)
    lockFileUpdate(envName, quiet)
}

#' Create an environment from a package list
#'
#' @param envPath A `character(1)` string specifying the path of the environment.
#' @param packages A `character()` vector of package names to install.
#' @param quiet A `logical()` indicating whether messages should be suppressed.
#' Default is `FALSE`.
#' @return A named `list()` of installed package names with their metadata.
#'  This list includes the "Package" and "Version" elements.
#'
#' @importFrom callr r
#' @importFrom renv init load install snapshot
#' @noRd
.createEnvFromPackagesList <- function(envPath, packages, quiet = FALSE) {
    envPath <- normalizePath(envPath, mustWork = FALSE)
    callr::r(
        function(envPath, packages, quiet) {
            if (!requireNamespace("renv", quietly = TRUE)) {
                stop("The 'renv' package is required. Install it using install.packages('renv').")
            }
            setwd(envPath)
            renv::init(project = ".", bare = TRUE)
            pkgNames <- renv::install(packages, project = ".")
            if (!quiet) message("Installed packages in environment: ", envPath)
            pkgNames
        },
        args = list(envPath, packages, quiet),
        stdout = if (quiet) NULL else "",
        stderr = if (quiet) NULL else ""
    )
}

#' Create an environment from a lockfile
#'
#' @param envPath A `character(1)` string specifying the path of the environment.
#' @param lockfile A `character(1)` string specifying the path to a lockfile.
#' @param quiet A `logical()` indicating whether messages should be suppressed.
#' Default is `FALSE`.
#'
#' @importFrom callr r
#' @importFrom renv init restore
#' @noRd
.createEnvFromLockFile <- function(envPath, lockfile, quiet = FALSE) {
    envPath <- normalizePath(envPath, mustWork = FALSE)
    if (!file.exists(lockfile)) {
        stop("Lockfile does not exist: ", lockfile)
    }
    file.copy(lockfile, file.path(envPath, "renv.lock"), overwrite = TRUE)
    callr::r(
        function(envPath, lockfile, quiet) {
            if (!requireNamespace("renv", quietly = TRUE)) {
                stop("The 'renv' package is required. Install it using install.packages('renv').")
            }
            setwd(envPath)
            renv::init(project = envPath, bare = TRUE)
            renv::restore(project = envPath, prompt = FALSE)
            if (!quiet) message("Restored environment from lockfile: ", lockfile)
        },
        args = list(envPath, lockfile, quiet),
        stdout = if (quiet) NULL else "",
        stderr = if (quiet) NULL else ""
    )
}


#' Delete environments
#'
#' Delete environment(s) directory.
#'
#' @param envName A `character()` string specifying the name(s) of
#' the environment(s) to delete. Default is all environments.
#' @param force A `logical(1)` specifying whether to skip the confirmation
#'
#' @export
envDelete <- function(envName = envList(), force = FALSE) {
    if (is.null(envName)) {
        envName <- envList()
    }
    if (length(envName) == 0) {
        message("No environments found.")
        return()
    }
    numEnvs <- length(envName)
    if (!force) {
        confirm <- readline(prompt = paste0(
            "Are you sure you want to delete ",
            numEnvs,
            " environment(s)? (yes/no): "
        ))
        if (tolower(confirm) != "yes") {
            message("Operation canceled.")
            return()
        }
    }
    for (env in envName) {
        envPath <- file.path(".envs", env)
        if (!dir.exists(envPath)) {
            warning("Environment directory does not exist: ", envPath)
        }
        unlink(envPath, recursive = TRUE, force = TRUE)
        message("Cleared environment: ", env)
    }
    if (length(envList()) == 0 && dir.exists(".envs")) {
        unlink(".envs", recursive = TRUE, force = TRUE)
        message("Removed '.envs' directory as no environments are left.")
    }
}


#' List existing environments
#'
#' Return a character vector containing the names of all
#' existing environments.
#'
#' @return A `character()` vector of environment names.
#' @export
envList <- function() {
    list.dirs(".envs", recursive = FALSE, full.names = FALSE)
}


#' Display environment(s) information
#'
#' Display environment status, packages installed, and file tree
#'  in the specified environment(s).
#'
#' @param envName A `character()` vector specifying the environment(s) name(s).
#' Default is all environments.
#' @param statusInfo A `logical(1)` specifying whether to display
#'  status information.
#' @param pkgInfo A `logical(1)` specifying whether to display
#'  package information.
#' @param fileInfo A `logical(1)` specifying whether to display
#'  file information.
#'
#' @importFrom jsonlite fromJSON
#' @export
envInfo <- function(envName = envList(),
    statusInfo = TRUE,
    pkgInfo = FALSE,
    fileInfo = FALSE) {
    if (length(envName) == 0) {
        stop("No environments found.")
    }
    for (env in envName) {
        envPath <- file.path(".envs", env)
        if (!dir.exists(envPath)) {
            warning("Environment directory does not exist: ", envPath)
            next
        }
        .displayEnvInfo(env, envPath, statusInfo, pkgInfo, fileInfo)
    }
}


#' Display information for a single environment
#'
#' @param env A `character(1)` string specifying the environment name.
#' @param envPath A `character(1)` string specifying the path to the environment.
#' @param statusInfo A `logical(1)` specifying whether to display status information.
#' @param pkgInfo A `logical(1)` specifying whether to display package information.
#' @noRd
.displayEnvInfo <- function(env, envPath, statusInfo, pkgInfo, fileInfo) {
    message("Environment Name: ", env)
    message("Path: ", envPath)
    if (statusInfo) {
        .displayEnvStatus(envPath)
    }
    if (pkgInfo) {
        .displayPackageDetails(envPath)
    }
    if (fileInfo) {
        .displayEnvFileTree(envPath)
    }
}

#' Display the status of an environment
#'
#' @param envPath A `character(1)` string specifying the path to the environment.
#' @noRd
.displayEnvStatus <- function(envPath) {
    envSize <- sum(file.info(
        list.files(envPath, recursive = TRUE, full.names = TRUE)
    )$size) / (1024^2) # Size in MB
    message("Size: ", round(envSize, 2), " MB")
    envLockfilePath <- file.path(envPath, "renv.lock")
    if (file.exists(envLockfilePath)) {
        message("Lockfile: Present")
    } else {
        message("Lockfile: Not Present")
    }
}

#' Display package details for an environment
#'
#' @param envPath A `character(1)` string specifying the path to the environment.
#' @noRd
.displayPackageDetails <- function(envPath) {
    envLockfilePath <- file.path(envPath, "renv.lock")
    if (file.exists(envLockfilePath)) {
        lockfileData <- jsonlite::fromJSON(envLockfilePath)
        message("Packages Installed: ", length(lockfileData$Packages))
        message("Packages:")
        for (pkg in names(lockfileData$Packages)) {
            pkgInfo <- lockfileData$Packages[[pkg]]
            source <- pkgInfo$Source %||% "Unknown"
            message(sprintf("  - %s (Version: %s, Source: %s)", pkg, pkgInfo$Version, source))
        }
    } else {
        message("No packages found (lockfile missing).")
    }
}

#' Display the file tree of an environment using the `fs` package
#'
#' @param envPath A `character(1)` string specifying the path to
#'  the environment.
#' @importFrom fs dir_tree
#' @noRd
.displayEnvFileTree <- function(envPath) {
    if (!dir.exists(envPath)) {
        stop("The specified environment path does not exist.")
    }
    # Define exclusion pattern (regex to match unwanted files/directories)
    exclusion_pattern <- ".*(DESCRIPTION|renv|Rprofile|renv\\.lock).*"

    message("File Tree:\n")
    fs::dir_tree(envPath,
        recurse = TRUE, type = "any",
        regexp = exclusion_pattern, invert = TRUE, all = TRUE
    )
}


#' Copy files to environments
#'
#' Copy a file or directory into multiple environments.
#'
#' @param sourcePath A `character(1)` string specifying the path of the
#' file or directory to copy.
#' @param envName A `character()` vector specifying the environment(s) name(s)
#' where the file should be copied. Default is all environments.
#' @param targetPath A `character(1)` string specifying the relative path
#' within each environment where the file should be copied. Default is root of
#' the environment.
#' @param quiet A `logical()` indicating whether messages should be suppressed.
#' Default is `FALSE`.
#' @export
envCopyTo <- function(
        sourcePath,
        envName = envList(),
        targetPath = "",
        quiet = FALSE) {
    if (!file.exists(sourcePath)) {
        stop("Source file or directory does not exist: ", sourcePath)
    }
    if (length(envName) == 0) {
        stop("No environments found.")
    }
    for (env in envName) {
        envTargetPath <- file.path(".envs", env, targetPath)
        dir.create(dirname(envTargetPath),
            recursive = TRUE,
            showWarnings = FALSE
        )
        if (file.info(sourcePath)$isdir) {
            file.copy(sourcePath, envTargetPath,
                recursive = TRUE,
                overwrite = TRUE
            )
        } else {
            file.copy(sourcePath, envTargetPath, overwrite = TRUE)
        }
        if (!quiet) message("Copied ", sourcePath, " to ", envTargetPath)
    }
}

#' Remove files from environments
#'
#' Remove a file or directory from multiple environments.
#'
#' @param targetPath A `character(1)` string specifying the relative path
#' within each environment where the file should be removed.
#' @param envName A `character()` vector specifying the environment(s) name(s)
#' where the file should be removed. Default is all environments.
#' @param quiet A `logical()` indicating whether messages should be suppressed.
#' Default is `FALSE`.
#' @export
envRemoveFrom <- function(targetPath, envName = envList(), quiet = FALSE) {
    if (length(envName) == 0) {
        stop("No environments found.")
    }
    for (env in envName) {
        envTargetPath <- file.path(".envs", env, targetPath)
        if (file.exists(envTargetPath)) {
            unlink(envTargetPath, recursive = TRUE, force = TRUE)
            if (!quiet) message("Removed ", envTargetPath)
        } else {
            if (!quiet) {
                message(
                    "Path does not exist in ",
                    env, ": ",
                    envTargetPath
                )
            }
        }
    }
}


#' Install new package(s) to environment(s)
#'
#' @param envName A `character()` string specifying the name(s) of the
#'  environment(s) for which to update the lockFiles.
#'  Default is all environments.
#' @param package A `character()` vector of package name(s) to install
#' in the environment.
#'
#' To specify the packages you want to install within the environment,
#' you can use different syntax:
#'
#' \itemize{
#'   \item \code{pkg}: install the latest version of the package from CRAN.
#'   \item \code{pkg@version}: install a specific version of the package
#' from CRAN.
#'   \item \code{username/repo}: install the package from GitHub.
#'      You can also specify the commit with \code{username/repo@commitId}.
#'   \item \code{bioc::pkg}: install the package from Bioconductor.
#' }
#'
#' Note that to install a specific version of a Bioconductor package, it is
#' recommended to install it via GitHub.
#'
#' You can find more information on package installation in the
#' \code{\link[renv:install]{renv::install}} documentation.
#'
#' @param quiet A `logical()` indicating whether messages should be suppressed.
#' Default is `FALSE`.
#' @importFrom renv install snapshot
#' @importFrom callr r
#'
#' @export
envInstallPackage <- function(package, envName = envList(), quiet = FALSE) {
    if (length(envName) == 0) {
        stop("No environment names provided.")
    }
    envPaths <- file.path(".envs", envName)
    for (envPath in envPaths) {
        pkgInfo <- callr::r(
            function(envPath, package, quiet) {
                if (!requireNamespace("renv", quietly = TRUE)) {
                    stop("The 'renv' package is required. Install it using install.packages('renv').")
                }
                setwd(envPath)
                renv::load()
                newPkg <- renv::install(package)
                oldPkg <- renv::dependencies("DESCRIPTION", quiet = TRUE)
                list(
                    "old" = oldPkg[, "Package"],
                    "new" = names(newPkg)
                )
            },
            args = list(envPath, package, quiet),
            stdout = if (quiet) NULL else "",
            stderr = if (quiet) NULL else ""
        )
        updatedPkgList <- combineDependencies(
            pkgInfo[["old"]],
            pkgInfo[["new"]]
        )
        .createDESCRIPTIONFile(
            file.path(envPath, "DESCRIPTION"),
            updatedPkgList
        )
    }
    for (name in envName) {
        lockFileUpdate(name, quiet = quiet)
    }
}
