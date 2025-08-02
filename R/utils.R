.lockFileStorageFromEnv <- function(envName) {
    file.path(
        "exportedLockFiles",
        paste0(envName, "_lockFile.lock")
    )
}

.createDESCRIPTIONFile <- function(descriptionFile, pkgNames) {
    descriptionContent <- c(
        "Description: Do not modify, this file is used internally by VerR",
        "Imports:"
    )
    if (!is.null(pkgNames)) {
        imports <- mapply(function(pkg, isLast) {
            if (!isLast) {
                paste0(pkg, ",")
            } else {
                pkg
            }
        }, pkgNames, seq_along(pkgNames) == length(pkgNames))
    } else {
        imports <- character(0)
    }

    descriptionContent <- c(descriptionContent, imports)
    writeLines(descriptionContent, descriptionFile)
}

# Update the existing package list with new packages
combineDependencies <- function(oldPkg, newPkg) {
    if (!is.character(oldPkg) || !is.character(newPkg)) {
        stop("Both 'oldPkg' and 'newPkg' must be character vectors.")
    }
    updatedDeps <- unique(c(oldPkg, newPkg))
    sort(updatedDeps)
}

# Remove packages from the existing list based on newPkg
removeDependencies <- function(oldPkg, newPkg) {
    if (!is.character(oldPkg) || !is.character(newPkg)) {
        stop("Both 'oldPkg' and 'newPkg' must be character vectors.")
    }
    remainingDeps <- setdiff(oldPkg, newPkg)
    sort(remainingDeps)
}

#' Get Installed Packages for an Environment
#'
#' This function retrieves the list of installed packages, including their version and source, for a specified environment.
#'
#' @param envName A `character(1)` string specifying the name of the environment.
#'
#' @return A `data.frame` with columns `Package`, `Version`, and `Source`, or `NULL` if no packages are found.
#' @importFrom jsonlite fromJSON
#' @noRd
.getInstalledPackages <- function(envName) {
    envPath <- file.path(".envs", envName)
    envLockfilePath <- file.path(envPath, "renv.lock")

    if (!dir.exists(envPath)) {
        stop("Environment does not exist: ", envName)
    }

    if (file.exists(envLockfilePath)) {
        lockfileData <- jsonlite::fromJSON(envLockfilePath)
        pkgList <- lapply(names(lockfileData$Packages), function(pkg) {
            pkgInfo <- lockfileData$Packages[[pkg]]
            source <- pkgInfo$Source %||% "Unknown"
            list(Package = pkg, Version = pkgInfo$Version, Source = source)
        })
        return(do.call(rbind, lapply(pkgList, as.data.frame)))
    } else {
        return(data.frame(
            Package = character(0),
            Version = character(0),
            Source = character(0)
        ))
    }
}

.getFileTree <- function(path) {
    entries <- list.files(path, full.names = TRUE, recursive = FALSE)
    names(entries) <- basename(entries)
    lapply(entries, function(entry) {
        if (dir.exists(entry)) {
            .getFileTree(entry)
        } else {
            ""
        }
    })
}

.namedListToDf <- function(namedList) {
    do.call(rbind, lapply(names(namedList), function(envName) {
        data.frame(
            envName = envName,
            rep = seq_along(namedList[[envName]]),
            time = namedList[[envName]],
            stringsAsFactors = FALSE
        )
    }))
}

envExists <- function(envName) {
    dir.exists(file.path(".envs", envName))
}
