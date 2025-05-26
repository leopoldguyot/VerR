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

get_file_tree <- function(path) {
    entries <- list.files(path, full.names = TRUE, recursive = FALSE)
    names(entries) <- basename(entries)
    lapply(entries, function(entry) {
        if (dir.exists(entry)) {
            get_file_tree(entry)
        } else {
            ""
        }
    })
}


to_expr_chr <- function(expr) {
    expr_sub <- substitute(expr)

    # Case 1: If it's a character literal (e.g. "mean(1:5)"), return it as-is
    if (is.character(expr_sub) && length(expr_sub) == 1) {
        return(expr_sub)
    }

    # Case 2: If it's a symbol (like a variable that contains a string), evaluate it
    if (is.symbol(expr_sub)) {
        val <- eval(expr_sub, parent.frame())
        if (!is.character(val)) stop("Symbol must evaluate to a character string.")
        return(val)
    }

    # Otherwise, treat it as an expression and deparse it
    return(deparse(expr_sub))
}
