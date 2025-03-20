.lockFileStorageFromEnv <- function(envName) {
    file.path(
        "exportedLockFiles",
        paste0(envName, "_lockFile.lock")
    )
}

.createDESCRIPTIONFile <- function(descriptionFile, pkgNames) {
    if (length(pkgNames) == 0) {
        stop("The 'pkgNames' vector must not be empty.")
    }
    descriptionContent <- c(
        "Description: Do not modify, this file is used internally by VerR",
        "Imports:"
    )
    imports <- mapply(function(pkg, isLast) {
        if (!isLast) {
            paste0(pkg, ",")
        } else {
            pkg
        }
    }, pkgNames, seq_along(pkgNames) == length(pkgNames))
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
