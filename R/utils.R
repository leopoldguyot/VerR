.lockFileStorageFromEnv <- function(envName) {
    file.path(
        "exportedLockFiles",
        paste0(envName, "_lockFile.lock")
    )
}

.createDESCRIPTIONFile <- function(descriptionFile, pkgNames, pkgVers) {
    if (length(pkgNames) != length(pkgVers)) {
        stop("The lengths of 'pkgNames' and 'pkgVers' must be the same.")
    }
    descriptionContent <- c(
        "Description: Do not modify, this file is used internally by VerR",
        "Imports:"
    )
    imports <- mapply(function(pkg, version) {
        # If a version is provided, use it in the format (== version)
        if (!is.na(version) && version != "") {
            paste0(pkg, " (== ", version, ")")
        } else {
            pkg
        }
    }, pkgNames, pkgVers)
    descriptionContent <- c(descriptionContent, imports)
    writeLines(descriptionContent, descriptionFile)
}
