.lockFileStorageFromEnv <- function(envName) {
    return(file.path("exportedLockFiles", paste0(envName, "_lockFile.lock")))
}
