library(testthat)
if (dir.exists(".envs")) {
    unlink(".envs/", recursive = TRUE, force = TRUE)
}
if (dir.exists("exportedLockFiles")) {
    unlink("exportedLockFiles/", recursive = TRUE, force = TRUE)
}
test_that("createEnv creates a new environment", {
    envName <- "test_env"
    packages <- c("digest")
    createEnv(envName = envName, packages = packages)
    expect_true(dir.exists(file.path(".envs", envName)))
    deleteEnv(envName = envName, force = TRUE)
})

test_that("createEnv throws an error when both packages and lockfile are provided", {
    envName <- "test_env"
    packages <- c("digest")
    lockfile <- tempfile(fileext = ".lock")
    writeLines("{}", lockfile)

    expect_error(createEnv(
        envName = envName,
        packages = packages,
        lockfile = lockfile
    ))

    unlink(lockfile)
})

test_that("deleteEnv removes an environment", {
    envName <- "test_env"
    createEnv(envName = envName, packages = c("digest"))
    expect_true(dir.exists(file.path(".envs", envName)))

    deleteEnv(envName = envName, force = TRUE)

    expect_false(dir.exists(file.path(".envs", envName)))
})

test_that("listEnvs lists all environments", {
    envName1 <- "test_env1"
    envName2 <- "test_env2"
    createEnv(envName = envName1, packages = c("digest"))
    createEnv(envName = envName2, packages = c("digest"))

    envs <- listEnvs()
    expect_equal(envs, c(envName1, envName2))
    deleteEnv(envName = c(envName1, envName2), force = TRUE)
})

test_that("copyToEnv copies a file to environments", {
    envName <- "test_env"
    createEnv(envName = envName, packages = c("digest"))

    tempFile <- tempfile()
    writeLines("test content", tempFile)

    copyToEnv(sourcePath = tempFile, envName = envName, targetPath = "test_file.txt")

    expect_true(file.exists(file.path(".envs", envName, "test_file.txt")))

    deleteEnv(envName = envName, force = TRUE)
    unlink(tempFile)
})

test_that("removeFromEnv removes a file from environments", {
    envName <- "test_env"
    createEnv(envName = envName, packages = c("digest"))

    tempFile <- tempfile()
    writeLines("test content", tempFile)
    copyToEnv(sourcePath = tempFile, envName = envName, targetPath = "test_file.txt")

    expect_true(file.exists(file.path(".envs", envName, "test_file.txt")))

    removeFromEnv(targetPath = "test_file.txt", envName = envName)

    expect_false(file.exists(file.path(".envs", envName, "test_file.txt")))

    deleteEnv(envName = envName, force = TRUE)
    unlink(tempFile)
})

# need to be created outside test_that, since test_that
# cause issues with renv::install (isolated r session)
createEnv(envName = "digestOld", packages = c("digest@0.6.18"))
createEnv(envName = "digestLatest", packages = c("digest"))

test_that("correct package version - from package vector", {
    version <- runInEnv(packageVersion("digest"),
        envName = c("digestOld", "digestLatest"))

    expect_equal(as.character(version[["digestOld"]]), "0.6.18")
    expect_equal(as.character(version[["digestLatest"]]), "0.6.37")
})

exportLockfile()
deleteEnv(force = TRUE)
createEnv(envName = "digestOldLock",
          lockfile = VerR:::.lockFileStorageFromEnv("digestOld"))
createEnv(envName = "digestLatestLock",
          lockfile = VerR:::.lockFileStorageFromEnv("digestLatest"))

test_that("correct package version - from lockfile", {
    version <- runInEnv(packageVersion("digest"),
                        envName = c("digestOldLock", "digestLatestLock"))
    expect_equal(as.character(version[["digestOldLock"]]), "0.6.18")
    expect_equal(as.character(version[["digestLatestLock"]]), "0.6.37")

    deleteEnv(force = TRUE)
})
