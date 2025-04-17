library(testthat)
if (dir.exists(".envs")) {
    unlink(".envs/", recursive = TRUE, force = TRUE)
}
if (dir.exists("exportedLockFiles")) {
    unlink("exportedLockFiles/", recursive = TRUE, force = TRUE)
}
test_that("envCreate creates a new environment", {
    envName <- "test_env"
    packages <- c("jsonlite")
    envCreate(envName = envName, packages = packages)
    expect_true(dir.exists(file.path(".envs", envName)))
    envDelete(envName = envName, force = TRUE)
})

test_that("envCreate throws an error when both packages and lockfile are provided", {
    envName <- "test_env"
    packages <- c("jsonlite")
    lockfile <- tempfile(fileext = ".lock")
    writeLines("{}", lockfile)

    expect_error(envCreate(
        envName = envName,
        packages = packages,
        lockfile = lockfile
    ))

    unlink(lockfile)
})

test_that("envDelete removes an environment", {
    envName <- "test_env"
    envCreate(envName = envName, packages = c("jsonlite"))
    expect_true(dir.exists(file.path(".envs", envName)))

    envDelete(envName = envName, force = TRUE)

    expect_false(dir.exists(file.path(".envs", envName)))
})

test_that("envList lists all environments", {
    envName1 <- "test_env1"
    envName2 <- "test_env2"
    envCreate(envName = envName1, packages = c("jsonlite"))
    envCreate(envName = envName2, packages = c("jsonlite"))

    envs <- envList()
    expect_equal(envs, c(envName1, envName2))
    envDelete(envName = c(envName1, envName2), force = TRUE)
})

test_that("envCopyTo copies a file to environments", {
    envName <- "test_env"
    envCreate(envName = envName, packages = c("jsonlite"))

    tempFile <- tempfile()
    writeLines("test content", tempFile)

    envCopyTo(sourcePath = tempFile, envName = envName, targetPath = "test_file.txt")

    expect_true(file.exists(file.path(".envs", envName, "test_file.txt")))

    envDelete(envName = envName, force = TRUE)
    unlink(tempFile)
})

test_that("envRemoveFrom removes a file from environments", {
    envName <- "test_env"
    envCreate(envName = envName, packages = c("jsonlite"))

    tempFile <- tempfile()
    writeLines("test content", tempFile)
    envCopyTo(sourcePath = tempFile, envName = envName, targetPath = "test_file.txt")

    expect_true(file.exists(file.path(".envs", envName, "test_file.txt")))

    envRemoveFrom(targetPath = "test_file.txt", envName = envName)

    expect_false(file.exists(file.path(".envs", envName, "test_file.txt")))

    envDelete(envName = envName, force = TRUE)
    unlink(tempFile)
})
