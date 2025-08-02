# Constants
.envBasePath <- ".envs"

# Utility Functions
buildEnvPath <- function(envName, subdir = NULL) {
    if (is.null(subdir)) {
        return(file.path(.envBasePath, envName))
    }
    file.path(.envBasePath, envName, subdir)
}

notifySuccess <- function(msg) showNotification(msg, type = "message")
notifyError <- function(msg) showNotification(msg, type = "error")

withSpinner <- function(message, expr) {
    waiter::waiter_show(html = tagList(spin_fading_circles(), message), color = "#333333d3")
    on.exit(waiter::waiter_hide(), add = TRUE)
    force(expr)
}
