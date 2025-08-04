#' @importFrom shiny showNotification
#' @keywords internal
.envBasePath <- ".envs"

#' Build environment path
#'
#' @param envName Environment name (string)
#' @param subdir Optional subdirectory (string)
#'
#' @return A full file path (string)
#' @keywords internal
buildEnvPath <- function(envName, subdir = NULL) {
    if (is.null(subdir)) {
        return(file.path(.envBasePath, envName))
    }
    file.path(.envBasePath, envName, subdir)
}

#' Show success notification
#'
#' @param msg Message to display
#' @importFrom shiny showNotification
#' @keywords internal
notifySuccess <- function(msg) showNotification(msg, type = "message")

#' Show error notification
#'
#' @param msg Message to display
#' @importFrom shiny showNotification
#' @keywords internal
notifyError <- function(msg) showNotification(msg, type = "error")

#' Wrap an expression with a loading spinner
#'
#' @param message Spinner message (string)
#' @param expr Expression to evaluate
#'
#' @importFrom waiter waiter_show waiter_hide spin_fading_circles
#' @importFrom htmltools tagList
#' @keywords internal
withSpinner <- function(message, expr) {
    waiter::waiter_show(html = tagList(spin_fading_circles(), message), color = "#333333d3")
    on.exit(waiter::waiter_hide(), add = TRUE)
    force(expr)
}
