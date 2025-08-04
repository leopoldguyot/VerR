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

#' @importFrom htmltools tagList tags
#' @importFrom shiny icon
#' @keywords internal
addPkgTooltip <- function() {
    tagList(
        "Package Name ",
        tags$span(
            icon("question-circle"),
            title = "Install formats:\n
- pkg: CRAN latest (e.g., dplyr)\n
- pkg@version: CRAN version (e.g., ggplot2@3.4.0)\n
- user/repo: GitHub (e.g., rstudio/shiny)\n
- user/repo@branch: GitHub branch (e.g., rstudio/shiny@branchName)\n
- user/repo@commit: GitHub commit (e.g., rstudio/shiny@commitId)\n
- bioc::pkg: Bioconductor (e.g., bioc::edgeR)\n
Tip: Use GitHub to install specific Bioconductor versions.",
            style = "cursor: help; color: #3c8dbc;"
        )
    )
}

#' Generate labeled tooltips for fields
#'
#' @param label A `character(1)` main label string.
#' @param tooltip A `character(1)` string with the tooltip description.
#'
#' @return A `tagList` containing the label and tooltip icon.
#' @importFrom htmltools tagList tags
#' @importFrom shiny icon
#' @keywords internal
tooltipMaker <- function(label, tooltip) {
    tagList(
        label,
        tags$span(
            icon("question-circle"),
            title = tooltip,
            style = "cursor: help; color: #3c8dbc; margin-left: 5px;"
        )
    )
}
