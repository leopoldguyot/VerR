#' Launch the VerR Shiny Application
#'
#' This function launches the VerR Shiny application,
#'  which provides a graphical user interface for managing
#'  environments and running expressions.
#'
#' @return A Shiny application object.
#' @examples
#' if (interactive()) {
#'     VerRapp()
#' }
#'
#' @importFrom shiny shinyApp
#' @export
VerRapp <- function() {
    options(shiny.maxRequestSize = 100 * 1024^2)
    ui <- .buildUI()
    server <- .buildServer()
    shinyApp(ui = ui, server = server)
}

#' Build the Shiny Server for VerR
#'
#' This function defines the server logic for the VerR Shiny application.
#'
#' @return A Shiny server function.
#'
#' @noRd
.buildServer <- function() {
    server <- function(input, output, session) {
        .createEnvironmentTabServer("env_tab")
        .createJobTabServer("job_tab")
        .createBenchTabServer("bench_tab")
    }
    server
}

#' Build the Shiny UI for VerR
#'
#' This function defines the user interface for the VerR Shiny application.
#'
#' @return A Shiny UI object.
#'
#' @importFrom shinydashboard dashboardPage dashboardBody tabItems tabItem
#' @importFrom htmltools includeCSS
#' @importFrom shinyjs useShinyjs
#' @noRd
.buildUI <- function() {
    ui <- dashboardPage(
        skin = "blue",
        header = .createHeader(),
        sidebar = .createSidebar(),
        body = dashboardBody(
            waiter::use_waiter(),
            includeCSS(system.file(package = "VerR", "www", "style.css")),
            tabItems(
                tabItem(
                    tabName = "env_tab",
                    .createEnvironmentTabUI("env_tab")
                ),
                tabItem(
                    tabName = "job_tab",
                    .createJobTabUI("job_tab")
                ),
                tabItem(
                    tabName = "bench_tab",
                    .createBenchTabUI("bench_tab")
                )
            )
        ),
        title = "VerR"
    )
    ui
}

#' Create the Header for the VerR Shiny Application
#'
#' This function defines the header for the VerR Shiny application.
#'
#' @return A Shiny `dashboardHeader` object.
#'
#' @importFrom shinydashboard dashboardHeader
#' @noRd
.createHeader <- function() {
    dashboardHeader(
        title = "VerR"
    )
}

#' Create the Sidebar for the VerR Shiny Application
#'
#' This function defines the sidebar for the VerR Shiny application.
#'
#' @return A Shiny `dashboardSidebar` object.
#'
#' @importFrom shinydashboard dashboardSidebar sidebarMenu menuItem
#' @importFrom shiny icon
#' @noRd
.createSidebar <- function() {
    dashboardSidebar(
        sidebarMenu(
            id = "sidebar_menu",
            menuItem(
                "Environments Setup",
                tabName = "env_tab",
                icon = icon("gear")
            ),
            menuItem(
                "Job Manager",
                tabName = "job_tab",
                icon = icon("tasks")
            ),
            menuItem(
                "Benchmarking Tool",
                tabName = "bench_tab",
                icon = icon("hourglass-half")
            )
        )
    )
}
