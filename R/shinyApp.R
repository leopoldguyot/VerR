#' Launch the VerR Shiny Application
#'
#' This function launches the VerR Shiny application,
#'  which provides a graphical user interface for managing
#'  environments and running expressions.
#'
#' @return A Shiny application object.
#' @examples
#' if (interactive()) {
#'     guiVerR()
#' }
#'
#' @importFrom shiny shinyApp
#' @export
guiVerR <- function() {
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
            useShinyjs(),
            tabItems(
                tabItem(
                    tabName = "env_tab",
                    .createEnvironmentTabUI("env_tab")
                )
            ),
            includeCSS(system.file(package = "VerR", "www", "style.css"))
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
#' @noRd
.createSidebar <- function() {
    dashboardSidebar(
        sidebarMenu(
            id = "sidebar_menu",
            menuItem(
                "Environments",
                tabName = "env_tab"
            )
        )
    )
}
