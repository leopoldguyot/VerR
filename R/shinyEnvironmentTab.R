#' Create the UI for the Environment Tab in the VerR Shiny Application
#'
#' This function defines the UI for the "Environment" tab in the VerR Shiny application.
#'
#' @param id A `character(1)` string representing the namespace ID for the tab.
#'
#' @return A Shiny `fluidRow` object containing the UI elements for the "Environment" tab.
#' @importFrom shiny NS fluidRow uiOutput actionButton
#' @importFrom shinydashboard box
#' @noRd
.createEnvironmentTabUI <- function(id) {
    fluidRow(
        id = NS(id, "env_tab"),
        uiOutput(NS(id, "environment_boxes")),
        box(
            title = "Add Environment",
            status = "primary",
            width = 12,
            solidHeader = FALSE,
            collapsible = FALSE,
            actionButton(NS(id, "add_env"), "Add Environment",
                width = "100%"
            )
        ),
        actionButton(NS(id, "refresh_envs"), "Refresh Environments", width = "100%")
    )
}

#' Create the Server Logic for the Environment Tab in the VerR Shiny Application
#'
#' This function defines the server logic for the "Environment"
#'  tab in the VerR Shiny application.
#'
#' @param id A `character(1)` string representing the namespace ID for the tab.
#'
#' @return A Shiny module server function.
#' @importFrom shiny moduleServer renderUI tagList observeEvent 
#'  observe reactiveVal reactive
#' @importFrom shinydashboard box
#' @noRd
.createEnvironmentTabServer <- function(id) {
    moduleServer(id, function(input, output, session) {
        refresh_trigger <- reactiveVal(0)

        observeEvent(input$refresh_envs, {
            refresh_trigger(refresh_trigger() + 1)
        })
        listEnv <- reactive({
            refresh_trigger()
            envList()
        })

        observeEvent(listEnv(), {
            envs <- listEnv()
            output$environment_boxes <- renderUI({
                lapply(envs, function(envName) {
                    boxId <- gsub("[^a-zA-Z0-9]", "_", envName)
                    .createEnvironmentBoxUI(NS(id, boxId))
                })
            })
            lapply(envs, function(envName) {
                boxId <- gsub("[^a-zA-Z0-9]", "_", envName)
                .createEnvironmentBoxServer(boxId)
            })
        })
    })
}

.createEnvironmentBoxUI <- function(id) {
    uiOutput(NS(id, "environment_ui"))
}

.createEnvironmentBoxServer <- function(id) {
    moduleServer(id, function(input, output, session) {
        output$environment_ui <- renderUI({
            box(
                title = id,
                status = "primary",
                width = 12,
                solidHeader = FALSE,
                collapsible = FALSE,
                actionButton(NS(id, "remove_env"), "Remove Environment",
                    width = "100%"
                )
            )
        })
    })
}