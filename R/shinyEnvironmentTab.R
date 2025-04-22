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
            textInput(NS(id, "env_name"),
             "New Environment Name",
              placeholder = "newEnv"),
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
        observeEvent(input$add_env, {
            envName <- input$env_name
            envCreate(envName, quiet = TRUE)
        })
        listEnv <- reactive({
            input$refresh_envs
            input$add_env
            envList()
        })

        observeEvent(listEnv(), {
            envs <- listEnv()
            output$environment_boxes <- renderUI({
                lapply(envs, function(envName) {
                    .createEnvironmentBoxUI(NS(id, envName), envName)
                })
            })
            lapply(envs, function(envName) {
                .createEnvironmentBoxServer(envName, envName)
            })
        })
    })
}
#' Create the UI for an Environment Box in the VerR Shiny Application
#'
#' This function defines the UI for a box that manages an environment in the VerR Shiny application.
#' The box includes sections for adding packages, viewing installed packages, adding files, and viewing the file tree.
#'
#' @param id A `character(1)` string representing the namespace ID for the box.
#' @param envName A `character(1)` string specifying the name of the environment.
#'
#' @return A Shiny `box` object containing the UI elements for managing the environment.
#' @importFrom shiny NS textInput actionButton fileInput uiOutput column
#' @importFrom shinydashboard box
#' @importFrom DT dataTableOutput
#' @noRd
.createEnvironmentBoxUI <- function(id, envName) {
    box(
        title = paste("Environment:", envName),
        status = "primary",
        width = 12,
        solidHeader = TRUE,
        collapsible = TRUE,
        # Fixed grid layout
        fluidRow(
            # Add Package and Installed Packages side by side
            column(
                width = 6,
                box(
                    width = 12,
                    title = "Add Package",
                    status = "info",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    textInput(NS(id, "addPkgInput"), "Package Name"),
                    actionButton(NS(id, "addPkgBtn"), "Add Package", width = "100%")
                )
            ),
            column(
                width = 6,
                box(
                    width = 12,
                    title = "Installed Packages",
                    status = "info",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    DT::dataTableOutput(NS(id, "pkgList"))
                )
            )
        ),
        fluidRow(
            # Add File and File Tree side by side
            column(
                width = 6,
                box(
                    width = 12,
                    title = "Add File",
                    status = "info",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    fileInput(NS(id, "add_file_input"), "Choose File"),
                    actionButton(NS(id, "add_file_btn"),
                        "Add File",
                        width = "100%"
                    )
                )
            ),
            column(
                width = 6,
                box(
                    width = 12,
                    title = "File Tree",
                    status = "info",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    uiOutput(NS(id, "file_tree"))
                )
            )
        )
    )
}

#' Create the Server Logic for an Environment Box in the VerR Shiny Application
#'
#' This function defines the server logic for managing an environment in the VerR Shiny application.
#' It handles adding packages, displaying installed packages, adding files, and rendering the file tree.
#'
#' @param id A `character(1)` string representing the namespace ID for the box.
#' @param envName A `character(1)` string specifying the name of the environment.
#'
#' @return A Shiny module server function.
#' @importFrom shiny moduleServer observeEvent modalDialog showModal modalButton updateActionButton
#' @importFrom shinyjs disable enable
#' @importFrom DT renderDataTable
#' @noRd
.createEnvironmentBoxServer <- function(id, envName) {
    moduleServer(id, function(input, output, session) {
        # Flag to track if installation is in progress
        installing <- reactiveVal(FALSE)

        # Add Package
        observeEvent(input$addPkgBtn, {
            pkgName <- input$addPkgInput
            print("hello")
            if (pkgName != "" && !installing()) {
                installing(TRUE)
                updateActionButton("addPkgBtn", label = "Installing...", disabled = TRUE, session = session)
                
                # Show the loading modal
                showModal(modalDialog(
                    "Installing package...",
                    footer = NULL,
                    easyClose = FALSE
                ))

                success <- TRUE
                tryCatch({
                    envInstallPackage(pkgName, envName = envName, quiet = FALSE)
                }, error = function(e) {
                    success <<- FALSE
                    showModal(modalDialog(
                        title = "Error",
                        paste("Failed to install:", e$message),
                        easyClose = TRUE,
                        footer = modalButton("OK")
                    ))
                })

                # If successful, remove the loading modal
                if (success) {
                    removeModal()
                }

                # Re-enable the button and reset the flag
                updateActionButton("addPkgBtn", "Add Package", disabled = FALSE, session = session)
                installing(FALSE)
            }
        })

        # Installed Packages
        output$pkgList <- DT::renderDataTable(
            {
                installedPkgs <- .getInstalledPackages(envName)
                installedPkgs
            },
            options = list(
                pageLength = 5,
                autoWidth = TRUE,
                dom = "t"
            ),
            rownames = FALSE
        )
    })
}


# Need to fix the fact that the installation is linked to refresh !!!!