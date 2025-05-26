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
                value = "newEnv"
            ),
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
        # track environments we've initialized
        initializedEnvs <- reactiveVal(character(0))

        observeEvent(input$add_env, {
            envName <- input$env_name
            envCreate(envName, quiet = TRUE)
        })

        listEnv <- reactive({
            input$refresh_envs
            input$add_env
            envList()
        })

        # render UI for current list of environments
        observeEvent(listEnv(), {
            envs <- listEnv()

            output$environment_boxes <- renderUI({
                lapply(envs, function(envName) {
                    .createEnvironmentBoxUI(NS(id, envName), envName)
                })
            })

            # Only register servers for environments not yet initialized
            newEnvs <- setdiff(envs, initializedEnvs())
            for (envName in newEnvs) {
                .createEnvironmentBoxServer(id = envName)
            }

            # update the initialized list
            initializedEnvs(union(initializedEnvs(), newEnvs))
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
                    textInput(
                        NS(id, "addPkgInput"),
                        "Package Name"
                    ),
                    actionButton(NS(id, "addPkgBtn"),
                        "Add Package",
                        width = "100%"
                    )
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
                    textInput(NS(id, "targetSubdir"), "Target Subdirectory",
                        placeholder = "e.g., data/raw"
                    ),
                    fileInput(NS(id, "addFileInput"), "Choose File"),
                    actionButton(NS(id, "addFileBtn"),
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
                    shinyTree::shinyTree(NS(id, "treeDisplay"),
                        search = TRUE,
                        theme = "proton"
                    )
                )
            )
        )
    )
}

#' Create the Server Logic for an Environment Box in the VerR Shiny Application
#'
#' This function defines the server logic for managing an environment
#' in the VerR Shiny application.
#' It handles adding packages, displaying installed packages, adding files,
#'  and rendering the file tree.
#'
#' @param id A `character(1)` string representing the namespace ID for the box.
#' @param envName A `character(1)` string specifying the name of
#' the environment.
#'
#' @return A Shiny module server function.
#' @importFrom shiny moduleServer observeEvent modalDialog reactive
#' @importFrom shiny showModal modalButton updateActionButton
#' @importFrom shiny req showNotification
#' @importFrom shinyjs disable enable
#' @importFrom shinybusy show_modal_spinner remove_modal_spinner
#' @importFrom DT renderDataTable
#' @noRd
.createEnvironmentBoxServer <- function(id) {
    moduleServer(id, function(input, output, session) {
        pkgReloadTrigger <- reactiveVal(0)
        fileReloadTrigger <- reactiveVal(0)
        # Add Package
        observeEvent(input$addPkgBtn, {
            pkgName <- input$addPkgInput

            shinybusy::show_modal_spinner(
                text = paste("Installing", pkgName, "in", id, "...")
            )

            tryCatch({
                envInstallPackage(pkgName, envName = id, quiet = FALSE)
                pkgReloadTrigger(pkgReloadTrigger() + 1)
                showNotification("\u2705 Package installed!",
                    type = "message"
                )
            }, error = function(e) {
                showNotification(
                    paste("\u274c Error: more information in the console"),
                    type = "error"
                )
            }, finally = {
                shinybusy::remove_modal_spinner()
            })
        })
        table <- reactive({
            pkgReloadTrigger()
            .getInstalledPackages(id)
        })
        output$pkgList <- DT::renderDataTable({
            req(table())
            DT::datatable(table(),
                extensions = "FixedColumns",
                options = list(
                    searching = FALSE,
                    scrollX = TRUE,
                    fixedColumns = TRUE,
                    pageLength = 5,
                    lengthMenu = c(5, 10, 15)
                )
            )
        })

        observeEvent(input$addFileBtn, {
            req(input$addFileInput)

            uploaded_file <- input$addFileInput

            subdir <- input$targetSubdir

            subdir <- gsub("^/|/$", "", subdir) # remove leading/trailing slashes
            dest_dir <- file.path(".envs", id, subdir)
            dest_path <- file.path(dest_dir, uploaded_file$name)

            tryCatch(
                {
                    # Create subdir if it doesn't exist
                    if (!dir.exists(dest_dir)) {
                        dir.create(dest_dir,
                            recursive = TRUE
                        )
                    }

                    file.copy(uploaded_file$datapath, dest_path)
                    showNotification(
                        paste(
                            "\u2705 File uploaded to",
                            file.path(subdir, uploaded_file$name)
                        ),
                        type = "message"
                    )

                    fileReloadTrigger(fileReloadTrigger() + 1)
                },
                error = function(e) {
                    showNotification(paste("\u274c Upload failed:", e$message),
                        type = "error"
                    )
                }
            )
        })

        envFiles <- reactive({
            fileReloadTrigger()
            pkgReloadTrigger()
            get_file_tree(file.path(".envs", id))
        })
        output$treeDisplay <- shinyTree::renderTree({
            envFiles()
        })
    })
}
