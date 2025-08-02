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
                placeholder = "Your environment name"
            ),
            actionButton(NS(id, "add_env"), "Add Environment",
                class = "btn-add-custom",
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
        triggers <- reactiveValues(pkg = 0, file = 0, global = 0)
        initializedEnvs <- reactiveVal(character(0))

        observeEvent(input$add_env, {
            envCreate(input$env_name, quiet = TRUE)
            triggers$global <- triggers$global + 1
        })

        observeEvent(input$refresh_envs, {
            triggers$global <- triggers$global + 1
        })

        listEnv <- reactive({
            triggers$global
            envList()
        })

        observeEvent(listEnv(), {
            envs <- listEnv()
            output$environment_boxes <- renderUI({
                lapply(envs, function(envName) {
                    .createEnvironmentBoxUI(NS(id, envName), envName)
                })
            })

            newEnvs <- setdiff(envs, initializedEnvs())
            for (envName in newEnvs) {
                .createEnvironmentBoxServer(
                    id = envName,
                    refreshCallback = function() triggers$global <- triggers$global + 1,
                    globalTriggers = triggers
                )
            }
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
        fluidRow(
            column(width = 6, packageManagerUI(NS(id, "pkg"))),
            column(width = 6, fileManagerUI(NS(id, "file")))
        ),
        fluidRow(
            column(
                width = 12,
                actionButton(NS(id, "deleteEnvBtn"), "Delete Environment",
                    class = "btn-delete-custom", width = "100%"
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
#' @importFrom waiter waiter_show waiter_hide spin_fading_circles
#' @importFrom DT renderDataTable
#' @noRd
.createEnvironmentBoxServer <- function(id, refreshCallback, globalTriggers) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns
        envName <- id

        # Mount submodules
        packageManagerServer("pkg", envName, globalTriggers)
        fileManagerServer("file", envName, globalTriggers)

        # Delete Environment
        observeEvent(input$deleteEnvBtn, {
            showModal(
                modalDialog(
                    title = paste("Delete Environment:", envName),
                    "Are you sure? This cannot be undone.",
                    footer = tagList(
                        modalButton("Cancel"),
                        actionButton(ns("confirmDeleteEnv"), "Yes, Delete", class = "btn-danger")
                    )
                )
            )
        })

        observeEvent(input$confirmDeleteEnv, {
            removeModal()
            withSpinner(
                paste("Deleting environment:", envName),
                {
                    tryCatch(
                        {
                            envDelete(envName, force = TRUE)
                            notifySuccess(paste("\u2705 Deleted", envName))
                            refreshCallback()
                        },
                        error = function(e) {
                            notifyError(paste("\u274c Error deleting environment:", e$message))
                        }
                    )
                }
            )
        })
    })
}

#############################################
########## Package Manager Module ###########
#############################################

#' Create UI for the Package Manager Section
#'
#' This function creates the UI elements for the package management section
#' within an environment box in the VerR Shiny application.
#'
#' @param id A `character(1)` string representing the namespace ID for this UI module.
#'
#' @return A Shiny `box` UI element for managing R packages.
#' @importFrom shiny NS textInput actionButton
#' @importFrom htmltools strong div tags
#' @importFrom shinydashboard box
#' @importFrom DT dataTableOutput
#' @noRd
packageManagerUI <- function(id) {
    ns <- NS(id)
    box(
        title = "Package Manager",
        status = "info",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = 12,
        div(
            textInput(ns("addPkgInput"), "Package Name"),
            actionButton(ns("addPkgBtn"), "Add Package", class = "btn-add-custom", width = "100%")
        ),
        tags$hr(),
        tags$h5(strong("Installed Packages:")),
        DT::dataTableOutput(ns("pkgList"))
    )
}

#' Server Logic for the Package Manager Section
#'
#' This function defines the server-side logic for the package manager module,
#' including installing packages and displaying the installed package list.
#'
#' @param id A `character(1)` string representing the namespace ID for the module.
#' @param envName A `character(1)` string specifying the name of the environment.
#' @param triggers A `reactiveValues` object containing reactive triggers for package and global updates.
#'
#' @return A Shiny module server function.
#' @importFrom shiny moduleServer observeEvent req
#' @importFrom DT renderDataTable
#' @noRd
packageManagerServer <- function(id, envName, triggers) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        observeEvent(input$addPkgBtn, {
            pkgName <- input$addPkgInput
            withSpinner(
                paste0("Installing package ", pkgName, " in ", envName, "..."),
                {
                    tryCatch(
                        {
                            envInstallPackage(pkgName, envName = envName, quiet = FALSE)
                            triggers$pkg <- triggers$pkg + 1
                            notifySuccess("\u2705 Package installed!")
                        },
                        error = function(e) {
                            notifyError("\u274c Error installing package.")
                        }
                    )
                }
            )
        })

        output$pkgList <- DT::renderDataTable({
            req(envExists(envName))
            triggers$pkg + triggers$global # reactive dependency
            DT::datatable(
                .getInstalledPackages(envName),
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
    })
}

##########################################
########## File Manager Module ###########
##########################################

#' Create UI for the File Manager Section
#'
#' This function creates the UI elements for file upload and display
#' within an environment box in the VerR Shiny application.
#'
#' @param id A `character(1)` string representing the namespace ID for this UI module.
#'
#' @return A Shiny `box` UI element for file management.
#' @importFrom shiny NS textInput fileInput actionButton
#' @importFrom htmltools strong div tags
#' @importFrom shinydashboard box
#' @importFrom shinyTree shinyTree
#' @noRd
fileManagerUI <- function(id) {
    ns <- NS(id)
    box(
        title = "File Manager",
        status = "info",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = 12,
        div(
            textInput(ns("targetSubdir"), "Target Subdirectory", placeholder = "e.g., data/raw"),
            fileInput(ns("addFileInput"), "Choose File"),
            actionButton(ns("addFileBtn"), "Add File", class = "btn-add-custom", width = "100%")
        ),
        tags$hr(),
        tags$h5(strong("File Tree:")),
        shinyTree::shinyTree(ns("treeDisplay"), search = TRUE, theme = "proton")
    )
}

#' Server Logic for the File Manager Section
#'
#' This function defines the server-side logic for the file manager module,
#' including handling file uploads and rendering the file tree.
#'
#' @param id A `character(1)` string representing the namespace ID for the module.
#' @param envName A `character(1)` string specifying the name of the environment.
#' @param triggers A `reactiveValues` object containing reactive triggers for file and global updates.
#'
#' @return A Shiny module server function.
#' @importFrom shiny moduleServer observeEvent req
#' @importFrom shinyTree renderTree
#' @noRd
fileManagerServer <- function(id, envName, triggers) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        observeEvent(input$addFileBtn, {
            req(input$addFileInput)
            subdir <- gsub("^/|/$", "", input$targetSubdir)
            destPath <- buildEnvPath(envName, subdir)
            fullDest <- file.path(destPath, input$addFileInput$name)

            withSpinner(
                paste0("Uploading file to ", fullDest),
                {
                    tryCatch(
                        {
                            if (!dir.exists(destPath)) dir.create(destPath, recursive = TRUE)
                            file.copy(input$addFileInput$datapath, fullDest)
                            notifySuccess(paste("✅ Uploaded:", file.path(subdir, input$addFileInput$name)))
                            triggers$file <- triggers$file + 1
                        },
                        error = function(e) {
                            notifyError(paste("❌ Upload failed:", e$message))
                        }
                    )
                }
            )
        })

        output$treeDisplay <- shinyTree::renderTree({
            req(envExists(envName))
            triggers$file + triggers$global
            .getFileTree(buildEnvPath(envName))
        })
    })
}
