#' Create the UI for the Environment Tab in the VerR Shiny Application
#'
#' This function defines the UI for the "Environment" tab in the VerR Shiny application.
#'
#' @param id A `character(1)` string representing the namespace ID for the tab.
#'
#' @return A Shiny `fluidRow` object containing the UI elements for the "Environment" tab.
#' @importFrom shiny NS fluidRow uiOutput actionButton fileInput textInput icon
#' @importFrom shinydashboard box
#' @importFrom htmltools tagList
#' @noRd
.createEnvironmentTabUI <- function(id) {
    ns <- NS(id)

    fluidRow(
        id = ns("env_tab"),

        # Refresh button alone at the top
        column(
            width = 12,
            box(
                title = NULL,
                status = "primary",
                solidHeader = FALSE,
                collapsible = FALSE,
                width = 12,
                actionButton(ns("refresh_envs"), tagList(icon("sync"), "Refresh Environments"), width = "100%")
            )
        ),

        # Add Environment and Global Package/File Manager side by side
        column(
            width = 12,
            fluidRow(
                column(
                    width = 6,
                    box(
                        title = "Add Environment",
                        status = "primary",
                        solidHeader = FALSE,
                        collapsible = FALSE,
                        width = 12,
                        textInput(ns("env_name"),
                                  "New Environment Name",
                                  placeholder = "Your environment name"
                        ),
                        actionButton(ns("add_env"), "Add Environment",
                                     class = "btn-add-custom",
                                     width = "100%")
                    )
                ),
                column(
                    width = 6,
                    box(
                        title = "Global Package/File Manager",
                        status = "primary",
                        solidHeader = FALSE,
                        collapsible = FALSE,
                        width = 12,
                        textInput(ns("global_pkg_name"), "Package Name"),
                        actionButton(ns("install_global_pkg"), "Add Package to All Environments", class = "btn-add-custom", width = "100%"),
                        tags$hr(),
                        textInput(ns("global_file_subdir"), "Target Subdirectory", placeholder = "e.g., data/shared"),
                        fileInput(ns("global_file"), "Upload File"),
                        actionButton(ns("upload_global_file"), "Upload to All Environments", class = "btn-add-custom", width = "100%")
                    )
                )
            )
        ),

        # Environment Boxes below
        column(
            width = 12,
            box(
                title = "Environments",
                status = "primary",
                solidHeader = FALSE,
                collapsible = FALSE,
                width = 12,
            uiOutput(ns("environment_boxes"))
            )
        )
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
#'  observe reactiveVal reactive reactiveValues icon
#' @importFrom shinydashboard box
#' @importFrom htmltools HTML
#' @noRd
.createEnvironmentTabServer <- function(id) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns
        triggers <- reactiveValues(pkg = 0, file = 0, global = 0)
        initializedEnvs <- reactiveVal(character(0))

        observeEvent(input$add_env, {
            envName <- input$env_name
            withSpinner(
                paste0("Creating new environment: '", envName,"'..."),
                {tryCatch({
                    envCreate(envName, quiet = TRUE)
                    notifySuccess(HTML(paste0(icon("check-circle"), " Environment '", envName, "' created")))
                    triggers$global <- triggers$global + 1
                }, error = function(e) {
                    notifyError(HTML(paste(icon("times-circle"), "Error creating the environment:", e$message)))
                })}
            )
        })

        observeEvent(input$refresh_envs, {
            triggers$global <- triggers$global + 1
        })

        observeEvent(input$install_global_pkg, {
            pkgName <- input$global_pkg_name
            withSpinner(
                paste0("Installing package '", pkgName, "' in all environments..."),
                {tryCatch({
                    envs <- envList()
                    envInstallPackage(pkgName, envName = envs, quiet = TRUE)
                    notifySuccess(HTML(paste0(icon("check-circle"), " Package '", pkgName, "' installed in all environments")))
                    triggers$pkg <- triggers$pkg + 1
                }, error = function(e) {
                    notifyError(HTML(paste(icon("times-circle"), "Error installing package globally:", e$message)))
                })}
            )
        })

        observeEvent(input$upload_global_file, {
            req(input$global_file)
            withSpinner(
                "Uploading file in all environments...",
                {tryCatch({
                    envs <- envList()
                    subdir <- gsub("^/|/$", "", input$global_file_subdir)
                    for (env in envs) {
                        destPath <- buildEnvPath(env, subdir)
                        fullDest <- file.path(destPath, input$global_file$name)
                        if (!dir.exists(destPath)) dir.create(destPath, recursive = TRUE)
                        file.copy(input$global_file$datapath, fullDest)
                    }
                    notifySuccess(HTML(paste(icon("check-circle"), "File uploaded to all environments")))
                    triggers$file <- triggers$file + 1
                }, error = function(e) {
                    notifyError(HTML(paste(icon("times-circle"), "Upload failed:", e$message)))
                })}
            )
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
#' @importFrom shiny showModal modalButton updateActionButton removeModal
#' @importFrom shiny req showNotification
#' @importFrom shinyjs disable enable
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
                    title = paste0("Delete Environment '", envName, "'"),
                    "Are you sure? This cannot be undone.",
                    footer = tagList(
                        modalButton("Cancel"),
                        actionButton(ns("confirmDeleteEnv"), "Yes, Delete", class = "btn-delete-custom")
                    )
                )
            )
        })

        observeEvent(input$confirmDeleteEnv, {
            removeModal()
            withSpinner(
                paste0("Deleting environment: '", envName, "'..."),
                {tryCatch(
                    {
                        envDelete(envName, force = TRUE)
                        notifySuccess(HTML(paste0(icon("check-circle"), " Deleted environment: '", envName, "'")))
                        refreshCallback()
                    },
                    error = function(e) {
                        notifyError(HTML(paste(icon("times-circle"), "Error deleting environment:", e$message)))
                    })}
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
#' @importFrom shiny moduleServer observeEvent req icon
#' @importFrom htmltools HTML
#' @importFrom DT renderDataTable
#' @noRd
packageManagerServer <- function(id, envName, triggers) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        observeEvent(input$addPkgBtn, {
            pkgName <- input$addPkgInput
            withSpinner(
                paste0("Installing package '", pkgName, "' in '", envName, "'..."),
                {tryCatch(
                    {
                        envInstallPackage(pkgName, envName = envName, quiet = FALSE)
                        triggers$pkg <- triggers$pkg + 1
                        notifySuccess(HTML(paste0(icon("check-circle"), " Package '", pkgName, "' installed")))
                    },
                    error = function(e) {
                        notifyError(HTML(paste(icon("times-circle"), "Error installing package.")))
                    })}
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
#' @importFrom shiny moduleServer observeEvent req icon
#' @importFrom shinyTree renderTree
#' @importFrom htmltools HTML
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
                {tryCatch(
                    {
                        if (!dir.exists(destPath)) dir.create(destPath, recursive = TRUE)
                        file.copy(input$addFileInput$datapath, fullDest)
                        notifySuccess(HTML(paste(icon("check-circle"), "Uploaded:", file.path(subdir, input$addFileInput$name))))
                        triggers$file <- triggers$file + 1
                    },
                    error = function(e) {
                        notifyError(HTML(paste(icon("times-circle"), "Upload failed:", e$message)))
                    })}
            )
        })
        output$treeDisplay <- shinyTree::renderTree({
            req(envExists(envName))
            triggers$file + triggers$global
            .getFileTree(buildEnvPath(envName))
        })
    })
}
