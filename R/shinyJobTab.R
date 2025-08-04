#' Create the UI for the Job Tab in the VerR Shiny Application
#'
#' This function defines the UI for the "Job Manager" tab in the
#'  VerR Shiny application.
#'
#' @param id A `character(1)` string representing the namespace ID for the tab.
#'
#' @return A Shiny `fluidRow` object containing the UI elements for
#' the "Job Manager" tab.
#' @importFrom shiny NS fluidRow uiOutput actionButton verbatimTextOutput downloadButton checkboxInput
#' @importFrom shinydashboard box
#' @importFrom shinyAce aceEditor
#' @importFrom htmltools br
#' @noRd

.createJobTabUI <- function(id) {
    ns <- NS(id)
    fluidRow(
        id = ns("job_tab"),
        box(
            title = "Job Manager",
            status = "primary",
            width = 12,
            solidHeader = FALSE,
            collapsible = FALSE,
            shinyAce::aceEditor(
                outputId = ns("expr_chr"),
                mode = "r",
                theme = "textmate",
                height = "200px",
                fontSize = 14,
                placeholder = "Write here the R code that you want to evaluate ..."
            ),
            checkboxInput(ns("parallel_flag"), "Run in Parallel", value = FALSE),
            actionButton(ns("runBtn"), "Run in All Environments", class = "btn-add-custom", width = "100%"),
            br(), br(),
            verbatimTextOutput(ns("resultOutput")),
            br(),
            downloadButton(ns("downloadResult"), "Download Result as .RDS", width = "100%")
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
#'  observe reactiveVal reactive downloadHandler validate need icon
#' @importFrom shinydashboard box
#' @importFrom shiny renderText
#' @importFrom htmltools HTML
#' @importFrom utils capture.output
#' @noRd
.createJobTabServer <- function(id) {
    moduleServer(id, function(input, output, session) {
        resultText <- reactiveVal("")
        resultData <- reactiveVal(NULL)

        observeEvent(input$runBtn, {
            req(input$expr_chr)
            expr_chr <- input$expr_chr
            use_parallel <- input$parallel_flag

            # Show loading spinner
            waiter::waiter_show(
                html = tagList(
                    waiter::spin_fading_circles(),
                    "Running job in environments..."
                ),
                color = "#333333cc"
            )

            results <- tryCatch(
                {
                    runInEnv(
                        expr = expr_chr,
                        envName = envList(),
                        parallel = use_parallel,
                        ncores = if (use_parallel) max(1, parallel::detectCores() - 1) else 1
                    )
                },
                error = function(e) {
                    HTML(paste(
                        icon("times-circle"),
                        "Error during execution:\n", e$message, "\n",
                        "More information can be found in the R console"
                    ))
                },
                finally = {
                    waiter::waiter_hide()
                }
            )

            if (is.character(results)) {
                resultText(results)
                resultData(NULL)
            } else {
                result_summary <- capture.output(print(results))
                resultText(paste(result_summary, collapse = "\n"))
                resultData(results)
            }
        })

        output$resultOutput <- renderText({
            resultText()
        })

        output$downloadResult <- downloadHandler(
            filename = function() {
                "VerR_job_results.rds"
            },
            content = function(file) {
                data <- resultData()
                validate(need(!is.null(data), "No result available to download."))
                saveRDS(data, file)
            }
        )
    })
}
