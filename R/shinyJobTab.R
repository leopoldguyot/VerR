#' Create the UI for the Job Tab in the VerR Shiny Application
#'
#' This function defines the UI for the "Job Manager" tab in the
#'  VerR Shiny application.
#'
#' @param id A `character(1)` string representing the namespace ID for the tab.
#'
#' @return A Shiny `fluidRow` object containing the UI elements for
#' the "Job Manager" tab.
#' @importFrom shiny NS fluidRow uiOutput actionButton verbatimTextOutput
#' @importFrom shinydashboard box
#' @importFrom shinyAce aceEditor
#' @importFrom htmltools br
#' @noRd
.createJobTabUI <- function(id) {
    fluidRow(
        id = NS(id, "job_tab"),
        box(
            title = "Job Manager",
            status = "primary",
            width = 12,
            solidHeader = FALSE,
            collapsible = FALSE,
            shinyAce::aceEditor(
                outputId = NS(id, "expr"),
                mode = "r",
                theme = "textmate",
                height = "200px",
                fontSize = 14,
                placeholder = "Write here the R code that you want to evaluate ..."
            ),
            actionButton(NS(id, "runBtn"), "Run in All Environments", width = "100%"),
            br(), br(),
            verbatimTextOutput(NS(id, "resultOutput"))
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
#'  observe reactiveVal reactive
#' @importFrom shinydashboard box
#' @noRd
.createJobTabServer <- function(id) {
    moduleServer(id, function(input, output, session) {
        resultText <- reactiveVal("")

        observeEvent(input$runBtn, {
            req(input$expr)

            # Parse the code string into an R expression *now*
            parsed_expr <- tryCatch(parse(text = input$expr)[[1]], error = function(e) e)

            if (inherits(parsed_expr, "error")) {
                resultText(paste("\u274c Error parsing expression:\n", parsed_expr$message))
                return()
            }

            # Use quote to prevent evaluation in current env
            quoted_expr <- substitute(parsed_expr)
            print(eval(parsed_expr))

            results <- tryCatch(
                {
                    runInEnv(
                        expr = substitute(parsed_expr),
                        envName = envList(),
                        parallel = FALSE,
                        ncores = parallel::detectCores() - 1
                    )
                },
                error = function(e) {
                    paste("\u274c Error during execution:\n", e$message)
                }
            )

            if (is.character(results)) {
                resultText(results)
            } else {
                result_summary <- capture.output(print(results))
                resultText(paste(result_summary, collapse = "\n"))
            }
        })



        output$resultOutput <- renderText({
            resultText()
        })
    })
}
