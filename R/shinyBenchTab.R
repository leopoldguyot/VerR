#' Create the UI for the Benchmark Tab in the VerR Shiny Application
#'
#' This function defines the UI for the "Benchmark" tab in the VerR Shiny application.
#'
#' @param id A `character(1)` string representing the namespace ID for the tab.
#'
#' @importFrom shiny NS fluidRow uiOutput actionButton verbatimTextOutput downloadButton numericInput
#' @importFrom shinydashboard box
#' @importFrom shinyAce aceEditor
#' @importFrom htmltools br
#' @importFrom DT dataTableOutput renderDataTable datatable
#' @importFrom plotly plotlyOutput renderPlotly plot_ly layout
#' @noRd
.createBenchTabUI <- function(id) {
    ns <- NS(id)
    fluidRow(
        id = ns("benchmark_tab"),
        box(
            title = "Benchmark Environments",
            status = "primary",
            width = 12,
            solidHeader = FALSE,
            collapsible = FALSE,
            tags$h5(strong("Setup Code:")),
            shinyAce::aceEditor(
                outputId = ns("setup_code"),
                mode = "r",
                theme = "textmate",
                height = "150px",
                fontSize = 14,
                placeholder = "Optional setup code (e.g., load packages, define data)..."
            ),
            br(),
            tags$h5(strong("Benchmarked Code:")),
            shinyAce::aceEditor(
                outputId = ns("benchmark_code"),
                mode = "r",
                theme = "textmate",
                height = "150px",
                fontSize = 14,
                placeholder = "Code to benchmark across environments..."
            ),
            numericInput(
                ns("replicates"),
                "Number of Replicates",
                value = 3,
                min = 1,
                step = 1
            ),
            numericInput(
                ns("warmup"),
                "Number of Warmup Turns",
                value = 0,
                min = 0,
                step = 1
            ),
            actionButton(ns("runBenchmark"), "Run Benchmark", class = "btn-add-custom", width = "100%"),
            br(), br(),
            uiOutput(ns("benchmarkTableBox")),
            uiOutput(ns("benchmarkPlotBox")),
            downloadButton(ns("downloadBenchmark"), "Download Benchmark as .CSV", width = "100%")
        )
    )
}


#' Create the Server Logic for the Benchmark Tab in the VerR Shiny Application
#'
#' This function defines the server logic for the "Benchmark" tab in the VerR Shiny application.
#'
#' @param id A `character(1)` string representing the namespace ID for the tab.
#'
#' @return A Shiny module server function.
#' @importFrom shiny moduleServer renderUI tagList observeEvent reactiveVal downloadHandler validate need renderText icon showNotification
#' @importFrom utils capture.output write.csv
#' @importFrom htmltools HTML
#' @importFrom waiter waiter_show waiter_hide spin_fading_circles
#' @importFrom DT renderDataTable datatable
#' @importFrom plotly renderPlotly plot_ly layout
#' @noRd
.createBenchTabServer <- function(id) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns
        resultData <- reactiveVal(NULL)

        observeEvent(input$runBenchmark, {
            req(input$benchmark_code)
            code_expr <- input$benchmark_code
            setup_expr <- input$setup_code
            replicates <- input$replicates
            warmup <- input$warmup

            withSpinner(
                "Running benchmark across environments...",
                {results <- tryCatch(
                    {
                        benchInEnv(
                            expr = code_expr,
                            envName = envList(),
                            rep = replicates,
                            warmup = warmup,
                            setup = setup_expr,
                            returnDataframe = TRUE
                        )
                    },
                    error = function(e) {
                        showNotification(paste("Error during benchmarking:", e$message), type = "error")
                        NULL
                    })
                }
            )

            if (!is.null(results) && is.data.frame(results)) {
                resultData(results)
            } else {
                resultData(NULL)
            }
        })

        output$benchmarkTable <- DT::renderDataTable({
            req(resultData())
            DT::datatable(resultData(), options = list(pageLength = 10, scrollX = TRUE))
        })

        output$benchmarkPlot <- plotly::renderPlotly({
            df <- resultData()
            req(df)
            p <- plot_ly(
                data = df,
                x = ~envName,
                y = ~time,
                type = "box",
                boxpoints = "all",
                jitter = 0.5,
                pointpos = 0
            )
            p <- layout(
                p,
                title = "Benchmark Timing Distribution by Environment",
                yaxis = list(title = "Time (seconds)"),
                xaxis = list(title = "Environment")
            )
            p
        })

        output$benchmarkTableBox <- renderUI({
            req(resultData())
            box(
                title = "Raw Results",
                status = "primary",
                width = 12,
                solidHeader = FALSE,
                collapsible = TRUE,
                collapsed = TRUE,
                DT::dataTableOutput(ns("benchmarkTable"))
            )
        })

        output$benchmarkPlotBox <- renderUI({
            req(resultData())
            box(
                title = "Plot",
                status = "primary",
                width = 12,
                solidHeader = FALSE,
                collapsible = TRUE,
                plotly::plotlyOutput(ns("benchmarkPlot"))
            )
        })

        output$downloadBenchmark <- downloadHandler(
            filename = function() {
                "VerR_benchmark_results.csv"
            },
            content = function(file) {
                data <- resultData()
                validate(need(!is.null(data), "No benchmark data to download."))
                write.csv(data, file, row.names = FALSE)
            }
        )
    })
}

