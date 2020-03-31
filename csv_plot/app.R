
library(shiny)
options(shiny.maxRequestSize = 50*1024^2)

import_fileUI <- function(id) {
    ns <- NS(id)
    verticalLayout(
        wellPanel(
            splitLayout(fileInput(ns("file"), NULL),
                        actionButton(ns("preview"), "Preview")
            ),
            uiOutput(ns("slider"))
        ),
        DT::dataTableOutput(ns("datatable"))
    )
}

import_file <- function(input, output, session) {
    ns <- session$ns

    datafile <- reactive({
        req(input$file)
        read.csv(input$file$datapath, stringsAsFactors = FALSE)
    })

    output$slider <- renderUI({
        sliderInput(ns("interval"), "Select dataset rows:",
                    min = 1, max = dim(datafile())[1],
                    value = c(1, dim(datafile())[1])
        )
    })

    observeEvent(input$preview, {
        output$datatable <- DT::renderDataTable(isolate(DT::datatable(
            datafile()[input$interval[1] : input$interval[2],], filter = "top"
        )))
    })

    return(datafile)
}


select_tsUI <- function(id) {
    ns <- NS(id)
    sidebarLayout(
        sidebarPanel(
            verticalLayout(
                uiOutput(ns("select_values")),
                radioButtons(ns("use_date_column"), "Time axis:",
                             choices = c("Row indexes" = "indexes",
                                         "Start date and forward" = "start",
                                         "Date column" = "column")
                             ),
                uiOutput(ns("select_dates")),
                actionButton(ns("update_ts"), "Show timeseries")
            )
        ),
        mainPanel(
            DT::dataTableOutput(ns("ts_table"))
        )
    )
}

select_ts <- function(input, output, session, datafile) {
    ns <- session$ns

    output$select_values <- renderUI({
        selectInput(ns("values_column"), "Select values column:",
                    choices = names(Filter(is.numeric, datafile()))
        )
    })

    output$select_dates <- renderUI({
        switch(input$use_date_column,
               "column" = {
                   verticalLayout(
                       selectInput(ns("date_column"), "Select tisme column:",
                                   choices = names(Filter(is.character, datafile()))
                       ),
                       textInput(ns("date_format"), "Date and time format:")
                   )
               },
               "start" = dateInput(ns("start_date"), "Select start date:")
        )
    })

    row_indexes <- reactive({
        which(!is.na(datafile()[input$values_column])) - 1
    })

    ts_data <- reactive({
        data.frame(t = switch(input$use_date_column,
                              "indexes" = row_indexes(),
                              "start" = as.POSIXct(input$start_date) + as.difftime(15, units = "mins") * row_indexes(),
                              "column" = as.POSIXct(datafile()[row_indexes() + 1, input$date_column], "UTC",
                                                    format = input$date_format
                              )
        ),
        y = datafile()[row_indexes() + 1, input$values_column]
        )
    })

    observeEvent(input$update_ts, {
        output$ts_table <- DT::renderDataTable(isolate(DT::datatable(
            data.frame(t = format(ts_data()[, 1]), y = ts_data()[, 2]),
            rownames = FALSE
        )))
    })

    return(ts_data)
}

ui <- fluidPage(
    tabsetPanel(
        tabPanel("File selection", import_fileUI("import_file")),
        tabPanel("Specify timeseries", select_tsUI("select_ts")),
        tabPanel("Plot",
                 verticalLayout(plotly::plotlyOutput("plot_graph"),
                                wellPanel(
                                    uiOutput("plot_interval")
                                )
                 )
        )
    )
)

server <- function(input, output) {
    datafile <- callModule(import_file, "import_file")

    ts_data <- callModule(select_ts, "select_ts", datafile)

    output$plot_graph <- plotly::renderPlotly({
        plotly::plot_ly(ts_data()[input$plot_range[1] : input$plot_range[2],],
                        x = ~t, y = ~y, type = "scatter", mode = "line"
        )
    })

    output$plot_interval <- renderUI({
        sliderInput("plot_range", "Select rows:",
                    min = 1, max = dim(ts_data())[1], value = c(1, dim(ts_data())[1])
        )
    })
}


shinyApp(ui, server)
