library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(DT)

ui <- dashboardPage(
    
    dashboardHeader(
        title = "Coursera Dashboard"), #dashboard header
    dashboardSidebar(width = 150,
                     
                     #sidebar menu
                     sidebarMenu(
                         menuItem("Upload File",
                                  tabName="upload",
                                  icon = icon("arrow-circle-up",
                                              lib = "font-awesome")
                         )
                     )
    ),
    
    dashboardBody(
        tabItems(
            tabItem(
                tabName = "upload",
                sidebarPanel(
                    fileInput(inputId = "file",
                              label = "Choose a CSV file to upload",
                              accept = c("csv", ".csv")),
                    checkboxInput(inputId = "dataheader",
                                  label = "File has header?",
                                  value = TRUE),
                    uiOutput(outputId = "colX"),
                    uiOutput(outputId = "colY")
                    
                ),
                
                mainPanel(
                    tabsetPanel(
                        type = "tabs",
                        tabPanel("Data", DT::dataTableOutput("showData")),
                        tabPanel("summary", verbatimTextOutput("showSummary")),
                        tabPanel("plot", plotlyOutput("showPlot"))
                    )
                )
            )
        )
    )
)

server <- function(input, output, session) {
    
    columns <- reactiveVal()
    
    rawdata <- reactive({
        uploaded <- input$file
        if(is.null(uploaded)) {
            return()
        }
        read.csv(uploaded$datapath,
                 header = input$dataheader)
        
    })
    
    output$showData <- DT::renderDataTable(
        rawdata(),
        options = list(
            pageLength = 10,
            scrollY="400px"
        )
        
    )
    
    output$showSummary <- renderPrint(summary(rawdata()))
    
    observe(columns(unique(colnames(Filter(is.numeric, rawdata())))))
    
    output$colX <- renderUI({
        selectInput(inputId = "xAxis",
                    label = "Select X axis:",
                    choices = columns())
        
    })
    
    output$colY <- renderUI({
        selectInput(inputId = "yAxis",
                    label = "Select Y axis:",
                    choices = columns())
        
    })
    output$showPlot <- renderPlotly({
        g <- ggplot(rawdata(), aes_string(x = input$xAxis, y = input$yAxis)) +
            geom_point(color="blue") +
            theme_bw()
        ggplotly(g)
        
    }) 
}

shinyApp(ui = ui, server = server)

