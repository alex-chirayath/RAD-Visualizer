library(shiny)
library(shinydashboard)
library(colourpicker)

dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      radioButtons("delimFile", "Select file format:",
                   choices = c("csv", "tsv")),
      fileInput(
        "file",
        "Upload data file in selected format: ",
        accept = c(
          "text/tsv",
          "text/tab-separated-values,text/plain",
          ".txt",
          ".tsv",
          "text/comma-separated-values,text/plain",
          ".csv"
        )
      ),
      menuItem("Summary", tabName = "summary"),
      menuItem("Scatter Plot", tabName = "scatterPlot"),
      menuItem("Bar Plot", tabName = "barPlot"),
      menuItem("Box Plot", tabName = "boxPlot"),
      menuItem("Histograms", tabName = "histograms")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "summary",
              fluidRow(
                box("First ten rows of input file",
                    dataTableOutput("headData")),
                box("Correlation Matrix",
                    tableOutput("corrMatrixData")),
                box("Mean",
                    tableOutput("meanData"),
                    "Median",
                    tableOutput("medianData"))
              )),
      tabItem(tabName = "scatterPlot",
              fluidRow(
                box("Inputs for Scatter Plot",
                    uiOutput("scatterPlotVariableXInput"),
                    uiOutput("scatterPlotVariableYInput"),
                    colourInput("scatterPlotColor", "Select color ", "black"),
                    actionButton("plotScatter", "Plot")),
                
                box(plotOutput("scatterPlotOutput")),
                
                box("Code", verbatimTextOutput("scatterPlotCode"))
              )
      ),
      
      tabItem(tabName = "barPlot",
              fluidRow(
                box("Inputs for Bar Plot",
                    uiOutput("barPlotVariableXInput"),
                    uiOutput("barPlotVariableColorInput"),
                    checkboxInput("barstacked", "Stacked?", value = FALSE),
                    actionButton("plotBar", "Plot")),
                
                box(plotOutput("barPlotOutput")),
                
                box("Code", verbatimTextOutput("barPlotCode"))
              )
      ),
      
      tabItem(tabName = "boxPlot",
              fluidRow(
                box("Inputs for Box Plot",
                    uiOutput("boxPlotVariableXInput"),
                    uiOutput("boxPlotVariableYInput"),
                    actionButton("plotBox", "Plot")),
                
                box(plotOutput("boxPlotOutput")),
                
                box("Code", verbatimTextOutput("boxPlotCode"))
              )
      ),
      
      tabItem(tabName = "histograms",
              fluidRow(
                box("Inputs for Histograms",
                    uiOutput("histVariableXInput"),
                    actionButton("plotHist", "Plot")),
                
                box(plotOutput("histPlotOutput")),
                
                box("Code", verbatimTextOutput("histPlotCode"))
              )
      )
    )
  )
)