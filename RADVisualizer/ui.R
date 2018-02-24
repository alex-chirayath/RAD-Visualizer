library(shiny)
library(shinydashboard)

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
      menuItem("Scatter Plot", tabName = "scatterPlot"),
      menuItem("Bar Plot", tabName = "barPlot"),
      menuItem("Box Plot", tabName = "boxPlot"),
      menuItem("Histograms", tabName = "histograms")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "scatterPlot",
              fluidRow(
                box("Inputs for Scatter Plot",
                    uiOutput("scatterPlotVariableXInput"),
                    uiOutput("scatterPlotVariableYInput"),
                    actionButton("plotScatter", "Plot")),
                
                box(plotOutput("scatterPlotOutput")),
                
                box("Code", verbatimTextOutput("scatterPlotCode"))
              )
      )
    )
  )
)