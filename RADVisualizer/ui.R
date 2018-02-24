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
                    actionButton("plotBar", "Plot")),
                
                box(plotOutput("barPlotOutput")),
                
                box("Code", verbatimTextOutput("barPlotCode"))
              )
      )
    )
  )
)