library(shiny)
library(shinydashboard)
library(colourpicker)
library(leaflet)

dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      radioButtons("delimFile", "Select file format:",
                   choices = c("csv", "tsv"), inline = TRUE),
      fileInput(
        "file",
        "Upload data file ",
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
      menuItem("Data Visualization",
        menuSubItem("Scatter Plot", tabName = "scatterPlot"),
        menuSubItem("Bar Plot", tabName = "barPlot"),
        menuSubItem("Box Plot", tabName = "boxPlot"),
        menuSubItem("Histograms", tabName = "histograms"),
        menuSubItem("Geospatial Plot", tabName = "geospatialmap")
      ),
      menuItem("Machine Learning",
        radioButtons("testFileOpt", "Upload or split train set:", choices = c("Upload", "Split"),
                     inline = TRUE),
        conditionalPanel(condition = "input.testFileOpt == 'Upload'",
                         radioButtons("delimTestFile", "Select file format:",
                                      choices = c("csv", "tsv"), inline = TRUE),
                         fileInput("testFile",
                                   "Upload test file ",
                                   accept = c(
                                     "text/tsv",
                                     "text/tab-separated-values,text/plain",
                                     ".txt",
                                     ".tsv",
                                     "text/comma-separated-values,text/plain",
                                     ".csv"
                                   ))
        ),
        conditionalPanel(condition = "input.testFileOpt == 'Split'",
                         numericInput("trainRatio", "Enter train size (0 - 1)",
                                      min = 0, max = 1.0, step = 0.05, value = 0.7)
        ),
        menuSubItem("Linear Regression", tabName = "linearRegression"),
        menuSubItem("Decision Tree", tabName = "decisionTree")
      )
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
      ),
      
      tabItem(tabName = "geospatialmap",
              fluidRow(
                box("Inputs for Geospatial map",
                    uiOutput("geoVariableLatInput"),
                    uiOutput("geoVariableLongInput"),
                    uiOutput("geoVariableInfoInput"),
                    actionButton("plotMap", "Plot")),
                
                box(leafletOutput("mapPlotOutput")),
                
                box("Code", verbatimTextOutput("mapPlotCode"))
              )
      ),
      
      tabItem(tabName = "linearRegression",
              fluidRow(
                box("LR setup",
                  uiOutput("lrVariables"),
                  uiOutput("lrPredictor"),
                  actionButton("runLR", "Run")
                ),
                
                box("Predictions",
                    wellPanel(
                      tableOutput("predictionsLR"),
                      style = "overflow-y:scroll; max-height: 200px;
                       overflow-x:scroll; auto"
                    ),
                    textOutput("mseLR"))
              )
      ),
      
      tabItem(tabName = "decisionTree",
              fluidRow(
                box("DT setup",
                    uiOutput("dtVariables"),
                    uiOutput("dtPredictor"),
                    actionButton("runDT", "Run")
                ),
                
                box("Predictions",
                    wellPanel(
                      tableOutput("predictionsDT"),
                      style = "overflow-y:scroll; max-height: 200px;
                      overflow-x:scroll; auto"
                    )
                ),
                
                box("Plot",
                    plotOutput("plotDT"))
                )
        )
    )
  )
)