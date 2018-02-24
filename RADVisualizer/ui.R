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
  )
)