library(shiny)
library(ggplot2)

shinyServer(function(input, output) {
  
  GetInputFile <- reactive({
    in.file <- input$file
    
    shiny::validate(need(!is.null(in.file), "Input file required"))
    
    in.file
  })
  
  GetDataFrame <- reactive({
    in.file <- GetInputFile()
    
    if (input$delimFile == "tsv") {
      sep.choice <- '\t'
    }
    else if (input$delimFile == "csv") {
      sep.choice <- ','
    }
    
    read.csv(
      in.file$datapath,
      header = TRUE,
      stringsAsFactors = FALSE,
      sep = sep.choice
    )
  })
  
  GetNumericVariables <- reactive({
    data_frame <- GetDataFrame()
    
    vars <- names(data_frame)
    
    numeric_vars <- c()
    
    for(var in vars) {
      if (typeof(data_frame[, var]) == "double" | typeof(data_frame[, var]) == "integer") {
        numeric_vars <- c(numeric_vars, var)
      }
    } 
    
    numeric_vars
  })
  
  GetCategoricalVariables <- reactive({
    data_frame <- GetDataFrame()
    vars <- names(data_frame)
    numeric_vars <- GetNumericVariables()
    categorical_vars <- vars[!vars %in% numeric_vars]
    categorical_vars
  })
  
  output$scatterPlotVariableXInput <- renderUI({
    numeric_vars <- GetNumericVariables()
    
    selectInput('scatterPlotVariableX',
                'Variable on X-axis',
                numeric_vars,
                multiple = FALSE)
  })
  
  output$scatterPlotVariableYInput <- renderUI({
    numeric_vars <- GetNumericVariables()
    
    selectInput('scatterPlotVariableY',
                'Variable on Y-axis',
                numeric_vars,
                multiple = FALSE)
  })
  
  output$scatterPlotOutput <- renderPlot({
    
    if(input$plotScatter == 0)
      return()
    
    isolate({
      data_frame <- GetDataFrame()
      
      xvar <- input$scatterPlotVariableX
      yvar <- input$scatterPlotVariableY
      
      ggplot(data_frame, aes(x = data_frame[, xvar],
                             y = data_frame[, yvar])) +
        geom_point(color = input$scatterPlotColor) + 
        labs(title = paste0("Scatter plot of ", yvar, " vs ", xvar),
             x = xvar,
             y = yvar)
    })
  })
  
  output$scatterPlotCode <- renderText({
    
    if(input$plotScatter == 0)
      return()
    
    isolate({
      line_1 <- paste0('ggplot(data_frame, aes(x = ', input$scatterPlotVariableX, ',' ,
                       'y = ', input$scatterPlotVariableY, '))', ' +\n')
      line_2 <- paste0('color = ', input$scatterPlotColor, ' +\n')
      line_3 <- paste0('labs(title = "Scatter plot of ', input$scatterPlotVariableY, ' vs ', input$scatterPlotVariableX, '", x = ', input$scatterPlotVariableX,
                       ', y = ', input$scatterPlotVariableY, ')')
      
      paste0(line_1, line_2, line_3)
      
    })
  })
  
})
