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
  
  output$barPlotVariableXInput <- renderUI({
    categorical_vars <- GetCategoricalVariables()
    selectInput('barPlotVariableX',
                'Variable on X-axis',
                categorical_vars,
                multiple = FALSE)
  })
  
  output$barPlotVariableColorInput <- renderUI({
    categorical_vars <- GetCategoricalVariables()
    selectInput('barPlotVariableColor',
                'Variable for color',
                categorical_vars,
                multiple = FALSE)
  })
  
  output$barPlotOutput <- renderPlot({
    
    if(input$plotBar == 0)
      return()
    
    isolate({
      data_frame <- GetDataFrame()
      
      ggplot(data = data_frame, mapping = aes(x = data_frame[, input$barPlotVariableX],
                                              fill = data_frame[, input$barPlotVariableColor])) + 
      geom_bar(position = "dodge") +
      labs(title = paste0("Bar plot of ", input$barPlotVariableX, " with ", input$barPlotVariableColor),
            x = input$barPlotVariableX,
            y = "Count",
            fill = input$barPlotVariableColor)
    })
  })
  
  output$barPlotCode <- renderText({
    
    if(input$plotBar == 0) 
      return()
    
    isolate({
        text <- paste0('ggplot(data = data_frame, mapping = aes(x = data_frame[,', input$barPlotVariableX, '],
                       fill = data_frame[,', input$barPlotVariableColor,'])) + \n
                       geom_bar(position = "dodge") + \n
                       labs(title = "Bar plot of ', input$barPlotVariableX, ' with ', input$barPlotVariableColor, '"
                       x = ', input$barPlotVariableX, '
                       y = "Count",
                       fill = ', input$barPlotVariableColor, ')')
    })
    
  })
  
})
