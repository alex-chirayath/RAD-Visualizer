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
      
      if (input$barstacked == FALSE) {
        ggplot(data = data_frame, mapping = aes(x = data_frame[, input$barPlotVariableX],
                                                fill = data_frame[, input$barPlotVariableColor])) + 
          geom_bar(position = "dodge") +
          labs(title = paste0("Bar plot of ", input$barPlotVariableX, " with ", input$barPlotVariableColor),
               x = input$barPlotVariableX,
               y = "Count",
               fill = input$barPlotVariableColor)
      }
      else {
        ggplot(data = data_frame, mapping = aes(x = data_frame[, input$barPlotVariableX],
                                                fill = data_frame[, input$barPlotVariableColor])) + 
          geom_bar() +
          labs(title = paste0("Bar plot of ", input$barPlotVariableX, " with ", input$barPlotVariableColor),
               x = input$barPlotVariableX,
               y = "Count",
               fill = input$barPlotVariableColor)
      }
    })
  })
  
  output$barPlotCode <- renderText({
    
    if(input$plotBar == 0) 
      return()
    
    isolate({
      if (input$barstacked == FALSE) {
        text <- paste0('ggplot(data = data_frame, mapping = aes(x = data_frame[,', input$barPlotVariableX, '],
                       fill = data_frame[,', input$barPlotVariableColor,'])) + \n
                       geom_bar(position = "dodge") + \n
                       labs(title = "Bar plot of ', input$barPlotVariableX, ' with ', input$barPlotVariableColor, '"
                       x = ', input$barPlotVariableX, '
                       y = "Count",
                       fill = ', input$barPlotVariableColor, ')')
      }
      else {
        text <- paste0('ggplot(data = data_frame, mapping = aes(x = data_frame[,', input$barPlotVariableX, '],
                       fill = data_frame[,', input$barPlotVariableColor,'])) + \n
                       geom_bar() + \n
                       labs(title = "Bar plot of ', input$barPlotVariableX, ' with ', input$barPlotVariableColor, '"
                       x = ', input$barPlotVariableX, '
                       y = "Count",
                       fill = ', input$barPlotVariableColor, ')')
      }
    })
    
  })
  
  output$boxPlotVariableXInput <- renderUI({
    categorical_vars <- GetCategoricalVariables()
    selectInput('boxPlotVariableX',
                'Variable on X-axis',
                categorical_vars,
                multiple = FALSE)
  })
  
  output$boxPlotVariableYInput <- renderUI({
    numeric_vars <- GetNumericVariables()
    selectInput('boxPlotVariableY',
                'Variable on Y-axis',
                numeric_vars,
                multiple = FALSE)
  })
  
  output$boxPlotOutput <- renderPlot({
    if (input$plotBox == 0)
      return()
    
    isolate({
      data_frame <- GetDataFrame()
      
      ggplot(data = data_frame, mapping = aes(x = data_frame[, input$boxPlotVariableX], 
                                              y = data_frame[, input$boxPlotVariableY],
                                              fill = data_frame[, input$boxPlotVariableX])) + 
        geom_boxplot() +
        labs(title = paste0("Bar plot of ", input$boxPlotVariableX, " with ", input$boxPlotVariableY),
             x = input$boxPlotVariableX,
             y = input$boxPlotVariableY,
             fill = input$boxPlotVariableX)
    })
  })
  
  output$boxPlotCode <- renderText({
    if (input$plotBox == 0)
      return()
    
    isolate({
      paste0(
        'ggplot(data = data_frame, mapping = aes(x = data_frame[, ' ,input$boxPlotVariableX,'], 
                                                y = data_frame[, ',input$boxPlotVariableY, '],
                                                fill = data_frame[, ', input$boxPlotVariableX, '])) + 
          geom_boxplot() +
          labs(title = "Bar plot of ', input$boxPlotVariableX, ' with ', input$boxPlotVariableY, '",
               x = ', input$boxPlotVariableX, ',
               y = ', input$boxPlotVariableY, ',
               fill = ', input$boxPlotVariableX, ')'
      )
    })
  })
  
  output$histVariableXInput <- renderUI({
    numeric_vars <- GetNumericVariables()
    selectInput('histPlotVariableX',
                'Variable on X-axis',
                numeric_vars,
                multiple = FALSE)
  })
  
  output$histPlotOutput <- renderPlot({
    if(input$plotHist == 0)
      return()
    
    isolate({
      data_frame <- GetDataFrame()
      
      ggplot(data = data_frame, mapping = aes(x = data_frame[, input$histPlotVariableX])) +
        geom_histogram()
    })
  })
  
  output$histPlotCode <- renderText({
    if(input$plotHist == 0)
      return()
    
    isolate({
      data_frame <- GetDataFrame()
      
      paste0(
        'ggplot(data = data_frame, mapping = aes(x = data_frame[,', input$histPlotVariableX, '])) +\n
        geom_histogram() +\n
        labs(title = "Histogram of ', input$histPlotVariableX, '", 
             x = ', input$histPlotVariableX, '
             y = "Count")')
    })
  })
  
})
