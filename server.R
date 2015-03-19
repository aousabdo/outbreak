library(shiny)

shinyServer(function(input, output) {
  
  dataTable <- reactive({
    N <- 20 # maximum number of iterations to go through
    
    pUP <- input$pUP/100.
    pDN <- input$pDN/100.
    
    population <- simPopulation(iter = N, Npop = 100, pUP = pUP, pDN = pDN)    
    
    return(population)
  })
  
  # read database and create HS table
  dbDT <- reactive({
    return(fetchDB('himss_test'))#, hoursNMax = as.numeric(input$last)))
  })
  
  DTProcessed <- reactive({
    set.seed(123)
    return(processDT(dbDT(), simulate = input$simulate, pUP = input$pUP/100, pDN = input$pDN/100))
  })
  
  output$outbreakPlot <- renderPlot({
    if (is.null(input$hours))
      return(NULL)
    makePlot(DT = dbDT(), DTW = DTProcessed(), level = input$hours)
    
  })
  
  output$linePlot <- renderPlot({
    linePlot(DT = dbDT(), DTW = DTProcessed())
  })
  
  output$trendPlot <- renderPlot({
    trendPlot(DT = dbDT(), DTW = DTProcessed())
  })
  
  output$trendPlot2 <- renderPlot({
    trendPlot2(DT = dbDT(), DTW = DTProcessed())
  })
  
  output$hoursControl <- renderUI({
    sliderInput(inputId = "hours", label = "Hours", value = 1, min = 1, step = as.numeric(input$lapse),
                max = dbDT()[, length(unique(health_status_snapshot_date))],
                animate = animationOptions(interval = 2100, loop = FALSE))
  })
})
