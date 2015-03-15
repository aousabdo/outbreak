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
    return(fetchDB('himss_test'))
  })
  
  DTProcessed <- reactive({
    return(processDT(dbDT(), simulate = T))
  })
    
  output$outbreakPlot <- renderPlot({
    population <- DTProcessed()
    makePlot(DT = population, level = input$iteration) #length(population[, grep("HS.", colnames(population))])
    
  })
  
  output$linePlot <- renderPlot({
    population <- DTProcessed() 
    linePlot(population, xmin = 1, xmax = 100)
  })
  
  output$trendPlot <- renderPlot({
    population <- DTProcessed() 
    trendPlot(population)
  })
  
})
