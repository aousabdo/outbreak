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
    set.seed(123)
    return(processDT(dbDT(), simulate = T))
  })
    
  output$outbreakPlot <- renderPlot({
    makePlot(DT = dbDT(), DTW = DTProcessed(), level = input$iteration)
    
  })
  
  output$linePlot <- renderPlot({
    linePlot(DT = dbDT(), DTW = DTProcessed(), xmin = 1, xmax = 100)
  })
  
  output$trendPlot <- renderPlot({
    trendPlot(DT = dbDT(), DTW = DTProcessed())
  })
  
})
