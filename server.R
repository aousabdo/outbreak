
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(input, output) {
  
  dataTable <- reactive({
    N <- 20 # maximum number of iterations to go through
    
    pUP <- input$pUP/100.
    pDN <- input$pDN/100.
    
    population <- simPopulation(iter = N, Npop = 100, pUP = pUP, pDN = pDN)    
    
    return(population)
  })

  dbDT <- reactive({
    return(fetchDB('himss_test'))
  })
  
  output$outbreakPlot <- renderPlot({
    # simPop <- dataTable()
    simPop <- processDT(dbDT(), simulate = F)
    makePlot(DT = simPop, level = input$iteration)
  })
  
  output$linePlot <- renderPlot({
    linePlot(dataTable())
  })
  
  output$trendPlot <- renderPlot({
    trendPlot(dataTable())
  })
  
})
