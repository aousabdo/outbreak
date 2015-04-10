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
    DT <- fetchDB('himss_live', startDate = '2015-04-01')
    if(input$dayTimeOnlyBox) DT <- dayTimeOnlyFun(DT, dayStart = 8, dayEnd = 21)
    return(DT)
  })
  
  dbDTFinal <- reactive({
    DT <- dbDT()
    displayDays <- NULL
    if(input$daysCheckBox) displayDays <- input$days
    return(selectDT(DT = DT, day = displayDays))
  })
  
  DTProcessed <- reactive({
    set.seed(123)
    return(processDT(dbDTFinal(), simulate = input$simulate, pUP = input$pUP/100, pDN = input$pDN/100))
  })
  
  output$outbreakPlot <- renderPlot({
    if (is.null(input$hours))
      return(NULL)
    makePlot(DT = dbDTFinal(), DTW = DTProcessed(), level = input$hours)
  })
  
  output$linePlot <- renderPlot({
    linePlot(DT = dbDTFinal(), DTW = DTProcessed())
  })
  
  output$trendPlot <- renderPlot({
    trendPlot(DT = dbDTFinal(), DTW = DTProcessed())
  })
  
  output$trendPlot2 <- renderPlot({
    trendPlot2(DT = dbDTFinal(), DTW = DTProcessed())
  })
  
  output$hoursControl <- renderUI({
    sliderInput(inputId = "hours", label = "Hours", value = 1, min = 1, step = as.numeric(input$lapse),
                max = dbDTFinal()[, length(unique(health_status_snapshot_date))],
                animate = animationOptions(interval = 2100, loop = FALSE))
  })
  
  output$daysControl <- renderUI({
    DT <- dbDT()
    days <- unique(grep("2015", unlist(strsplit(DT[1:nrow(DT), health_status_snapshot_date], split = " ")), value = T))
    selectInput("days",label = "Select Days to Display", choices = days)
  })
  
#   output$dateRangeControl <- renderUI({
#     DT <- dbDT()
#     days <- unique(grep("2015", unlist(strsplit(DT[1:nrow(DT), health_status_snapshot_date], split = " ")), value = T))
#     dateRangeInput('dateRange',
#                    label = "Date Range",
#                    start = days[1], end = days[length(days)],
#                    min = days[1], max = days[length(days)]
#     )
#   })
})
