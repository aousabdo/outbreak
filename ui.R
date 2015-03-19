
library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Disease Outbreak in Population"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        # selectInput('last', 'Show the most recent hours:', choices = c(6, 12, 24, 36, 48, 72), selected = 24),
        # br(),
        uiOutput("hoursControl"),
        br(),
        selectInput("lapse", "Lapse time in hours", choices = c(1, 2, 6, 12), selected = 1)), 
      wellPanel(
        helpText("Data Simulation"),
        checkboxInput("simulate", label = "Simulate Data", value = FALSE), 
        conditionalPanel(condition = "input.simulate",
                         sliderInput("pUP",
                                     "Probability of getting sicker:", min = 20, max = 50, value = 35),
                         br(),
                         sliderInput("pDN",
                                     "Probability of recovery:", min = 20, max = 50, value = 25))
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Disease Outbreak", plotOutput("outbreakPlot", width = "600px", height = "550px")),
        tabPanel("Disease Trend", plotOutput("trendPlot", width = "800px", height = "650px")),
        tabPanel("Recovery Trend", plotOutput("trendPlot2", width = "800px", height = "650px")),
        tabPanel("Health Status Trend", plotOutput("linePlot", width = "1000px", height = "800px"))
      )
    )
  )
))
