
library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Disease Outbreak in Population"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        helpText(HTML("<p> <font color=\"black\" size = 3>  Disease Outbreak Animation </font> </p> <hr>")),
        uiOutput("hoursControl"),
        tags$hr(),
        selectInput("lapse", "Animation lapse time in hours", choices = c(1, 2, 6, 12), selected = 1)),
      wellPanel(
        # uiOutput("dateRangeControl"),
        helpText(HTML("<p> <font color=\"black\" size = 3>  Date and Time Selection </font> </p> <hr>")),
        checkboxInput("dayTimeOnlyBox", "Only show day-time events", value = TRUE),
        tags$hr(),
        checkboxInput("daysCheckBox", label = "Select Days to Display", value = FALSE),
        conditionalPanel(condition = "input.daysCheckBox",
                         uiOutput("daysControl"))
      ), 
      wellPanel(
        helpText(HTML("<p> <font color=\"black\" size = 3>  Data Simulatoin </font> </p>")),
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
