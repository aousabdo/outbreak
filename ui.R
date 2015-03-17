
library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Disease Outbreak in Population"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("iteration",
                  "Hours:", min = 1, max = 79, value = 1, animate = animationOptions(interval = 2100, loop = FALSE))
      ),

    mainPanel(
      tabsetPanel(
        tabPanel("Outbreak", plotOutput("outbreakPlot", width = "600px", height = "550px")),
        tabPanel("Line Plot", plotOutput("linePlot", width = "1000px", height = "800px")),
        tabPanel("Trend Plot", plotOutput("trendPlot", width = "800px", height = "650px"))
        )
      )
    )
  ))
  