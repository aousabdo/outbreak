
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Disease Outbreak in Population"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("iteration",
                  "Number of iterations:", min = 1, max = 79, value = 1, animate = animationOptions(interval = 1250, loop = FALSE)),
      br(),
      wellPanel(
        helpText("Probabilities per iteration:"),
        sliderInput("pUP",
                    "Probability of getting sicker:", min = 20, max = 50, value = 42),
        br(),
        sliderInput("pDN",
                    "Probability of recovery:", min = 20, max = 50, value = 25))),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Outbreak", plotOutput("outbreakPlot", width = "600px", height = "550px")),
        tabPanel("Line Plot", plotOutput("linePlot", width = "1000px", height = "800px")),
        tabPanel("Trend Plot", plotOutput("trendPlot", width = "800px", height = "650px"))
        )
      )
    )
  ))
  