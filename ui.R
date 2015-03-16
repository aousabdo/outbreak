
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
                  "Hours:", min = 1, max = 79, value = 1, animate = animationOptions(interval = 1250, loop = FALSE))
      ),
    
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
  