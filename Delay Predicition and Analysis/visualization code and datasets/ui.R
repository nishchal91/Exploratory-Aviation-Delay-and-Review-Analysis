library(shiny)
library(plotly)
# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel(" Delay Analysis of Flight Data - 2006-2007 "),
  br(),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      helpText("Select variables for analysis"),
      
      selectInput("var", 
                  label  = "Choose a variable to display",
                  choices = list("By Day of Week", "By Month of Year",
                                 "By Hour of Day","By Carrier"),
                  selected = "By Day of Week"),
      selectInput("origin", 
                  label = "Origin Airport",
                  choices = list("JFK","LAX","SFO","All"),
                  selected = "All")
    
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("distPlot")
    )
  )
))
