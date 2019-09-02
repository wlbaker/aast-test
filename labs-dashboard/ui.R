library(shiny)

labs <- data.frame(
  names=c( "Creatinine", "Troponin", "Myoglobin",  "Lactate", "pH"),
  y_min=c( 0, 0, 0, 0, 7.0 ),
  y_max=c( 3.7, 25, 2000, 16, 8.0 )
)

all_grps <- c("TC", "NC", "PC", "FR", "P45", "P60")
default_grps <- c("FR", "P45", "P60")

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Labs"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId="lab",
                  label="Lab Name:",
                  choices=labs$names),
      checkboxGroupInput(inputId="checkGroup",
                  label="Study Groups:",
                  choices= all_grps,
                  selected= default_grps),
      numericInput( "y_min", "Y Minimum", value="0"),
      numericInput( "y_max", "Y Maximum", value="20")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("labPlot")
    )
  )
))
