#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

labs <- data.frame(
    names=c( "Creatinine", "Troponin", "Myoglobin",  "Lactate", "pH"),
    y_min=c( 0, 0, 0, 0, 7.0 ),
    y_max=c( 3.7, 25, 2000, 16, 8.0 )
)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Survival"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            textInput( "title", "Plot Title", value="Survivalll"),
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot")
        )
    )
))
