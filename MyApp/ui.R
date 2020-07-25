####################################################
# Purpose: ui for St558 project3 app               #
# Author: Zhijun Liu                               #
# Date: 2020-07-24                                 #
####################################################

library(shiny)
library(shinythemes)
library(dashboard)


# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    # Application theme
    theme = shinytheme("sandstone"),

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
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
