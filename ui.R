####################################################
# Purpose: ui for St558 project3 app               #
# Author: Zhijun Liu                               #
# Date: 2020-07-24                                 #
####################################################

# prepare the packages
library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(DT)
library(ggplot2)
library(GGally)
library(tidyverse)
library(shinythemes)
library(plotly)
library(rgl)
library(pca3d)
library(tree)


####################################
#              Shiny UI            #
####################################

# ready for rgl plot
options(rgl.useNULL = TRUE)


dashboardPage(skin = "red",
              dashboardHeader(title = "Heart Failure Clinical Records"),
              dashboardSidebar(uiOutput("ui_sidebar")),
              dashboardBody(
                  shinyDashboardThemes(
                      theme = "flat_red"
                  ),
                  uiOutput("ui_body")
              )
)