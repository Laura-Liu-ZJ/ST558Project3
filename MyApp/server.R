####################################################
# Purpose: server for St558 project3 app           #
# Author: Zhijun Liu                               #
# Date: 2020-07-24                                 #
####################################################


####################################
author <- "Zhijun(Laura) Liu"      #
email <- "zliu63@ncsu.edu"         #
####################################



####################################
#        Read in raw data          #
####################################

data <- read_csv("heart_failure_clinical_records_dataset.csv") %>%
    # change the variable names
    rename( "CPK"=creatinine_phosphokinase,
            "EF" = ejection_fraction,
            "HT" = high_blood_pressure,
            "creatinine" = serum_creatinine,
            "sodium" = serum_sodium,
            "death" = DEATH_EVENT) %>%
    # create a new variable called survive
    mutate(survive = 1-death) %>% select (-"death")


# create a new data set which is similar with data
factorData <- data
# change the boolean variable into categorical data
factorData$anaemia <- as.factor(factorData$anaemia)
factorData$diabetes <- as.factor(factorData$diabetes)
factorData$HT <- as.factor(factorData$HT)
factorData$sex <- as.factor(factorData$sex)
factorData$smoking <- as.factor(factorData$smoking)
factorData$survive <- as.factor(factorData$survive)



####################################
#            Shiny Server          #
####################################

shinyServer(function(input, output, session, options = options(warn = -1)) {
    
    output$author <- renderUI({
        fluidRow(
            column(12,
                   offset = 1,
                   br(),
                   h5(paste("Author:", author), 
                      style = "color:gray;"
                      ),
                   h5(tagList("E-mail:", a(email, href = paste0("mailto:", email))),
                      style = "color:gray;"
                      )
            )
        )
    })
    
    output$ui_sidebar <- renderUI({
        sidebarMenu(id = "tab",
                    
                    menuItem("Information", 
                             tabName = "info", 
                             icon = icon("cube")
                    ),
                    menuItem("Exploratory Data Analysis", 
                             tabName = "eda", 
                             icon = icon("chart-pie")
                    ),
                    menuItem("Principal Component Analysis", 
                             tabName = "pca", 
                             icon = icon("project-diagram")
                    ),
                    menuItem("Modeling", 
                             tabName = "model", 
                             icon = icon("laptop")
                    ),
                    menuItem("Data", 
                             tabName = "data", 
                             icon = icon("table")
                    ),
                    
                    uiOutput("author")
        )
    })
    
    output$ui_body <- renderUI({
        updateTabsetPanel(session, "tab", selected = "info")
        tabItems(
            tabItem_info,
            tabItem_eda,
            tabItem_pca,
            tabItem_model,
            tabItem_data
        )
    })
    
    
    # Modules
    source('tab/data.R', local = TRUE)
    source('tab/info.R', local = TRUE)
    source('tab/eda.R', local = TRUE)
    source('tab/pca.R', local = TRUE)
    source('tab/model.R', local = TRUE)
    
})  