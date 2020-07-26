tabItem_model <- 
  tabItem(tabName = "model",
          
          navbarPage("Modeling",
            
            tabPanel("Logistic Regression",
                     
                     sidebarPanel("Input"
                                  
                                  
                                  
                     ),
                     
                     mainPanel(
                       # Change
                       plotOutput("model_into")
                       #
                     )
                     
            ), # End of Logistic Regression
            
            tabPanel("Classification Tree",
                     
                     sidebarPanel("Input"
                                  
                                  
                                  
                     ),
                     
                     mainPanel(
                       plotOutput("model_ana")
                     )
                     
            ) # End of Classification Tree
            
          ) # End of navbar
  )

output$model_into <-
  renderPlot(
    ggcorr(data.frame(a = rnorm(100), b = rnorm(50), c = runif(100)))
  )

output$model_ana <-
  renderPlot(
    hist(iris$Sepal.Length)
  )
