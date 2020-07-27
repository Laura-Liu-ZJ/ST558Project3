c_list <- list("anaemia","diabetes","HT","sex","smoking")
n_list <- list("age","CPK","EF","platelets","creatinine","sodium","time")

# user input change NULL to NA
user_in <- function(var, type = "n"){
  if(type == "n"){
    if(!is.null(var)){as.numeric(var)}else{NA}
  }else if(type == "c"){
    if(!is.null(var)){as.factor(var)}else{NA}
  }
  
}

tabItem_model <- 
  tabItem(tabName = "model",
          
          navbarPage("Modeling",
                     
                     tabPanel("Logistic Regression",
                              sidebarPanel(h4("Input Panel"),
                                           column(6,
                                                  checkboxGroupInput("log_nvar",
                                                                     "Numerical",
                                                                     choiceNames = n_list,
                                                                     choiceValues = n_list,
                                                                     selected = c(n_list[3],n_list[5])
                                                  )
                                           ),
                                           column(6,
                                                  checkboxGroupInput("log_cvar",
                                                                     "Categorical",
                                                                     choiceNames = c_list,
                                                                     choiceValues = c_list,
                                                                     selected = FALSE
                                                  )
                                           ),
                                           br(),
                                           actionButton("log_submit_1",
                                                        "Submit",
                                                        class = "btn btn-primary"),
                                           conditionalPanel(condition = "input.log_submit_1 > 0",
                                                            hr(),
                                                            h4("Prediction"),
                                                            uiOutput("log_n1"),
                                                            uiOutput("log_n2"),
                                                            uiOutput("log_n3"),
                                                            uiOutput("log_n4"),
                                                            uiOutput("log_n5"),
                                                            uiOutput("log_n6"),
                                                            uiOutput("log_n7"),
                                                            uiOutput("log_c1"),
                                                            uiOutput("log_c2"),
                                                            uiOutput("log_c3"),
                                                            uiOutput("log_c4"),
                                                            uiOutput("log_c5"),
                                                            actionButton("log_submit_2",
                                                                         "Predict",
                                                                         class = "btn btn-primary")
                                           )
                                           
                              ),
                              
                              mainPanel(
                                withMathJax(),
                                div(includeMarkdown("info/log.md"), 
                                    align = "justify"),
                                hr(),
                                # analysis resuts after submit
                                conditionalPanel(
                                  condition = "input.log_submit_1 > 0",
                                  h4("Analysis"),
                                  verbatimTextOutput("log_sum"),
                                  conditionalPanel(
                                    condition = "input.log_submit_2 == 0",
                                    h4("Ready for prediction. Please click the Predict button.",
                                       style = "color:gray;")
                                  )
                                ),
                                
                                # prune resutls
                                conditionalPanel(
                                  condition = "input.log_submit_2 > 0",
                                  br(),
                                  hr(),
                                  h4("Prediction Result"),
                                  column(12,
                                         offset = 0.5,
                                         h4(textOutput("log_pre"),
                                            style = "color:darkred;"))
                                ),
                                
                                # default text
                                conditionalPanel(
                                  condition = "input.log_submit_1 == 0",
                                  h4("Ready for analysis. Please click the Submit button.",
                                     style = "color:gray;")
                                )
                                
                              )
                              
                     ), # End of Logistic Regression
                     
                     tabPanel("Classification Tree",
                              
                              sidebarPanel(h4("Input Panel"),
                                           column(6,
                                                  checkboxGroupInput("tree_nvar",
                                                                     "Numerical",
                                                                     choiceNames = n_list,
                                                                     choiceValues = n_list,
                                                                     selected = c(n_list[3],n_list[5])
                                                  )
                                           ),
                                           column(6,
                                                  checkboxGroupInput("tree_cvar",
                                                                     "Categorical",
                                                                     choiceNames = c_list,
                                                                     choiceValues = c_list,
                                                                     selected = FALSE
                                                  )
                                           ),
                                           br(),
                                           actionButton("tree_submit_1",
                                                        "Submit",
                                                        class = "btn btn-primary"),
                                           
                                           conditionalPanel(condition = "input.tree_submit_1 > 0",
                                                            hr(),
                                                            h4("Pruning"),
                                                            numericInput("tree_num",
                                                                         "How many trees do you want?",
                                                                         value = 2
                                                            ),
                                                            hr(),
                                                            h4("Prediction"),
                                                            uiOutput("tree_n1"),
                                                            uiOutput("tree_n2"),
                                                            uiOutput("tree_n3"),
                                                            uiOutput("tree_n4"),
                                                            uiOutput("tree_n5"),
                                                            uiOutput("tree_n6"),
                                                            uiOutput("tree_n7"),
                                                            uiOutput("tree_c1"),
                                                            uiOutput("tree_c2"),
                                                            uiOutput("tree_c3"),
                                                            uiOutput("tree_c4"),
                                                            uiOutput("tree_c5"),
                                                            actionButton("tree_submit_2",
                                                                         "Predict",
                                                                         class = "btn btn-primary")
                                           )
                                           
                              ),
                              
                              mainPanel(
                                # Introduction
                                withMathJax(),
                                div(includeMarkdown("info/tree.md"), 
                                    align = "justify"),
                                hr(),
                                
                                # analysis resuts after submit
                                conditionalPanel(
                                  condition = "input.tree_submit_1 > 0",
                                  h4("Analysis"),
                                  verbatimTextOutput("tree_sum"),
                                  plotOutput("tree_plt"),
                                  column(12,
                                         align = "right",
                                         downloadButton("tree_save",
                                                        "Save Plot")
                                  ),
                                  h4("Referrence"),
                                  column(4,
                                         align = "center",
                                         br(),
                                         h5("Deviance Table"),
                                         tableOutput("tree_ref_tb")
                                  ),
                                  column(8,
                                         plotOutput("tree_ref_plt")
                                  ),
                                  conditionalPanel(
                                    condition = "input.tree_submit_2 == 0",
                                    h4("Ready for prediction. Please click the Predict button.",
                                       style = "color:gray;")
                                  )
                                ),
                                
                                # prune resutls
                                conditionalPanel(
                                  condition = "input.tree_submit_2 > 0",
                                  br(),
                                  hr(),
                                  h4("Prune Result"),
                                  plotOutput("tree_pru_plt"),
                                  h4("Prediction Result"),
                                  column(12,
                                         offset = 0.5,
                                         h4(textOutput("tree_pre"),
                                            style = "color:darkred;"))
                                ),
                                
                                # default text
                                conditionalPanel(
                                  condition = "input.tree_submit_1 == 0",
                                  h4("Ready for analysis. Please click the Submit button.",
                                     style = "color:gray;")
                                )
                                
                              )
                              
                     ) # End of Classification Tree
                     
          ) # End of navbar
  )

# prevent EMPTY selections
observe({
  if(length(input$tree_nvar)+length(input$tree_cvar) < 1){
    updateCheckboxGroupInput(session, "tree_nvar", selected= c(n_list[3],n_list[5]))
  }
})

observe({
  if(length(input$log_nvar)+length(input$log_cvar) < 1){
    updateCheckboxGroupInput(session, "log_nvar", selected= c(n_list[3],n_list[5]))
  }
})


################
#  LOG results #
################

# log model
log_model <- reactive({
  # prepare data for logistic regression
  model_data <- factorData %>% select(input$log_nvar,input$log_cvar,survive)
  # logistic regression
  glmFit <- glm(survive ~ ., data=model_data,family = "binomial")
  
})

# summary
output$log_sum <- 
  renderPrint({
    if (req(input$log_submit_1)>0) { 
      isolate({
        glmFit <- log_model()
        # result of logistic regression
        summary(glmFit)
      })
    }
  })


output$log_pre <-
  renderText({
    if (req(input$log_submit_2)>0) { 
      isolate({
        glmFit <- log_model()
        # predict
        predFrac<- exp(predict(glmFit, 
                               newdata = data.frame(
                                 # user defined value
                                 age = user_in(input$log_nvar1),
                                 CPK = user_in(input$log_nvar2),
                                 EF = user_in(input$log_nvar3),
                                 platelets = user_in(input$log_nvar4),
                                 creatinine = user_in(input$log_nvar5),
                                 sodium = user_in(input$log_nvar6),
                                 time = user_in(input$log_nvar7),
                                 anaemia = user_in(input$log_cvar1,type = "c"),
                                 diabetes = user_in(input$log_cvar2,type = "c"),
                                 HT = user_in(input$log_cvar3,type = "c"),
                                 sex = user_in(input$log_cvar4,type = "c"),
                                 smoking = user_in(input$log_cvar5,type = "c")
                               ),type="link"))
        
        # prediction results
        if (predFrac > 1){
          '"Survive"'
        } else {
          '"Not survive"'
        }
      })
    }
    
  })


################
# TREE results #
################

# tree model
tree_model <- reactive({
  # prepare data for tree model
  model_data <- factorData %>% select(input$tree_nvar,input$tree_cvar,survive)
  # fit the tree model
  treeFit <- tree(survive ~ . ,data=model_data,split="deviance")
})


# summary
output$tree_sum <- 
  renderPrint({
    if (req(input$tree_submit_1)>0) { 
      isolate({
        treeFit <- tree_model()
        # result of tree model
        summary(treeFit)
      })
    }
  })

# plot
output$tree_plt <-
  renderPlot({
    isolate({
      treeFit <- tree_model()
    })
    if (req(input$tree_submit_1)>0) { 
      # tree model plot
      plot(treeFit, col = "grey")
      text(treeFit, pretty = 0, cex = 1)
    }
  })

# Referrence

## table
output$tree_ref_tb <-
  renderTable({
    if (req(input$tree_submit_1)>0) { 
      isolate({
        data_factor_tree <- factorData %>% select(input$tree_nvar,input$tree_cvar,survive)
        treeFit <- tree(survive ~ . ,data=data_factor_tree,split="deviance")
        # giving the referrence result for choosing tree size
        pruneFit <- cv.tree(treeFit, FUN = prune.misclass)
        data.frame(size=pruneFit$size,deviance=pruneFit$dev)
      })
    }
  })

plot_tree <- reactive({
  data_factor_tree <- factorData %>% select(input$tree_nvar,input$tree_cvar,survive)
  treeFit <- tree(survive ~ . ,data=data_factor_tree,split="deviance")
  pruneFit <- cv.tree(treeFit, FUN = prune.misclass)
  # referrence plot for choosing tree size
  plot(pruneFit$size,pruneFit$dev,type="b",col="#990000",lwd=2,
       main="Deviance Plot",xlab="Size",ylab="Deviance")
})

## plot
output$tree_ref_plt <-
  renderPlot({
    isolate({
      data_factor_tree <- factorData %>% select(input$tree_nvar,input$tree_cvar,survive)
      treeFit <- tree(survive ~ . ,data=data_factor_tree,split="deviance")
      pruneFit <- cv.tree(treeFit, FUN = prune.misclass)
    })
    if (req(input$tree_submit_1)>0) { 
      plot(pruneFit$size,pruneFit$dev,type="b",col="#990000",lwd=2,
           main="Deviance Plot",xlab="Size",ylab="Deviance")
    }
  })

## save plot
output$tree_save <-
  downloadHandler(
    filename = "Tree Plot.png",
    content = function(file){
      png(file)
      isolate({
        treeFit <- tree_model()
      })
      if (req(input$tree_submit_1)>0) { 
        # tree model plot
        plot(treeFit, col = "grey")
        text(treeFit, pretty = 0, cex = 1)
      }
      dev.off()  # turn the device off
    }
  )

# prune plot
output$tree_pru_plt <-
  renderPlot({
    isolate({
      treeFit <- tree_model()
      # prune for better prediction
      pruneFitFinal <- prune.misclass(treeFit,best=as.numeric(input$tree_num)) # choose the tree size in best option
    })
    if (req(input$tree_submit_2)>0) { 
      plot(pruneFitFinal, col = "grey")
      text(pruneFitFinal, pretty = 0, cex = 1)
    }
  })


# prediction
output$tree_pre <-
  renderText({
    if (req(input$tree_submit_2)>0) { 
      isolate({
        treeFit <- tree_model()
        pruneFitFinal <- prune.misclass(treeFit,best=as.numeric(input$tree_num))
        
        predClass <- predict(pruneFitFinal,
                             newdata = data.frame(
                               # user defined value
                               age = user_in(input$tree_nvar1),
                               CPK = user_in(input$tree_nvar2),
                               EF = user_in(input$tree_nvar3),
                               platelets = user_in(input$tree_nvar4),
                               creatinine = user_in(input$tree_nvar5),
                               sodium = user_in(input$tree_nvar6),
                               time = user_in(input$tree_nvar7),
                               anaemia = user_in(input$tree_cvar1,type = "c"),
                               diabetes = user_in(input$tree_cvar2,type = "c"),
                               HT = user_in(input$tree_cvar3,type = "c"),
                               sex = user_in(input$tree_cvar4,type = "c"),
                               smoking = user_in(input$tree_cvar5,type = "c")
                               
                             ),type="class"
        )
        
        if (predClass == 1){
          '"Survive"'
        } else {
          '"Not survive"'
        }
      })
    }
    
  })



#############
# LOG panel #
#############

output$log_n1 <-
  renderUI({
    if(n_list[1] %in% input$log_nvar){   # age
      sliderInput("log_nvar1",
                  n_list[1],
                  value = 0,
                  min = 0,
                  max = 150,
                  step = 1
      )}
  })

output$log_n2 <-
  renderUI({
    if(n_list[2] %in% input$log_nvar){   # CPK
      sliderInput("log_nvar2",
                  n_list[2],
                  value = 0,
                  min = 0,
                  max = 10000,
                  step = 1
      )}
  })

output$log_n3 <-
  renderUI({
    if(n_list[3] %in% input$log_nvar){   # EF
      sliderInput("log_nvar3",
                  n_list[3],
                  value = 0,
                  min = 0,
                  max = 100,
                  step = 1
      )}
  })

output$log_n4 <-
  renderUI({
    if(n_list[4] %in% input$log_nvar){   # platelets
      sliderInput("log_nvar4",
                  n_list[4],
                  value = 20000,
                  min = 20000,
                  max = 900000,
                  step = 1000
      )}
  })

output$log_n5 <-
  renderUI({
    if(n_list[5] %in% input$log_nvar){   # creatinine
      sliderInput("log_nvar5",
                  n_list[5],
                  value = 0,
                  min = 0,
                  max = 10,
                  step = 0.1
      )}
  })

output$log_n6 <-
  renderUI({
    if(n_list[6] %in% input$log_nvar){   # sodium
      sliderInput("log_nvar6",
                  n_list[6],
                  value = 100,
                  min = 100,
                  max = 150,
                  step = 1
      )}
  })

output$log_n7 <-
  renderUI({
    if(n_list[7] %in% input$log_nvar){   # time
      sliderInput("log_nvar7",
                  n_list[7],
                  value = 0,
                  min = 0,
                  max = 400,
                  step = 1
      )}
  })

output$log_c1 <-
  renderUI({
    if(c_list[1] %in% input$log_cvar){   # anaemia
      selectInput("log_cvar1",
                  c_list[1],
                  choices = c(0,1),
                  selected = 0
      )}
  })

output$log_c2 <-
  renderUI({
    if(c_list[2] %in% input$log_cvar){   # diabetes
      selectInput("log_cvar2",
                  c_list[2],
                  choices = c(0,1),
                  selected = 0
      )}
  })

output$log_c3 <-
  renderUI({
    if(c_list[3] %in% input$log_cvar){   # HT
      selectInput("log_cvar3",
                  c_list[3],
                  choices = c(0,1),
                  selected = 0
      )}
  })

output$log_c4 <-
  renderUI({
    if(c_list[4] %in% input$log_cvar){   # sex
      selectInput("log_cvar4",
                  c_list[4],
                  choices = c(0,1),
                  selected = 0
      )}
  })

output$log_c5 <-
  renderUI({
    if(c_list[5] %in% input$log_cvar){   # smoking
      selectInput("log_cvar5",
                  c_list[5],
                  choices = c(0,1),
                  selected = 0
      )}
    
  })

##############
# TREE panel #
##############

output$tree_n1 <-
  renderUI({
    if(n_list[1] %in% input$tree_nvar){   # age
      sliderInput("tree_nvar1",
                  n_list[1],
                  value = 0,
                  min = 0,
                  max = 150,
                  step = 1
      )}
  })

output$tree_n2 <-
  renderUI({
    if(n_list[2] %in% input$tree_nvar){   # CPK
      sliderInput("tree_nvar2",
                  n_list[2],
                  value = 0,
                  min = 0,
                  max = 10000,
                  step = 1
      )}
  })

output$tree_n3 <-
  renderUI({
    if(n_list[3] %in% input$tree_nvar){   # EF
      sliderInput("tree_nvar3",
                  n_list[3],
                  value = 0,
                  min = 0,
                  max = 100,
                  step = 1
      )}
  })

output$tree_n4 <-
  renderUI({
    if(n_list[4] %in% input$tree_nvar){   # platelets
      sliderInput("tree_nvar4",
                  n_list[4],
                  value = 20000,
                  min = 20000,
                  max = 900000,
                  step = 1000
      )}
  })

output$tree_n5 <-
  renderUI({
    if(n_list[5] %in% input$tree_nvar){   # creatinine
      sliderInput("tree_nvar5",
                  n_list[5],
                  value = 0,
                  min = 0,
                  max = 10,
                  step = 0.1
      )}
  })

output$tree_n6 <-
  renderUI({
    if(n_list[6] %in% input$tree_nvar){   # sodium
      sliderInput("tree_nvar6",
                  n_list[6],
                  value = 100,
                  min = 100,
                  max = 150,
                  step = 1
      )}
  })

output$tree_n7 <-
  renderUI({
    if(n_list[7] %in% input$tree_nvar){   # time
      sliderInput("tree_nvar7",
                  n_list[7],
                  value = 0,
                  min = 0,
                  max = 400,
                  step = 1
      )}
  })
output$tree_c1 <-
  renderUI({
    if(c_list[1] %in% input$tree_cvar){   # anaemia
      selectInput("tree_cvar1",
                  c_list[1],
                  choices = c(0,1),
                  selected = 0
      )}
  })

output$tree_c2 <-
  renderUI({
    if(c_list[2] %in% input$tree_cvar){   # diabetes
      selectInput("tree_cvar2",
                  c_list[2],
                  choices = c(0,1),
                  selected = 0
      )}
  })

output$tree_c3 <-
  renderUI({
    if(c_list[3] %in% input$tree_cvar){   # HT
      selectInput("tree_cvar3",
                  c_list[3],
                  choices = c(0,1),
                  selected = 0
      )}
  })

output$tree_c4 <-
  renderUI({
    if(c_list[4] %in% input$tree_cvar){   # sex
      selectInput("tree_cvar4",
                  c_list[4],
                  choices = c(0,1),
                  selected = 0
      )}
  })

output$tree_c5 <-
  renderUI({
    if(c_list[5] %in% input$tree_cvar){   # smoking
      selectInput("tree_cvar5",
                  c_list[5],
                  choices = c(0,1),
                  selected = 0
      )}
    
  })