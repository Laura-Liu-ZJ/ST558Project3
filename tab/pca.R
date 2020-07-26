# numerical variable list
n_list <- list("age","CPK","EF","platelets","creatinine","sodium","time")


tabItem_pca <- 
  tabItem(tabName = "pca",
          
          navbarPage("PCA",
            
            tabPanel("Introduction",
                     
                     div(includeMarkdown("info/pca.md"), 
                         align = "justify")
                     
            ), # End of Introduction
            
            tabPanel("Analysis",
                     
                     sidebarPanel(h4("Select Variables:"),
                                  
                                  checkboxGroupInput("pca_var",
                                                     "",
                                                     choiceNames = n_list,
                                                     choiceValues = n_list,
                                                     selected = n_list
                                  ),
                                  
                                  br(),
                                  sliderInput("pca_num_pc",
                                              "How many PCs do you want?",
                                              min = 1,
                                              max = 7,
                                              value = 1),
                                  br(),
                                  textInput("pca_num_3d",
                                            "Which PCs do you want to look at 3D PCA plot? (eg. 1,2,3)",
                                            value = "1,2,3"),
                                  br(),
                                  textInput("pca_num_2d",
                                            "Which PCs do you want to look at 2D PCA plot? (eg. 1,2)",
                                            value = "1,2"),
                                  br(),
                                  column(12,
                                         align = "right",
                                         actionButton("pca_submit",
                                                        "Submit")
                                  )
                                  
                                  ),
                     
                     mainPanel(
                       h4("PCA Importance"), # summary
                       verbatimTextOutput("pca_sum"), # summary
                       h4("Variances Plot"), # summary
                       plotOutput("pca_plt_sc"),
                       column(12,
                              align = "right",
                              downloadButton("pca_save_var",
                                                "Save Plot")
                              ),
                       br(),
                       
                       h4("Rotation"), # rotation
                       tableOutput("pca_ro"), # rotation
                       br(),
                       
                       h4("PCA 3D Plot"), # 3d plot
                       rglwidgetOutput("pca_plt_3d"), # 3d plot
                       column(12,
                              align = "right",
                              actionButton("pca_save_3d",
                                             "Save Plot")
                       ),
                       br(),
                       
                       h4("PCA 2D Plot"), # 2d plot
                       plotOutput("pca_plt_2d"), # 2d plot
                       column(12,
                              align = "right",
                              downloadButton("pca_save_2d",
                                             "Save Plot")
                       ),
                     )
                     
            ) # End of Analysis
            
          ) # End of navbar
  )

# Update sliderinput

observe({updateSliderInput(session, "pca_num_pc", max = length(input$pca_var))})

# prevent EMPTY selections
observe({
  if(length(input$pca_var) < 3){
    updateCheckboxGroupInput(session, "pca_var", selected= n_list)
  }
})



# PCA function
pca <- reactive({
  n_data  <- data %>% select(input$pca_var)
  prcomp(n_data,scale = T)
})


observe({
  if (input$pca_submit>0) {




# summary table
output$pca_sum <-
  renderPrint({
    output <- pca()
    summary(output)
  })


# variance plot
plt_sc <- reactive({
  output <- pca()
  screeplot(output,type="lines",col="#990000",lwd=2)
})

output$pca_plt_sc <-
  renderPlot({
    plt_sc()
  })

output$pca_save_var <-
  downloadHandler(
    filename = function(){paste0("PCA Variance.png")},
    content = function(file){
      ggsave(file,plt_sc())
    }
  )

# rotation

output$pca_ro <- 
  renderTable({
    output <- pca()
    round(output$rotation[,1:as.numeric(input$pca_num_pc)],3)
  })

# 3d plot

plt_3d <- reactive({
  output <- pca()
  try(rgl.close())
  # pca3d(output,components = c(1,2,3),group=data$survive, legend='right')
  commands.3d <- paste("pca3d(output,components = c(", noquote(input$pca_num_3d), "),group=data$survive, legend='right')")
  eval(parse(text=commands.3d))
  rglwidget()
})

output$pca_plt_3d <- 
  renderRglwidget({
    plt_3d()
  })

observe({
  if(input$pca_save_3d > 0){
   isolate(snapshotPCA3d(file="PCA 3d.png")) 
  }
})



# 2d plot

plt_2d <- reactive({
  output <- pca()
  # pca2d(output,components = c(1,2),group=data$survive,biplot = T, biplot.vars = 7, legend='right')
  commands.2d <- paste("pca2d(output,components = c(", noquote(input$pca_num_2d), "),group=data$survive,biplot = T, biplot.vars = 7, legend='right')")
  eval(parse(text=commands.2d))
  
})

output$pca_plt_2d <- 
  renderPlot({
    plt_2d()
  })

output$pca_save_2d <-
  downloadHandler(
    filename = function(){paste0("PCA 2d.png")},
    content = function(file){
      ggsave(file,plt_2d())
    }
  )



  } else {}
})