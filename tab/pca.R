# numerical variable list
n_list <- list("age","CPK","EF","platelets","creatinine","sodium","time")


tabItem_pca <- 
  tabItem(tabName = "pca",
          
          navbarPage("PCA",
                     
                     tabPanel("Introduction",
                              withMathJax(),
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
                                           ) # end of checkbox
                              ), # end of sidebarpanel
                              
                              mainPanel(
                                #--------------------------------
                                h4("PCA Importance"), # summary
                                verbatimTextOutput("pca_sum"), # summary
                                h4("Variances Plot"), # variance plot
                                plotOutput("pca_plt_sc"),
                                column(12,
                                       align = "right",
                                       downloadButton("pca_save_var",
                                                      "Save Plot"),
                                       hr(),
                                ),
                                
                                #--------------------------------
                                h4("Rotation"), # rotation
                                sliderInput("pca_num_pc",
                                            "How many PCs do you want?",
                                            min = 1,
                                            max = 7,
                                            value = 7),
                                tableOutput("pca_ro"), # rotation
                                
                                #--------------------------------
                                column(12,
                                       hr(),
                                       column(6,
                                              h4("PCA 3D Plot"), # 3d plot
                                              column(10,
                                                     textInput("pca_num_3d",
                                                               "Select PCs: (eg. 1,2,3)",
                                                               value = "1,2,3"
                                                     )
                                              ),
                                              column(2,
                                                     actionButton("pca_submit_1",
                                                                  "Refresh",
                                                                  class = "btn btn-primary")
                                              ),
                                              # plot panel
                                              conditionalPanel(
                                                condition = "input.pca_submit_1 > 0",
                                                rglwidgetOutput("pca_plt_3d", width = "400", height = "400") # 3d plot
                                              ),
                                              conditionalPanel(
                                                condition = "input.pca_submit_1 == 0",
                                                rglwidgetOutput("pca_plt_3d_default", width = "400", height = "400") # 3d plot
                                              )
                                       ),                                
                                       
                                       #--------------------------------
                                       column(6,
                                              h4("PCA 2D Plot"), # 2d plot
                                              column(10,
                                                     textInput("pca_num_2d",
                                                               "Select PCs: (eg. 1,2)",
                                                               value = "1,2"
                                                     )
                                              ),
                                              column(2,
                                                     actionButton("pca_submit_2",
                                                                  "Refresh",
                                                                  class = "btn btn-primary"),
                                                     
                                                     
                                              ),
                                              # plot panel
                                              conditionalPanel(
                                                condition = "input.pca_submit_2 > 0",
                                                
                                                plotOutput("pca_plt_2d", width = "400", height = "400"), # 2d plot
                                              ),
                                              # default plot
                                              conditionalPanel(
                                                condition = "input.pca_submit_2 == 0",
                                                
                                                plotOutput("pca_plt_2d_default", width = "400", height = "400") # 2d plot
                                                
                                              )
                                       ), # End of 2d plot
                                       
                                       div(style = "height:550px;") # change the height for plotting
                                ), # End of Plots
                                column(12,
                                       align = "right",
                                       downloadButton("pca_save_2d",
                                                      "Save Plot")
                                       ),
                                hr()
                                
                              ) # End of mainpanel
                              
                     ) # End of Analysis
                     
          ) # End of navbar
  )

# Update sliderinput

observe({updateSliderInput(session, 
                           "pca_num_pc", 
                           max = length(input$pca_var), 
                           value = length(input$pca_var)
)
})

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
    output <- pca()
    screeplot(output,type="lines",col="#990000",lwd=2)
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
    if (req(input$pca_submit_1)>0) { 
      isolate(plt_3d())
    }
  })


# default plot
output$pca_plt_3d_default <- 
  renderRglwidget({
    isolate(output <- pca())
    try(rgl.close())
    pca3d(output,components = c(1,2,3),group=data$survive, legend='right')
    rglwidget()
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
    isolate(output <- pca())
    if (req(input$pca_submit_2)>0) { 
      isolate(commands.2d <- paste("pca2d(output,components = c(", noquote(input$pca_num_2d), "),group=data$survive,biplot = T, biplot.vars = 7, legend='right')"))
      eval(parse(text=commands.2d))
    }
  })

# default plot
output$pca_plt_2d_default <- 
  renderPlot({
    isolate(output <- pca())
    pca2d(output,components = c(1,2),group=data$survive,biplot = T, biplot.vars = 7, legend='right')
  })

output$pca_save_2d <-
  downloadHandler(
    filename = function(){paste0("PCA 2d.png")},
    content = function(file){
      png(file)
      plt_2d()
      dev.off()
    }
  )






