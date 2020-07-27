# categorical variable list
c_list <- list("anaemia","diabetes","HT","sex","smoking")

# numerical variable list
n_list <- list("age","CPK","EF","platelets","creatinine","sodium","time")


tabItem_eda <- 
  tabItem(tabName = "eda",
          
          navbarPage("EDA",
            
            tabPanel("Categorical Variables",
                     
                     sidebarPanel(h4("Select Variables:"),
                                  
                                  selectInput("eda_c_var",
                                              "",
                                              choices = c_list,
                                              selected = c_list[1]
                                              ),
                                  
                     ),
                     
                     mainPanel(
                       h4(textOutput("eda_c_tb_title")), # Contingency table (title)
                       tableOutput("eda_c_tb"), # Contingency table
                       br(),
                       
                       h4(textOutput("eda_c_bar_title")), # Bar chart (title)
                       plotOutput("eda_c_bar"),  # Bar chart
                       column(12,
                              align = "right",
                              downloadButton("eda_c_save",
                                             "Save Plot")
                       )
                     )
                     
            ), # End of Categorical Variables
            
            tabPanel("Numerical Variables",
                     
                     sidebarPanel(h4("Select Variables:"),
                                  
                                  checkboxGroupInput("eda_n_var",
                                                     "",
                                                     choiceNames = n_list,
                                                     choiceValues = n_list,
                                                     selected = n_list
                                  )
                     ),
                     
                     mainPanel(
                       h4("Statistic Summaies for Selected Variables"), # Summary table (title)
                       tableOutput("eda_n_tb"), # Summary table
                       br(),
                       
                       h4("Correlation Analysis for Selected Variables"), # Correlation
                       plotOutput("eda_n_cor"),  # Correlation
                       column(12,
                              align = "right",
                              downloadButton("eda_n_save_cor",
                                             "Save Plot")
                              ),
                       br(),
                       
                       h4("Scatter Plot for All Variables"), # Scatter
                       plotlyOutput("eda_n_sc"),  # Scatter
                       br(),
                       
                       hr(),
                       selectInput("eda_n_bvar",
                                   "Select Variable:",
                                   choices = n_list,
                                   selected = n_list[1]
                       ),
                       h4(textOutput("eda_n_box_title")), # Box
                       plotOutput("eda_n_box"),  # Box
                       column(12,
                              align = "right",
                              downloadButton("eda_n_save_box",
                                             "Save Plot")
                       )
                     )
                     
            ) # End of Numerical Variables
            
          ) # End of navbar
  )


#############################
#   Categorical Variables   #
#############################

# Contingency table

output$eda_c_tb_title <-
  renderText(
    paste("Contingency Table for", input$eda_c_var ,"and Survive")
  )

output$eda_c_tb <-
  renderTable({
    
    eda_tmp_tb <- table(pull(data[input$eda_c_var]),data$survive)
    
    eda_tmp_tb <- as.data.frame.matrix(eda_tmp_tb)
    
    rownames(eda_tmp_tb) <- paste0(input$eda_c_var,c(" = 0"," = 1"))
    colnames(eda_tmp_tb) <- paste0("survive",c(" = 0"," = 1"))
    
    eda_tmp_tb
    
  },rownames = TRUE)


# Bar chart

output$eda_c_bar_title <-
  renderText(
    paste("Bar Chart for", input$eda_c_var)
  )

bar_plt <- reactive({
  
  ggplot(data = factorData) + 
    geom_bar(aes(x=eval(as.name(input$eda_c_var)),fill=survive),alpha=0.8) +
    scale_fill_brewer(palette="Set1") +
    labs(x=input$eda_c_var) +
    theme_minimal()
  
})

output$eda_c_bar <-
  renderPlot(
    bar_plt()
  )



#############################
#    Numerical Variables    #
#############################


# prevent EMPTY selections
observe({
  if(length(input$eda_n_var) < 2){
    updateCheckboxGroupInput(session, "eda_n_var", selected= n_list)
  }
})


# Summary table

output$eda_n_tb <- 
  renderTable({
    data %>% 
      select(input$eda_n_var) %>%
      apply(2,FUN=function(x){summary(x)})
  },rownames = TRUE)

# Correlation plot

cor_plt <- reactive({
  
  n_data <- 
    data %>% 
    select(input$eda_n_var)
  
  ggcorr(n_data,label = T, label_size = 3, angle= -30, label_round = 2,size=3)
  
})

output$eda_n_cor <- 
  renderPlot(
    cor_plt()
  )


# Scatter plot

sc_plt <- reactive({
  
  n_data <- 
    data %>% 
    select(age,CPK,EF,platelets,creatinine,sodium,time,survive)
  
  # prepare data for scatter plot
  n_data$death <- as.factor(1-n_data$survive)
  
  # define color
  pl_colorscale = list(c(0.0, '#119dff'),
                       c(0.5, '#119dff'),
                       c(0.5, '#ef553b'),
                       c(1, '#ef553b'))
  
  # define axis
  axis = list(showline=FALSE,
              zeroline=FALSE,
              gridcolor='#ffff',
              ticklen=4,
              titlefont=list(size=13))
  
  # points in scatterplot Matrix
  fig <- n_data %>%
    plot_ly() 
  fig <- fig %>%
    add_trace(
      type = 'splom',
      dimensions = list(
        list(label='age', values=~age),
        list(label='CPK', values=~CPK),
        list(label='EF', values=~EF),
        list(label='platelets', values=~platelets),
        list(label='creatinine', values=~creatinine),
        list(label='sodium', values=~sodium),
        list(label='time', values=~time)
      ),
      text=~factor(death, labels=c("survive","dead")),
      diagonal=list(visible=F),
      marker = list(
        color = ~death,
        colorscale = pl_colorscale,
        size = 5,
        line = list(
          width = 1,
          color = 'rgb(230,230,230)'
        )
      )
    ) 
  
  # backgroup of scatterplot Matrix
  fig <- fig %>%
    layout(
      title = "Scatterplot Matrix",
      hovermode='closest',
      dragmode = 'select',
      plot_bgcolor='rgba(240,240,240, 0.95)',
      xaxis=list(domain=NULL, showline=F, zeroline=F, gridcolor='#ffff', ticklen=4, titlefont=list(size=13)),
      yaxis=list(domain=NULL, showline=F, zeroline=F, gridcolor='#ffff', ticklen=4, titlefont=list(size=13)),
      xaxis2=axis,
      xaxis3=axis,
      xaxis4=axis,
      xaxis5=axis,
      xaxis6=axis,
      xaxis7=axis,
      yaxis2=axis,
      yaxis3=axis,
      yaxis4=axis,
      yaxis5=axis,
      yaxis6=axis,
      yaxis7=axis
    )%>% style(showupperhalf = F)
  
  # show the figure
  fig
  
})

output$eda_n_sc <- 
  renderPlotly(
    sc_plt()
  )



# Box plot

output$eda_n_box_title <-
  renderText(
    paste("Boxplot for", input$eda_n_bvar)
  )


box_plt <- reactive({
  
  ggplot(data = factorData,aes(x=survive,y=eval(as.name(input$eda_n_bvar)),color=survive))+
    geom_violin(trim=FALSE,alpha=0.4)+
    geom_point(alpha=0.5,position = "jitter")+
    scale_colour_brewer(palette="Set1")+
    labs(y=input$eda_n_bvar)+ 
    theme_minimal()
  
})

output$eda_n_box <- 
  renderPlot(
    box_plt()
  )


#############################
#          Download         #
#############################

# save Bar chart
output$eda_c_save <-
  downloadHandler(
    filename = function(){paste0("Bar chart for ", input$eda_c_var, ".png")},
    content = function(file){
      ggsave(file,bar_plt())
    }
  )

# save correlation
output$eda_n_save_cor <-
  downloadHandler(
    filename = "Correlation.png",
    content = function(file){
      ggsave(file,cor_plt())
    }
  )


# save boxplot
output$eda_n_save_box <-
  downloadHandler(
    filename = function(){paste0("Boxplot for ", input$eda_n_bvar, ".png")},
    content = function(file){
      ggsave(file,box_plt())
    }
  )
