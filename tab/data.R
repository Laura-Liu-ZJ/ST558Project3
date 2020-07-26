tabItem_data <- 
  tabItem(tabName = "data",
          fluidRow(
            
            sidebarPanel(
              h4("Select Variables:"),  # Selection panel
              
              checkboxGroupInput("data_var","",
                                 choiceNames =
                                   as.list(colnames(data)),
                                 choiceValues =
                                   as.list(colnames(data)),
                                 selected = 
                                   as.list(colnames(data))
              ),
              
              br(),
              
              downloadButton("data_download",  # Download button
                             "Download")
              
            ),
            
            mainPanel(
              dataTableOutput("data_dt")  # Data table
              
            )
            
            
          ))


dataInput <- reactive({
  data %>% 
    select(input$data_var)
})


output$data_dt <-
  renderDataTable(
    dataInput()
  )

output$data_download <-
  downloadHandler(
    filename = "heart_failure_clinical_records_dataset.csv",
    content = function(file){
      write.csv(dataInput(), file, row.names = FALSE)
    }
    
  )