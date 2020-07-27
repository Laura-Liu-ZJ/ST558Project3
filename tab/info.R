tabItem_info <- 
  tabItem(tabName = "info",
          
            navbarPage("Info",
              
              tabPanel("Data Description",
                       
                       div(includeMarkdown("info/data.md"), 
                           align = "justify")
                       
              ), # End of Data Description
              
              tabPanel("Application Description",
                       
                       div(includeMarkdown("info/app.md"),
                           align = "justify")
                       
              ) # End of App Description
            
          ) # End of navbar
  )
