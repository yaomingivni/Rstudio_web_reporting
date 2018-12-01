library(shiny)
library(ggplot2)
library(markdown)
ui <- navbarPage("Menu",
             tabPanel("Plot",
                      sidebarLayout(
                        sidebarPanel(
                          radioButtons("plotType", "Plot type",
                                       c("Scatter"="p", "Line"="l")
                          )
                        ),
                        mainPanel(
                          plotOutput("plot")
                        )
                      )
             ),
             tabPanel("Summary",
                      verbatimTextOutput("summary")
             ),
            tabPanel("Table",
                      DT::dataTableOutput("table")
            )
             
  )

  
  #DT::dataTableOutput("table")

 
    

  


server <- function(input, output){
  

  #  MyData <- read.csv(file="D:/Utilisateur/Documents/projet_dataviz/Reporting_web_rstudio/Rstudio_web_reporting/data/hadopi_chiffres_bruts.csv", header=TRUE, sep=";")
  
  
    
  MyData <- read.csv(file="data/hadopi_chiffres_bruts.csv", header=TRUE, sep=";")
  output$table <- DT::renderDataTable(DT::datatable({
    data <- MyData
  }))
  
  output$plot <- renderPlot({
    max_y <- max(MyData$delib_mensuel)
  
    plot_color <- c("blue", "red", "green")
    plot(MyData$delib_mensuel, type= input$plotType, col= plot_color[1], ylim = c(0,max_y), axes = FALSE, ann= FALSE)
  # lines(MyData$X2e_reco_mois, type = input$plotType, col=plot_color[2])
  #  lines(MyData$delib_mensuel, type = input$plotType, col=plot_color[3])
    
    axis(1, lab= MyData$annee_mois, at=1:86)
    axis(2, las = 1, at= 10*0:max_y)
    
  })
  
  output$summary <- renderPrint({
    summary(MyData)
  })
  
}
  
shinyApp(ui = ui, server = server)