
library(shiny)

shinyUI(fluidPage(

    titlePanel("Next word predictor"),
    
    sidebarLayout(
        sidebarPanel(
            
            # where user inserts text
            textInput("string", "Insert text here:"),
            
            # slider: nr predictions
            sliderInput("slider",
                        "Number of predictions",
                        min = 1, max = 5, value = 5),
            
            # submit button
            submitButton("Submit")
            
        ),

        mainPanel(
            tabsetPanel(type = "tabs",
                        
                        # 1st tab: prediction tables
                        tabPanel("Predicion", br(),
                                 
                                 # putting tables side by side
                                 fluidRow(
                                     # output both tables
                                     column(width=4, offset=1, tableOutput("table1") ),
                                     column(width=4, offset=1, tableOutput("table2") ) )
                                     
                                 ),
                        
                        # 2nd tab: documentation
                        tabPanel("Documentation", br(),
                                 textOutput("doc1"), br(),
                                 textOutput("doc2") )
                        )
        )
    )
))
