#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(Stat2Data)
library(plotly)
library(markdown)
library(shinythemes)

data("ThreeCars2017")
data=ThreeCars2017
models = unique(data$CarType)

sumPanel <- tabPanel("Summary",
             fluidRow(
                 column(6,
                        h1("Three cars 2017 dataset"),
                        h4("This is a dataset part of the package Stat2Data from R, that contains information about 90 cars from a web page in the state of Ohio."),
                        h4("The variables of the dataset are:"),
                        uiOutput("myList"),
                        uiOutput("tab")
                        #summary(data)
                 ),
                 column(3,
                        img(class="img-polaroid",
                            src="http://s3.caradvice.com.au/wp-content/uploads/2017/04/2017-toyota-yaris-v-mazda-2-comparison-41.jpg", height="200%", width="200%")))
)
plotPanel <- tabPanel("Price",
                        sidebarPanel(
                            sliderInput("Price",
                                        "Select price range:",
                                        min = min(data$Price),
                                        max = max(data$Price),
                                        value=c(min(data$Price), max(data$Price))
                                        )
                        ), mainPanel(
                            h4("We can see the characteristics of the cars that were available on February 2017"),
                            plotlyOutput("distPlot")
                        )
                        
)   

modelPanel <- tabPanel("Model",
                      sidebarPanel(
                          selectInput("Model",
                                      "Select model/s:",
                                      multiple=TRUE,
                                      choices=models,
                                      selected=head(models,3)
                          )
                      ), mainPanel(
                          h4("We can see the characteristics of the cars based on their model"),
                          plotlyOutput("distPlot2")
                      )
                      
)   


ui <-fluidPage(theme = shinytheme("cerulean"),
               navbarPage("Shiny App - Three cars",
                          sumPanel,
                          plotPanel,
                          modelPanel
               )) 

                        
# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlotly({
        d <- data[data$Price>=input$Price[1]&data$Price<=input$Price[2],]
        
        plot_ly(d, x = ~Age, y = ~Mileage, text = ~paste("Mileage: ", Mileage, '$<br>Age:', Age),
            color = ~Price, size = ~Price)
    })
    
    url <- a("Stat2Data v2.0.0", href="https://www.rdocumentation.org/packages/Stat2Data/versions/2.0.0")
    output$tab <- renderUI({
        tagList("For more information about this dataset and others go to:", url)
    })
    
    output$myList <- renderUI(HTML("<ul><li>CarType: model</li>
                                   <li>Age: years passed since production</li>
                                   <li>Price: price ask by the owners, expressed in $1,000</li>
                                   <li>Mileage: mileage in 1000</li>
                                   </ul>"))
    output$distPlot2 <- renderPlotly({
        d <- data[data$CarType%in%input$Model,]
        
        plot_ly(d, x = ~Age, y = ~Price, z = ~Mileage, color = ~CarType, showticklabels = F,text = ~paste("Price", Price,'$<br>Age:', Age,"$<br>Mileage: ", Mileage)) %>%
            add_trace(showlegend = FALSE) %>%    
            layout(scene = list(xaxis = list(title = 'Age from production'),
                                                yaxis = list(title = 'Price in $1,000'),
                                                zaxis = list(title = 'Mileage in 1,000')))

    })
}

# Run the application 
shinyApp(ui = ui, server = server)
