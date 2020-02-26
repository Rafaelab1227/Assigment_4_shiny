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
data("ThreeCars2017")
data=ThreeCars2017

headerPanel <- titlePanel("Three cars 2017")
    
sumPanel <- tabPanel("Summary",
             fluidRow(
                 column(6,
                        h4("This is a dataset part of the package *Stat2Data* from R, that contains information about 90 cars from a web page in the state of Ohio.")
                 ),
                 column(3,
                        img(class="img-polaroid",
                            src="http://s3.caradvice.com.au/wp-content/uploads/2017/04/2017-toyota-yaris-v-mazda-2-comparison-41.jpg", height="100%", width="100%")))
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
                                

ui <-navbarPage("Shiny App",
                headerPanel, 
                sumPanel,
                plotPanel
)

                        
# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlotly({
        # generate bins based on input$bins from ui.R
        d <- data[data$Price>=input$Price[1]&data$Price<=input$Price[2],]
        
        plot_ly(d, x = ~Age, y = ~Mileage, text = ~paste("Mileage: ", Mileage, '$<br>Age:', Age),
            color = ~Price, size = ~Price)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
