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
data("ThreeCars2017")
data=ThreeCars2017

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Three cars 2017"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("Price",
                        "Select price range:",
                        min = min(data$Price),
                        max = max(data$Price),
                        value=c(min(data$Price), max(data$Price)))
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotlyOutput("distPlot")
        )
    )
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
