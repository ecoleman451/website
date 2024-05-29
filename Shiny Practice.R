#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Amount Of test taken"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(sliderInput("bins", "Number of Tests:", min = 1,max = 100,value = 30),
                     sliderInput("A", "P( Having the condition ):", min = 0, max = 1,value = 0.2, step = 0.01),
                     sliderInput("sens", "Sensitivity: P( + | Having the condition )", min = 0, max = 1,value = 0.7, step = 0.01),
                     sliderInput("spec", "Specificity: P( - | Not having the condition )", min = 0, max = 1, value = 0.9, step = 0.01)
            
        ),
        
        
        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        #x <- faithful[, 2]
        n <- input$bins
        x<-0:n
        y<-n-x
        x1<-dbinom(0:n,n,input$sens)
        x2<-dbinom(0:n,n,(1-input$spec))
        lim<-max(c(x1,x2))
        p <- ((input$sens)^x)*((1-input$sens)^y)*(input$A) / (((input$sens)^x)*((1-input$sens)^y)*(input$A) + (((1-input$spec))^x)*(((input$spec))^y)*(1-input$A))
        n1<-sum(p>1/2)
        colors<-c(rep("green",n-n1+1), rep("red",n1))
        
        plot(x,p,type="b", xlab="Number of Positive Results (k)", main="Likelihood of Having Covid-19 relative to Number of Tests Taken", ylab="Scaled Probabilities", col="grey")
        
        lines(x,p,type="p", col=colors)
        
        barplot(x1/max(x1), col="red", add=TRUE)
        barplot( x2/max(x2), col="green", add=TRUE)
        
        
        # draw the histogram with the specified number of bins
        #hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
