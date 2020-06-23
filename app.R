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
    titlePanel("Example GLM"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("obs",
                        "Number of Observations:",
                        min = 10,
                        max = 500,
                        value = 30),
            sliderInput("rho",
                        "Correlation between error terms:",
                        min = -1,
                        max = 1,
                        step = 0.1,
                        value = 0),
            sliderInput("bpr",
                        "Breakpoint ratio:",
                        min = 0,
                        max = 1,
                        step = 0.1,
                        value = 0),
            sliderInput("me",
                        "Multiplicativ effect:",
                        min = 0.1, 
                        max = 10,
                        step = 0.1,
                        value = 1),
            selectInput("mh",
                        "Multiplicativ Heteroskedasticity:",
                        list(FALSE, TRUE))
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
        set.seed(156857)
        N = input$obs
        rho = input$rho
        bpr = input$bpr
        me = input$me
        
        e = rnorm(N)
        beta = c(5)
        
        X = matrix(c(rep(1,N)),  ncol = 1)
        
        ## autoregression
        e2 = vector(length = N)
        e2[1] = e[1]
        for(i in 2:(N)) {
            e2[i] = rho * e2[i-1] + e[i]
        }
        e = e2
        
        ## breakpoint
        if (bpr != 0){
            bp = N*(bpr)
            e = c(e[1:(bp)], me * e[(bp+1):length(e)])
        }
        
        
        ## multiplicative Heteroskedasticity
        if( input$mh == TRUE) {
            e = e * seq(1,me, length.out = N)
        }
        
        
        
        y = X%*%beta+e
        
        ## standart Modell ----
        
        reg1 = lm(y ~ X[,1]-1)
        
        summary(reg1)
        
        
        resids = data.frame(obs = rep(1:N), resid = resid(reg1), group = rep(1:1, each = N))
        
        library(ggplot2)
        
        res = ggplot(resids, aes(obs , resid),            
                     xlab = "observations" ,
                     ylab = "resid") + 
            geom_point() +
            geom_hline(yintercept=0)
        
        res + facet_grid(cols = vars(group))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
