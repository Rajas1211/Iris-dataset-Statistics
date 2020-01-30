#Rajas Godbole
# rajasgodbole2018@gmail.com

library(shinydashboard)
data("iris")
list=names(iris)
list

ui <- dashboardPage(
    dashboardHeader(title = "Statistical Analysis for Iris Dataset"),
    dashboardSidebar(
        sidebarMenu(
            #Creating tabs
            
            menuItem("Data", tabName = "Iris",icon = icon(lib="glyphicon","hand-right")),
            menuItem("Descriptive Analysis", tabName = "Descriptive",icon = icon(lib="glyphicon","hand-right")),
            menuItem("Probability of Random Variable", tabName = "prob",icon = icon(lib="glyphicon","hand-right")),
            menuItem("Regression Model", tabName = "glm",icon = icon(lib="glyphicon","hand-right"))
        )
    ),
    dashboardBody(
        tabItems(
            
            
            
            ################################################################################################### tab 1 content
            #Tab: Data
            #Displays the head of data
            tabItem(tabName = "Iris",
                    h2("Iris Data Set"),
                    
                    mainPanel(
                        tableOutput("contents")
                        
                    )
            ),
            ######################################################################################################Tab 2 contents
            #Tab : Descriptive Analysis
            #Creates Histogram of the selected attribute for visual analysis
            tabItem(tabName = "Descriptive",
                    h2("Descriptive Analysis"),
                    fluidPage(
                        sidebarLayout(
                            sidebarPanel(
                                
                                
                                selectInput("input_desp", label = h3("Attribute"),
                                            choices = list("Sepal.Length"="Sepal.Length",
                                                           "Sepal.Width"="Sepal.Width",
                                                           "Petal.Length"="Petal.Length",
                                                           "Petal.Width"="Petal.Width"
                                            )
                                            , selected = 1),
                                
                                
                                
                            ),
                            
                            mainPanel(
                                
                                
                                box(plotOutput("plot"),width = 250),
                                box(verbatimTextOutput("Summary"),width = 250),
                            )
                            
                        )
                    )
                    
                    
            ),        
            
            
            ###################################################################################################Summary
            # Tab:Probability of Random Variable
            #Displays the Probaility of selected attribute value
            tabItem(tabName = "prob",
                    h2("Probability of Random Variable"),
                    fluidPage(
                        sidebarLayout(
                            sidebarPanel(
                                
                                
                                selectInput("pro", label = h3("Attribute"),
                                            choices = list("Sepal.Length"="Sepal.Length",
                                                           "Sepal.Width"="Sepal.Width",
                                                           "Petal.Length"="Petal.Length",
                                                           "Petal.Width"="Petal.Width"                                               )
                                            , selected = 1),
                                numericInput("rn","Random Variable",value = 4)
                                
                                
                                
                            ),
                            
                            mainPanel(
                                fluidPage(
                                    
                                    box(verbatimTextOutput("prob1"),br(),
                                        verbatimTextOutput("prob2"),br(),
                                        verbatimTextOutput("prob3"),br(),
                                        verbatimTextOutput("prob4"),width=1000)
                                ))
                            
                        )
                    )
                    
                    
            ),
            
            ###################################################################################################
            #Tab: Regression Model
            #Uses Linear Regression using two variable. Also displays the summary and Histogram
            tabItem(tabName = "glm",
                    h2("Regression Model"),
                    fluidPage(
                        sidebarLayout(
                            sidebarPanel(
                                selectInput("outcome", label = h3("Outcome"),
                                            choices = list("Sepal.Length"="Sepal.Length",
                                                           "Sepal.Width"="Sepal.Width",
                                                           "Petal.Length"="Petal.Length",
                                                           "Petal.Width"="Petal.Width")
                                            , selected = 1),
                                
                                selectInput("indepvar", label = h3("Explanatory variable"),
                                            choices = list("Sepal.Length"="Sepal.Length",
                                                           "Sepal.Width"="Sepal.Width",
                                                           "Petal.Length"="Petal.Length",
                                                           "Petal.Width"="Petal.Width",
                                                           "Species"="Species")
                                            , selected = 1)
                                
                                
                            ),
                            
                            mainPanel(
                                
                                tabsetPanel(type = "tabs",
                                            
                                            tabPanel("Scatterplot", plotOutput("scatterplot")), # Plot
                                            tabPanel("Distribution", # Plots of distributions
                                                     fluidRow(
                                                         column(6, plotOutput("distribution1")),
                                                         column(6, plotOutput("distribution2")))
                                            ),
                                            tabPanel("Model Summary", verbatimTextOutput("summary")), # Regression output
                                            tabPanel("Data", DT::dataTableOutput('tbl')) # Data as datatable
                                            
                                )
                            )
                        ))
                    
                    
            )
            ###################################################################################################
        )
    )
)

server <- function(input, output) {
    ##########################################################################################################
    #Tab: Data
    
    #Displays head of the data in the first tab
    output$contents <- renderTable(head((iris),10))
    ##########################################################################################################
    #Tab: Descriptive Analysis
    
    #Plots distogram for the selected attribute in Second Tab
    output$plot <- renderPlot({
        
        hist(iris[,input$input_desp], breaks = 50, col = "#75AADB", border = "white",
             main = "Histogram")
        
        
    })
    
    #Displays Summary i.e mean, median, etc in the second tab
    output$Summary <- renderPrint(
        summary(iris[,input$input_desp])
    )
    ##############################################################################RAndom Variable
    #Tab: Probability of Random Variable
    
    #Displays the range of the selected attribute
    output$prob1 <- renderText( paste("Minimum",
                                      (input$pro),
                                      "is",
                                      (min(iris[,input$pro])),
                                      "Maximum",
                                      (input$pro),
                                      "is",
                                      (max(iris[,input$pro]))
    )
    )
    
    #Gives the probability of getting user-given value 
    #- assumes distribution to be normally distributed
    
    output$prob2 <- renderText( paste("Probability of getting ",
                                      (input$pro),
                                      "as",
                                      (input$rn),
                                      "is",
                                      round(dnorm(input$rn, mean= mean((iris[,input$pro])), sd= sd((iris[,input$pro]))),digits=6)
    )
    )
    
    #Gives the probability of getting values below the user-given value 
    #- assumes distribution to be normally distributed
    output$prob3 <- renderText( paste("Probability of getting ",
                                      (input$pro),
                                      "below",
                                      (input$rn),
                                      "is",
                                      round(pnorm(input$rn, mean= mean((iris[,input$pro])), sd= sd((iris[,input$pro]))),digits = 6)
    )
    )
    #Gives the probability of getting values above the user-given value 
    #- assumes distribution to be normally distributed
    
    output$prob4 <- renderText( paste("Probability of getting ",
                                      (input$pro),
                                      "above",
                                      (input$rn),
                                      "is",
                                      round(pnorm(input$rn, mean= mean((iris[,input$pro])), sd= sd((iris[,input$pro])),lower.tail=FALSE),digits = 6)
    )
    )
    
    ##############################################################################
    # Tab: Regression Model
    
     # Scatterplot output
    output$scatterplot <- renderPlot({
        plot(iris[,input$indepvar], iris[,input$outcome], main="Scatterplot",
             xlab=input$indepvar, ylab=input$outcome, pch=19)
        abline(lm(iris[,input$outcome] ~ iris[,input$indepvar]), col="red")
        lines(lowess(iris[,input$indepvar],iris[,input$outcome]), col="blue")
    }, height=400)
    
    
    # Histogram output var 1
    output$distribution1 <- renderPlot({
        hist(iris[,input$outcome], main="", xlab=input$outcome)
    }, height=300, width=300)
    
    # Histogram output var 2
    output$distribution2 <- renderPlot({
        hist(iris[,input$indepvar], main="", xlab=input$indepvar)
    }, height=300, width=300)
    
    # Regression output
    output$summary <- renderPrint({
        fit <- lm(iris[,input$outcome] ~ iris[,input$indepvar])
        names(fit$coefficients) <- c("Intercept", input$var2)
        summary(fit)
    })
    
    # Data output
    output$tbl = DT::renderDataTable({
        DT::datatable(iris, options = list(lengthChange = FALSE))
    })
}

shinyApp(ui, server)

