load("KrogerHouseholdsCarbs.RData")

library(shiny)

ui <- fluidPage(

    # Application title
    titlePanel("Kroger Household Analysis"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            selectInput(inputId = "locationvariable", label = "Select City(s) to Analysis", 
                        choices = unique(HOUSE$City), selected = c("Alcoa", "Knoxville"), 
                        multiple = TRUE),
            
            radioButtons(inputId = "quantityvariable", label = "Choose Quantity to Study by City", 
                         choiceNames = c("Total Items", "Unique Items", "Number of Stores", "Total Spent"),
                         choiceValues = c("TotalItems", "UniqueItems", "NumberOfStores", "TotSpent"), 
                         selected = "TotSpent"),
            
            checkboxInput(inputId = "logvalue", 
                          label = "Display Quantity Studied on Logscale?"),
            
            radioButtons(inputId = "yaxisvariable", 
                         label = "Choose Vertical Axis for Scatterplot", 
                         choiceNames = c("Pancake", "Pasta", "Sauce", "Syrup"), 
                         choiceValues = c("PancakeMoney", "PastaMoney", "SauceMoney", "SyrupMoney"),
                         selected = "SauceMoney"),
            
            radioButtons(inputId = "xaxisvariable", 
                         label = "Choose Horizontal Axis for Scatterplot", 
                         choiceNames = c("Pancake", "Pasta", "Sauce", "Syrup"), 
                         choiceValues = c("PancakeMoney", "PastaMoney", "SauceMoney", "SyrupMoney"),
                         selected = "PastaMoney"),
            
            checkboxInput(inputId = "ylogvalue",label = "Log Scale y variable on scatterplot?"),
            
            checkboxInput(inputId = "xlogvalue",label = "Log Scale x variable on scatterplot?")
            
        ),
            
        mainPanel(
            
            plotOutput("quantityplot"),
            textOutput("narration"),
            verbatimTextOutput("avgprice"),
            plotOutput("scatterplot")
            
        )
    )
)

server <- function(input, output) {
    
    output$scatterplot <- renderPlot({
        SUB <- droplevels( subset(HOUSE, City %in% input$locationvariable) )
        
        if( (input$xlogvalue == TRUE) & (input$ylogvalue == TRUE) ) { plot(formula(paste(input$yaxisvariable, "~", input$xaxisvariable)), data=SUB, log ="xy")}
        
        else if(input$ylogvalue == TRUE) { plot(formula(paste(input$yaxisvariable, "~", input$xaxisvariable)), data=SUB, log ="y")}
        
        else if(input$xlogvalue == TRUE) {plot(formula(paste(input$yaxisvariable, "~", input$xaxisvariable)), data=SUB, log ="x")}
        
        else{ plot(formula(paste(input$yaxisvariable, "~", input$xaxisvariable)), data=SUB)}
    })
    
    output$narration <- renderPrint({
        cat( paste("Total Spent for each City") ) 
    })
        
    output$avgprice <- renderPrint({
        SUB <- droplevels( subset(HOUSE, City %in% input$locationvariable) )
        y <- SUB$TotSpent
        x <- SUB[,"City"]
        if( ( class(x) == "numeric" | class(x) == "integer" ) ) { x <- discretize(x,method="frequency") }
        A <- aggregate(y~x,FUN=sum)
        names(A) <- c("City","Total Spent")
        A
    })
    
    output$quantityplot <- renderPlot({
        SUB <- droplevels( subset(HOUSE, City %in% input$locationvariable) )
        if(input$logvalue == TRUE ) { plot(formula(paste(input$quantityvariable, "~ City")), data = SUB, log = "y") } 
        else { plot(formula(paste(input$quantityvariable, "~ City")), data = SUB) }
        
    })
}

# Run the application 
# For some reason I could only get it to work if you rerun the library(shiny)
library(shiny)
shinyApp(ui = ui, server = server)
