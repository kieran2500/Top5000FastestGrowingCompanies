library(shiny)
inc5000 <- read.csv("inc5000.csv")

ui<- 
  navbarPage(
    title = "Top 5000 Fastest Growing Companies",
      
    column(4,
           selectInput("ind",
                       "Industry:",
                       c("All",sort(
                         unique(as.character(inc5000$INDUSTRY)))))
    ),

    column(4,
           selectInput("state",
                       "State:",
                       c("All", sort(
                         unique(as.character(inc5000$STATE)))))
    ),
    
    column(4,
           selectInput("found",
                       "Year Founded:",
                       c("All", sort(na.last = TRUE, 
                                     unique(as.character(inc5000$FOUNDED)), ))
           )
    ),
    tabPanel("DataTable",
             # Create a new Row in the UI for select Inputs
             
    fluidPage(

      # Create a new row for the table.
      DT::dataTableOutput("table"),
     
    )),
  tabPanel("Year Established",
           fluidRow(
            
    div(plotOutput("hist", height = "800px"), align = "center"))),
  
  tabPanel("Industry Percentages",
       fluidPage(column(10, plotOutput("pie", height = "800px")))),
    

tabPanel("About",
         fluidPage( fluidRow("Investors are always looking to get a large return on their investment and often try using many different ways to try to minimize risk and maximize reward. Some examples are by using different variables to see if there are any correlations on other businesses or stocks, analyzing trends, and more. This project uses the top 5000 growing companies by percentage revenue growth from 2014 to 2017 to show how the top 5000 companies compare to each other by year founded, place of origin, and industry."),
                    
                    fluidRow("The algorithm suitible for the data set is a catagorical algorithm which puts the data into a data table, histogram, and pie chart. By changing the three inputs above, Industry, State, and Year Founded, the data table, as well as the histogram and pie chart under Year Established and Industry Percentages will change to reflect the inputs."),
                    fluidRow("The data set contains rank, growth, number of employees, company name, state, revenue, year founded, and industry."),
                    fluidRow("Data set comes from https://www.kaggle.com/datasets/msrhossain/2018-inc-5000-companies-list?resource=download"))
))

server<- function(input, output) {
  
  output$table <- DT::renderDataTable(DT::datatable({
    data <- inc5000
    if (input$ind != "All") {
      data <- data[data$INDUSTRY == input$ind,]
    }
    if (input$state != "All") {
      data <- data[data$STATE == input$state,]
    }
    if (input$found != "All") {
      data <- data[data$FOUNDED == input$found,]
    }
    data
    
  }))
  
  
  output$hist <- renderPlot({
    data <- inc5000
    if (input$ind != "All") {
      data <- data[data$INDUSTRY == input$ind,]
    }
    if (input$state != "All") {
      data <- data[data$STATE == input$state,]
    }
    
    hist(data$FOUNDED, main = "Year Founded", xlab = "Year", ylab = "Number of companies", breaks = 100)})
  
  output$pie <- renderPlot({
    data <- inc5000
    if (input$found != "All") {
      data <- data[data$FOUNDED == input$found,]
    }
    if (input$state != "All") {
      data <- data[data$STATE == input$state,]
    }
    
      pietable <- table(data$INDUSTRY)
      pct <- round(pietable/sum(pietable)*100) 
      lbls <- paste(names(pietable), pct)
      lbls <- paste(lbls, "%", sep= "" )

     pie <- pie(pietable, labels = lbls, main = "Top 5000 Companies By Industry")
    
  })
  
  
}

shinyApp(ui = ui, server = server)

