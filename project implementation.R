install.packages("shiny")
install.packages("dplyr")
install.packages("arules")
install.packages("ggplot2")
library(shiny)
library(dplyr)
library(arules)
library(ggplot2)

Data_set <- reactiveVal()
ui <- fluidPage(
  titlePanel("Data Analzing"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("Data_set", "Enter you Data set:", accept=".csv"),
      actionButton("cleanbutton","Process the data"),
      verbatimTextOutput("result"),
      numericInput("k","Enter number of clusters:",value = 2,min = 2,max = 4),
      actionButton("k_means","Sumbit K"),
      numericInput("min_confidence","Enter the precentage of confidence:",value = 0.001,min = 0.001,max = 1.000,step = .001),
      actionButton("sumbit_confidence","Sumbit Confidence"),
      numericInput("min_support","Enter the precentage of support:",value = 0.001,min = 0.001,max = 1.000,step = .001),
      actionButton("sumbit_support","Sumbit Support"),
      actionButton("run_apriori", "Run Apriori")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Cleaned Dataset", dataTableOutput("cleaned_table")),
        tabPanel("Visualisation",
                 plotOutput("pie_chart"),
                 h4("Legend:"),
                 tags$div(
                   tags$p(tags$span(style = "background-color:goldenrod; padding: 5px;", "Credit")),
                   tags$p(tags$span(style = "background-color:steelblue; padding: 5px;", "Cash"))
                 ),
                 plotOutput("bar_chart"),
                 plotOutput("barplot"),
                 plotOutput("Histogram")
        ),
        tabPanel("Clustering",verbatimTextOutput("cluster_output"),
                 plotOutput("kmeans_result")),
        tabPanel("Association_Rules",tableOutput("apriori_rules"))
        )
        )
      )
    )

server <- function(input, output) {
  #aplly cleanning
  observeEvent(input$cleanbutton, {
    req(input$Data_set)
    data <- read.csv(input$Data_set$datapath)
    Data_set(data)  
    cleaned_data <- data %>% 
      distinct() %>%      
      na.omit()          
    Data_set(cleaned_data)
  })
  # Display the dataset
  output$cleaned_table <- renderDataTable({
    req(Data_set())
    Data_set()
  })
   #apply association rules
  observeEvent(input$run_apriori, {
    tdata <-  read.transactions(input$ Data_set$datapath, format = "basket", sep = ",")
    min_confidence <- as.numeric(input$min_confidence)
    min_support <- as.numeric(input$min_support)
    apriori_rules <- apriori(tdata, parameter = list(supp = min_support, conf = min_confidence, minlen = 2))

    output$apriori_rules <- renderTable({
      inspect(apriori_rules)
    })
  })
  
   #apply k_means
  observeEvent(input$k_means,{
    k <- as.numeric(input$k)
    selected_data <- c("age", "total") 
    DN <- Data_set() %>% select(all_of(selected_data))
    kmeans_result <- kmeans(DN, centers = k)
   Groups<- as.data.frame(DN)
   Groups$Cluster <- as.factor(kmeans_result$cluster)
    output$cluster_output <- renderPrint({
      cluster_groups <- split( Groups, Groups$Cluster)
      for (i in 1:k) {
        cat(paste0("Cluster ", i, ":\n"))
        print(cluster_groups[[as.character(i)]])
      }
    })
  })
  #pie chart
  output$pie_chart <- renderPlot({
    data <- Data_set() 
  x <- table(data$paymentType)
  percentage <-paste0(round(100*x/sum(x)),"%")
  pie(x, labels = percentage,main = "Compare between cridet & cash", col=c("goldenrod","steelblue"))
  })
  #bar chart1
  output$bar_chart <- renderPlot({
    data <- Data_set()
    D_N1<- data %>%
      group_by(age) %>%
      summarise(total_spending = sum(total, na.rm = TRUE))
    barplot( D_N1$total_spending,name= D_N1$age,xlab = "Age", ylab = "Total spending",main="Total Spending by Age ",col ="steelblue")
  })
  # bar blot
  output$barplot <- renderPlot({
    data <- Data_set()
    D_N2 <- data %>%
      group_by(city) %>%  
      summarise(total_spending = sum(total, na.rm = TRUE)) %>%
      arrange(desc(total_spending)) 
    barplot(  
      height = D_N2 $total_spending,  
      name = D_N2 $city, 
      col = "goldenrod", 
      main = "cities spending",  
      xlab = "city name",  
      ylab = "total spending"
    )
  })
  # Histogram
  output$Histogram <- renderPlot({
    data <- Data_set()
    hist(data$total,col="steelblue", border="goldenrod",main= "Display total spending. ", xlab="total spending" , 
         ylab=" distribution")
  })
  # update the result when enter number 
  output$result <- renderText({
    input$num^2
  })
  
}

shinyApp(ui = ui, server = server)



