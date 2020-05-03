#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
    tabsetPanel(
        tabsetPanel(
            tabPanel("Summary",
                     tags$h1("COVID19-Response"),
                     tags$hr(),
                     tags$br(),
                     tags$p("I decided to create the app as a way to monitor the responses to the current COVID19 pandemic. At first, I 
                                                                    just wanted to try to monitor how people were reacting, but I noticed we could gauge the economic responses of
                                                                    various individuals. Some of the questions we can answer using this data would be the following:"), 
                     tags$p("How has the intake of addictive substances, such as caffeine and alcohol, be affected?"),
                     tags$p("How has subscriptions to mass media, such as netflix, hulu, and other on demand services, been affected?"),
                     tags$p("Which gender, and age groups are responsible for the changes in behavior of demand for these substances, and services?")), 
            
            tabPanel("Data", 
                     tags$h1("Data Types"),
                     tags$hr(),
                     tags$br(),
                     tags$p("The data was collected via google forums. I created a survey that featured two types of questions: Categorical and Ordinal"),
                     tags$p("Categorical data is normally binary data. This data type takes exactly two possible values. In this case the values were either yes or no, male or female."),
                     tags$p("Ordinal data is a data type that assigns a number to a persons response. This number has no real value, and it should be considered more as a ranking, or 
                            a level of measurement"),
                     tags$p("It is important to note that I did maintain a few questions in their such as age range, and type of employment as a way to furthers classify the results. These type
                            of questions do not necessarily fall under the categorical or ordinal type. ")),
            tabPanel("Analysis", tags$h1("Types of Analysis"),
                     tags$hr(),
                     tags$br(),
                     navlistPanel(
                         tabPanel("Box and Whisker plot",
                                  selectInput(inputId = "Box_binary",
                                              label = "Our Binary Choices",
                                              choices = names(all_data[,1:11])),
                                  selectInput(inputId = "Box_ordinal",
                                              label = "Preferences",
                                              choices = names(all_data[12:18])),
                                   plotOutput((outputId = "Boxplot"))),
                         
                         tabPanel("Logistic Regression", 
                                  selectInput(inputId = "LR_binary",
                                              label = "Our Binary Choices",
                                              choices = names(all_data[,12:18])),
                                  selectInput(inputId = "LR_ordinal",
                                              label = "Preferences",
                                              choices = names(all_data[,12:18]),
                                              selected = "Netflix/Hulu"),
                                  plotOutput(outputId = "LR"),
                                  tags$br(),
                                  tags$p("I chose logistic regression because all the research I conducted showed that for ordinal data this type of regression works best.")),
                         tabPanel("Scatter Plot",
                                  selectInput(inputId = "y",
                                              label = "y-axis",
                                              choices = names(all_data[,1:11])),
                                  selectInput(inputId = "x",
                                              label = "x-axis",
                                              choices = names(all_data[,12:18])),
                                  plotOutput(outputId = "Scatter"))
                     ))))
    
)



server <- function(input, output) {
    output$Scatter <- renderPlot({
      ggplot(all_data, aes(!!as.name(input$x), !!as.name(input$y))) + geom_point() + geom_count()
    })
    output$LR <- renderPlot({
      ggplot(all_data, aes(!!as.name(input$LR_ordinal), !!as.name(input$LR_binary))) + geom_point() + geom_smooth(method = "glm") +geom_count()
    })
    output$Boxplot <- renderPlot({
      ggplot(all_data, aes(!!as.name(input$Box_binary), !!as.name(input$Box_ordinal))) + geom_boxplot()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
