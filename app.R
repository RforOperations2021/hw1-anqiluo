library(shiny)
library(ggplot2)
library(DT)
library(stringr)
library(tools)
library(dplyr)
test = read.csv('2006_-_2011_NYS_Math_Test_Results_by_Grade_-_Citywide_-_by_Race-Ethnicity.csv')


ui <- fluidPage(
  downloadLink('downloadData','download'),
  titlePanel("NYC Math Test Result"),
  sidebarLayout(
    sidebarPanel(
    
    checkboxInput(inputId = "show_table",
                  label = "Show data table",
                  value = TRUE),
    
    checkboxGroupInput(inputId = "selected_grade",
                       label = "Select grade:",
                       choices = c("3", "4", "5","6","7","8","All Grade"),
                       selected = "3"),
    sliderInput(inputId = "n_sample", 
                label = "Sample Size:", 
                min = 0, max = nrow(test),
                value = 10),
    
    textInput(inputId = "comment", 
              label = "Leave a comment!",
              placeholder = "Comments on the data distribution")
    ),
    mainPanel(
      plotOutput(outputId = "hist"),
      uiOutput(outputId = "comment"),
      plotOutput(outputId = 'boxplot'),
      DT::dataTableOutput(outputId = "scoretable")
    )
  )
)



server <- function(input, output,session) {
  
  comment = reactive({toTitleCase(input$comment)})
  
  grade_subset = reactive({
    req(input$selected_grade)
    filter(test,Grade%in%input$selected_grade)
  })
  
  
  grade_sample = reactive({
    req(input$n_sample)
    sample_n(test,input$n_sample)
  })
  
  output$scoretable = DT::renderDataTable(
    if (input$show_table){
      DT::datatable(data = test[, 1:6], 
                    options = list(pageLength = 10), 
                    rownames = FALSE)
    }
  )

  output$hist = renderPlot({
    hist(grade_sample()$Number.Tested,
      xlab = 'Number Tested',main =paste('Histogram of Number Tested for Grade', sapply(list(input$selected_grade), paste, collapse = ", ")))
  })
  

  output$boxplot = renderPlot({
    boxplot(grade_sample()[c('Level.1..','Level.1...1','Level.2..','Level.2...1','Level.3..','Level.3...1','Level.4..','Level.4...1','Level.3.4..','Level.3.4...1')],main=paste('Boxplot of test scores from different levels for Grade',sapply(list(input$selected_grade), paste, collapse = ", ")))
  })
  
  output$comment = renderUI({
    req(input$comment)
    HTML(paste("Comments from the user:", input$comment,"<br>"))
  })

  output$downloadData <- downloadHandler(
    filename = function() {
     paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(test, file)
    }
   )
}
shinyApp(ui = ui, server = server)
