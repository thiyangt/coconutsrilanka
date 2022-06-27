library(shiny)
library(DT)
library(shinyWidgets)
library(tidyverse)

#Create a Dataset
#get_data <- function(){
  coconut <- read_csv("coconut.csv")
  coconut$water <- coconut$FullWeight - (coconut$WeightWithOutWater)
  coconut$coconut_flesh <- coconut$FullWeight - ( coconut$water + coconut$ShellWeight)
  coconut$Palapi <- factor(coconut$Palapi)
  coconut$Kalati <- factor(coconut$Kalati)
#  return(coconut)
#}


this_table <- coconut[1,] %>% select(FullWeight,
                                      Hcircumference, Vcircumference,
                                      VCircumferencewithHusk, Palapi,
                                      Kalati)
#The goal is to simulate the inputs in the form of a table to be the new data set that will then
#be used in the newdata parameter to model the counts using Poisson regression

#-----------------------------------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("App to predict the weight of coconut flesh given the dimensions of the coconut"),
  sidebarPanel(
    numericInput("FullWeight", "Full Weight", 550, min = 150, max = 1000),
    numericInput("Hcircumference", "Horizontal Circumference (cm)", 31, min = 25, max = 50),
    numericInput("Vcircumference", "Vertical Circumference (cm)", 42, min = 25, max = 50),
    numericInput("VCircumferencewithHusk", "Vertical Circumference with Husk (cm)", 28, min = 28, max = 40),
    pickerInput('Palapi', 'Choose (0 - No, 1 - Yes):', choices =c(0, 1)),
    pickerInput('Kalati', "Choose (0 - No, 1 - Yes):", choices = c(0, 1)),

    
    actionButton("add_btn", "Add"),
    actionButton("delete_btn", "Delete"),
    actionButton("predict_btn", "Predict")
  ),
  
  mainPanel(
    DTOutput("shiny_table"),
    hr(),
    DTOutput("prediction_table")
  )
)

server <- function(input, output) {
  
  this_table <- reactiveVal(this_table)
  
  observeEvent(input$add_btn, {
    t = rbind(data.frame(FullWeight = input$FullWeight,
                         Hcircumference = input$Hcircumference,
                         Vcircumference = input$Vcircumference,
                         Palapi = input$Palapi,
                         Kalati = input$Kalati), this_table())
    this_table(t)
  })
  
  observeEvent(input$delete_btn, {
    t = this_table()
    print(nrow(t))
    if (!is.null(input$shiny_table_rows_selected)) {
      t <- t[-as.numeric(input$shiny_table_rows_selected),]
    }
    this_table(t)
  })
  
  
  
output$shiny_table <- renderDT({
    datatable(this_table(), selection = 'multiple', options = list(
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
        "}")
    ))
  })
  
  
predict_df <- eventReactive(input$predict_btn, {
  
  train <- get_data()

  
  Model <-  lm(coconut_flesh ~ FullWeight +
                         Hcircumference + Vcircumference +
                         VCircumferencewithHusk + Palapi +
                         Kalati, data=coconut)
  
  
  new_data <- this_table()
  new_data$Palapi <- as.factor(new_data$Palapi)
  new_data$Kalati <- as.factor(new_data$Kalati)
  
  
  df <- data.frame(new_data, PREDICTED_COUNTS = round(predict(Model, newdata = new_data, type = "response"), 2))
  df
})


output$prediction_table <- DT::renderDT({
  predict_df()
})
  
  
}

shinyApp(ui = ui, server = server)