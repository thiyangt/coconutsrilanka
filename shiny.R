library(shiny)
library(DT)
library(shinyWidgets)

#Create a Dataset
get_data <- function(size){
  startTime <- as.POSIXct("2016-01-01")
  endTime <- as.POSIXct("2019-01-31")
  DATE <- as.Date(sample(seq(startTime, endTime, 1), size))
  WEEKDAY <- weekdays(as.Date(DATE))
  LOCATION <- sample(c("A", "B", "C"), size, replace = T, prob = c(0.4, 0.4, 0.2))
  EQUIPMENT <- sample(c("E1", "E2", "E3", "E4"), size, replace = TRUE)
  COUNTS <- sample(c(1:10), size, replace = TRUE)
  df <- data.frame(WEEKDAY, LOCATION, EQUIPMENT, COUNTS)
  
  return(df)
}

this_table <-get_data(1) %>% select(WEEKDAY, LOCATION, EQUIPMENT)
#The goal is to simulate the inputs in the form of a table to be the new data set that will then
#be used in the newdata parameter to model the counts using Poisson regression

#-----------------------------------------------------------------------------------------------
ui <- fluidPage(
  sidebarPanel(
    pickerInput('days_of_week', 'Choose Weekdays:', choices =c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")),
    pickerInput('location', "Select Location:", choices = c("A", "B", "C")),
    pickerInput('equipment_type', "Choose Equipment:", choices = c("E1", "E2", "E3", "E4")),
    
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
    t = rbind(data.frame(WEEKDAY = input$days_of_week,
                         LOCATION = input$location,
                         EQUIPMENT = input$equipment_type), this_table())
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
  
  train <- get_data(10000)
  factors <- c("WEEKDAY", "LOCATION", "EQUIPMENT")
  train <- train %>% mutate_if(is.character, as.factor)
  
  Model <- glm(COUNTS ~ WEEKDAY + LOCATION + EQUIPMENT, data = train, family = "poisson")
  #summary(Model)
  
  new_data <- this_table()
  new_data$WEEKDAY <- as.factor(new_data$WEEKDAY)
  new_data$LOCATION <- as.factor(new_data$LOCATION)
  new_data$EQUIPMENT <- as.factor(new_data$EQUIPMENT)
  
  df <- data.frame(new_data, PREDICTED_COUNTS = predict(Model, newdata = new_data, type = "response"))
  df
})


output$prediction_table <- DT::renderDT({
  predict_df()
})
  
  
}

shinyApp(ui = ui, server = server)