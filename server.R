library(shiny)

# Load the ggplot2 package which provides
# the 'mpg' dataset.

data  <- read.csv("data/data_table.csv", encoding="utf8")

# Define a server for the Shiny app
shinyServer(function(input, output) {
  
  # Filter data based on selections
  output$table <- renderDataTable({

    if (input$estado != "Todos"){
      data <- data[data$Estado == input$estado,]
    }
    if (input$year != "Todos"){
      data <- data[data$Año == input$year,]
    }
    if (input$crimen != "Todos"){
      data <- data[data$Crimen == input$crimen,]
    }
    data
  })
  
})