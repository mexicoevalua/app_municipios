library(shiny)

# Cargar datos
data  <- read.csv("data/data_table.csv", encoding="utf8")

# Define the overall UI
shinyUI(
  fluidPage(
    titlePanel("Clasificación de municipios según delitos del fuero común"),
    helpText("La tabla muestra el lugar nacional que ocupa cada municipio en México de acuerdo al número de averiguaciones (2011-2014)
             previas y la tasa por cada 100 mil habitantes en delitos seleccionados del fuero común: homicidios dolosos,
             robo, secuestro, otros delitos y el total de las averiguaciones previas"),
    
    # Create a new Row in the UI for selectInputs
    fluidRow(
      column(4, 
             selectInput("estado", 
                         "Estado:", 
                         c("Todos", 
                           unique(as.character(data$Estado))))
      ),
      column(4, 
             selectInput("year", 
                         "Año:", 
                         c("Todos", 
                           unique(as.character(data$Año))))
      ),
     
      column(4, 
             selectInput("crimen", 
                         "Crimen:", 
                         c("Todos", 
                           unique(as.character(data$Crimen))))
      )
    ),
    # Create a new row for the table.
    fluidRow(
      dataTableOutput(outputId="table")
    ),
    helpText(
      "Desarrollado por ", a(href="http://www.mexicoevalua.org", "México Evalúa."),
      "Visita el proyecto en", a(href="https://github.com/mexicoevalua/app_municipios", "GitHub.")),
    img(src="logo_72x92.png", height = 72, width = 92)
    
             
  )  
)