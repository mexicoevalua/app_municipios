# Instalar paquetes para publicar el app
install.packages("shiny")
devtools::install_github('rstudio/shinyapps')
library(shinyapps)
library(shiny)
runApp("app_municipios")
setwd("app_municipios")
deployApp()
# Para eliminar la aplicacion
#shinyapps::terminateApp("app_municipios")

