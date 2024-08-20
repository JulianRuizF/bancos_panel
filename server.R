library(shiny)
library(tidyverse)

datos_path <- "datos/"

plot_text_size <- 18

function(input, output, session) {
  
  tabset_id_reactive <- reactive({input$main_navbar})
  
  cotizaciones <- cotizaciones_Server(id="cotizaciones", tabset_id=tabset_id_reactive)
  cascada <- cascada_Server(id="cascada", tabset_id=tabset_id_reactive)
  seccion2 <- seccion2_Server(id="seccion2",tabset_id=tabset_id_reactive) 
  opciones <- opciones_Server(id="opciones", tabset_id=tabset_id_reactive)
  
  
  
}