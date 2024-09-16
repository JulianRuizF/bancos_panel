library(shiny)
library(patchwork)
library(tidyverse)


ui <- navbarPage(
  fluid=TRUE,
  id="main_navbar",
  title="Situación bancaria",
  shinyjs::useShinyjs(),
  theme = shinythemes::shinytheme("united"),
  # Titulo
  tags$head(
    tags$style(
      HTML(".shiny-notification {
              height: 100px;
              width: 800px;
              position:fixed;
              top: calc(50% - 50px);;
              left: calc(50% - 400px);;
            }
           "
      )
    )
  ),
  
  cotizaciones_UI(id="cotizaciones"),
  EBA_UI(id="EBA"),
  metricas_banca_española_UI(id="metricas_banca_española"),
  cascada_UI(id="cascada"),
  opciones_UI(id="opciones"),
  
)