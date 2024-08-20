library(shiny)
library(patchwork)
library(tidyverse)


ui <- navbarPage(
  fluid=TRUE,
  id="main_navbar",
  title="entidades_bancarias",
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
  seccion2_UI(id="seccion2"),
  opciones_UI(id="opciones"),
  
)