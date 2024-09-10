library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(ggiraph)
library(openxlsx)
library(lubridate)

source("utilidades.R")  # Utilidades que podría incluir funciones personalizadas, asegúrate de que exista

# Leer el archivo del Dashboard ----
files <- list.files(path = "datos", pattern = "Data Annex InteractiveRiskDashboard.*202[45]\\.xlsx$", full.names = TRUE)
files_sorted <- files[order(sapply(files, function(file) {
  matches <- regmatches(file, regexpr("Q[1-4] 202[45]", file))
  return(matches)
}), decreasing = TRUE)]
latest_file <- files_sorted[1]
datos_EBA_df <- read_xlsx(latest_file, sheet = "KRIs by country and EU", col_names = TRUE) |>
  rename(fecha = `[Period]`, codigo_pais = `[Country]`, numero = `[Number]`, codigo = `[Name]`, valores = `[Ratio]`) |>
  mutate(fecha = as.Date(paste0(as.character(fecha), "01"), "%Y%m%d") |> ceiling_date("month") - days(1)) |> 
  left_join(diccionario_EBA_series_df, by = c("codigo" = "codigo_serie")) |>
  left_join(diccionario_paises_df, by = c("codigo_pais" = "codigo_pais")) |>
  select(fecha, pais, nombre, valores)

# UI----
EBA_UI <- function(id, label = "EBA") {
  ns <- NS(id)
  
  tabPanel(
    "EBA",
    value = "main_tabset_EBA",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        selectizeInput(
          inputId = ns("EBA_grafico_series_input"),
          label = "Variable",
          choices = datos_EBA_df$nombre,
          selected = c("ROE", "ROA", "NPL ratio", "Ratio de cobertura"),
          multiple = TRUE,
          options = list(create = TRUE)
        ),
        selectizeInput(
          inputId = ns("EBA_grafico_paises_input"),
          label = "Países",
          choices = datos_EBA_df$pais,
          selected = c("España", "Unión Europea", "Alemania", "Italia", "Francia", "Portugal"),
          multiple = TRUE,
          options = list(create = TRUE)
        ),
        dateRangeInput(
          inputId = ns("fecha_rango"),
          label = "Selecciona el rango de fechas",
          start = "2018-01-01",
          end = Sys.Date()
        ),
        checkboxInput(ns("base100"), "Mostrar en base 100", value = FALSE),
        downloadButton(outputId = ns("download_data"), label = "Guardar en Excel", class = "btn-lg btn-block")
      ),
      mainPanel(
        fluidRow(
          column(width = 6, ggiraph::girafeOutput(outputId = ns("EBA_grafico_plt_1"))),
          column(width = 6, ggiraph::girafeOutput(outputId = ns("EBA_grafico_plt_2")))
        ),
        fluidRow(
          column(width = 6, ggiraph::girafeOutput(outputId = ns("EBA_grafico_plt_3"))),
          column(width = 6, ggiraph::girafeOutput(outputId = ns("EBA_grafico_plt_4")))
        )
      )
    )
  )
}


# EBA_Server ----
EBA_Server <- function(id, tabset_id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Preparar datos seleccionados
    EBA_selected_df <- reactive({
      req(input$EBA_grafico_series_input, input$fecha_rango)
      datos_EBA_df %>%
        filter(nombre %in% input$EBA_grafico_series_input) %>%
        filter(pais %in% input$EBA_grafico_paises_input) %>%
        filter(fecha >= as.Date(input$fecha_rango[1]) & fecha <= as.Date(input$fecha_rango[2])) %>%
        {
          if (input$base100) {
            group_by(., nombre, pais) %>% 
              mutate(valores = (valores / first(valores)) * 100)
          } else {
            .
          }
        }
    })
    
    # Función para generar gráficos con generar_lineas_plot
    generar_grafico <- function(datos, idx) {
      if (nrow(datos) == 0) {
        return(NULL)
      }
      generar_lineas_plot(
        .data = datos,
        .fecha = "fecha",
        .valores = "valores",
        .nombres = "pais",  # El campo que tiene los países
        .yaccuracy = 0.01,
        .ysuffix = "",
        .trans = "",
        .xbreaks = NULL
      ) +
        ggiraph::geom_line_interactive(
          mapping = aes(tooltip = paste0(pais, ": ", valores, "\n", "fecha: ", fecha))
        ) +
        ggiraph::geom_point_interactive(
          mapping = aes(tooltip = paste0(pais, ": ", sprintf("%.2f", valores), "\n", "fecha: ", fecha))
        )
    }
    
    # Renderización de los 4 gráficos
    observe({
      datos <- EBA_selected_df()
      series_seleccionadas <- input$EBA_grafico_series_input
      
      # Verificar que se hayan seleccionado series y haya datos
      if (length(series_seleccionadas) > 0 && !is.null(datos)) {
        for (i in 1:min(4, length(series_seleccionadas))) {
          local({
            idx <- i  # Asegurar que se mantenga el valor de i dentro del ciclo
            output[[paste0("EBA_grafico_plt_", idx)]] <- ggiraph::renderGirafe({
              datos_serie <- datos %>% filter(nombre == series_seleccionadas[idx])
              if (nrow(datos_serie) > 0) {
                plot_plt <- generar_grafico(datos_serie, idx)
                ggiraph::girafe(ggobj = plot_plt)
              } else {
                ggiraph::girafe(ggobj = NULL)  # Evitar gráficos vacíos
              }
            })
          })
        }
      }
    })
    
    # Función para descargar datos
    output$download_data <- downloadHandler(
      filename = function() { paste("EBA-", Sys.Date(), ".xlsx", sep = "") },
      content = function(file) { write.xlsx(EBA_selected_df(), file) }
    )
  })
}

# UI y Server
ui <- navbarPage("EBA", EBA_UI("EBA"))

server <- function(input, output, session) {
  EBA_Server("EBA", "main_tabset_EBA")
}

shinyApp(ui = ui, server = server)
