library(shiny)
library(readxl)
library(tidyverse)
library(ggplot2)
library(ggiraph)
library(openxlsx)
library(lubridate)
library(scales)
library(officer)
library(rvg)

source("utilidades.R")  # Utilidades que podría incluir funciones personalizadas, asegúrate de que exista

# Leer el archivo del Dashboard ----
files <- list.files(path = "datos", pattern = "Data Annex InteractiveRiskDashboard.*202[45]\\.xlsx$", full.names = TRUE)
files_sorted <- files[order(sapply(files, function(file) {
  matches <- regmatches(file, regexpr("Q[1-4] 202[45]", file))
  return(matches)
}), decreasing = TRUE)]
latest_file <- files_sorted[1]
datos_EBA_df <- readxl::read_xlsx(latest_file, sheet = "KRIs by country and EU", col_names = TRUE) |>
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
          start = "2019-01-01",
          end = Sys.Date()
        ),
        checkboxInput(ns("base100"), "Mostrar en base 100", value = FALSE),
        checkboxInput(ns("porcentaje"), "Mostrar en porcentaje", value = TRUE),
        checkboxInput(ns("mostrar_nombre"), "Mostrar nombre del gráfico", value = TRUE),
        numericInput(ns("yaccuracy"), "Precisión de los ejes Y", value = 0.01),
        numericInput(ns("grosor_linea"), "Grosor de la línea", value = 1, min = 0.1),
        numericInput(ns("angulo_ejex"), "Angulo eje x: 0 es en horizontal", value = 90), # De 0 a 360
        numericInput(ns("ancho_grafico"), "Ancho del gráfico (pulgadas)", value = 4),
        numericInput(ns("largo_grafico"), "Largo del gráfico (pulgadas)", value = 4),
        numericInput(ns("size_tooltip"), "Tamaño del tooltip", value = 0.1),
        selectInput(ns("xbreaks"), "Periodicidad eje X", choices = c("Año" = "year", "Mes" = "month", "Trimestre" = "quarter", "Semestre" = "semester")),
        downloadButton(outputId = ns("download_data_excel"), label = "Guardar en Excel", class = "btn-lg btn-block"),
        downloadButton(outputId = ns("download_data_docx"), label = "Guardar Gráficos", class = "btn-lg btn-block")
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
      datos <- datos_EBA_df %>%
        filter(nombre %in% input$EBA_grafico_series_input) %>%
        filter(pais %in% input$EBA_grafico_paises_input) %>%
        filter(fecha >= as.Date(input$fecha_rango[1]) & fecha <= as.Date(input$fecha_rango[2])) 
      
      if (input$base100) {
        datos <- datos %>%
          group_by(nombre, pais) %>% 
          mutate(valores = (valores / first(valores)) * 100)
      }
      
      if (input$porcentaje) {
        datos <- datos %>%
          mutate(valores = valores * 100)  # Convertir a porcentaje si está activado
      }
      
      return(datos)
    })
    
    # Función para generar gráficos con generar_lineas_plot
    generar_grafico_EBA <- function(datos, idx, nombre_serie) {
      if (nrow(datos) == 0) {
        return(NULL)
      }
      
      ysuffix <- if (input$porcentaje) "%" else ""
      
      # Ajustar el eje X según la periodicidad seleccionada
      xbreaks_fun <- switch(input$xbreaks,
                            "month" = scales::date_breaks("1 month"),
                            "quarter" = scales::date_breaks("3 months"),
                            "semester" = scales::date_breaks("6 months"),
                            "year" = scales::date_breaks("1 year"))
      
      plot <- generar_lineas_plot(
        .data = datos,
        .fecha = "fecha",
        .valores = "valores",
        .nombres = "pais",
        .yaccuracy = input$yaccuracy,
        .ysuffix = ysuffix,
        .trans = "",
        .xbreaks = xbreaks_fun,
        .grosor_linea = input$grosor_linea,
        .angulo_ejex = input$angulo_ejex  # Usar input para el ángulo
        # .hjust_ejex = input$hjust_ejex     # Usar input para hjust
      ) +
        ggiraph::geom_point_interactive(
          mapping = aes(tooltip = paste0(pais, ": ", scales::comma(valores, accuracy = input$yaccuracy, big.mark = ".", decimal.mark = ","), ysuffix, "\n", "fecha: ", fecha)),
          size = input$size_tooltip  # Tamaño del tooltip ajustable
        )
      
      # Mostrar o no el nombre del gráfico
      if (input$mostrar_nombre) {
        plot <- plot + labs(title = nombre_serie)
      }
      
      return(plot)
    }
    
    observe({
      datos <- EBA_selected_df()
      series_seleccionadas <- input$EBA_grafico_series_input
      
      if (length(series_seleccionadas) > 0 && !is.null(datos)) {
        for (i in 1:min(4, length(series_seleccionadas))) {
          local({
            idx <- i
            output[[paste0("EBA_grafico_plt_", idx)]] <- ggiraph::renderGirafe({
              datos_serie <- datos %>% filter(nombre == series_seleccionadas[idx])
              if (nrow(datos_serie) > 0) {
                plot_plt <- generar_grafico_EBA(datos_serie, idx, series_seleccionadas[idx])
                ggiraph::girafe(ggobj = plot_plt, 
                                width_svg = input$ancho_grafico,  
                                height_svg = input$largo_grafico, 
                                options = list(opts_tooltip(opacity = 0.9)))
              } else {
                ggiraph::girafe(ggobj = NULL)
              }
            })
          })
        }
      }
    })
    
    # Guardar el Excel
    output$download_data_excel <- downloadHandler(
      filename = function() {
        paste("EBA-", Sys.Date(), ".xlsx", sep = "")
      },
      content = function(file) {
        openxlsx::write.xlsx(EBA_selected_df(), file)
      }
    )
    
    # Descargar gráficos en archivo Word
    output$download_data_docx <- downloadHandler(
      filename = function() {
        paste0("EBA_Graficos_", Sys.Date(), ".docx")
      },
      content = function(file) {
        series_seleccionadas <- input$EBA_grafico_series_input
        datos <- EBA_selected_df()
        
        graficos_creados <- lapply(1:min(4, length(series_seleccionadas)), function(i) {
          datos_serie <- datos %>% filter(nombre == series_seleccionadas[i])
          if (nrow(datos_serie) > 0) {
            generar_grafico_EBA(datos_serie, i, series_seleccionadas[i])
          } else {
            NULL
          }
        })
        
        guardar_graficos(graficos_creados, series_seleccionadas, input$ancho_grafico, input$largo_grafico, file)
      }
    )
  })
}


# UI y Server
ui <- navbarPage("EBA", EBA_UI("EBA"))

server <- function(input, output, session) {
  EBA_Server("EBA", "main_tabset_EBA")
}

shinyApp(ui = ui, server = server)
