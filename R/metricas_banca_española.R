library(shiny)
library(readxl)
library(tidyverse)
library(ggplot2)
library(ggiraph)
library(openxlsx)
library(lubridate)
library(scales)

source("utilidades.R")  # Utilidades que podría incluir funciones personalizadas, asegúrate de que exista

# Leer el archivo del Dashboard, CAMBIAR PARA QUE LUEGO LEA EL CUADROS PARA FLASH QUE SE VA A ACTUALIZAR ----
archivo <- list.files(path = "datos", pattern = "Cuadros para Flash.xlsx", full.names = TRUE)
metricas_banca_española_df <- readxl::read_xlsx(archivo, sheet = "Agregado", col_names = TRUE) |>
  mutate(across(-c("entidad", "periodo"), as.numeric)) |> 
  pivot_longer(cols = -c("entidad", "periodo"), 
               names_to = "codigo_metrica",
               values_to = "valores") |> 
  rename(fecha = periodo) |> 
  mutate(fecha = as.Date(fecha, format = "%Y-%m-%d")) |> 
  left_join(diccionario_metricas_bancos_df, by = c("codigo_metrica" = "codigo_metrica")) |> 
  select(fecha, metrica, entidad, valores, everything()) # Se pone fecha al principio y se generan NAs porque hay espacios vacíos en el Excel


# UI ---- A partir de aquí sustituyo EBA por metricas_banca_española
metricas_banca_española_UI <- function(id, label = "Información financiera de Bancos Españoles") {
  ns <- NS(id)
  
  tabPanel(
    "Información financiera de Bancos Españoles",
    value = "main_tabset_metricas_banca_española",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        selectizeInput(
          inputId = ns("banca_española_metrica_input"),
          label = "Métrica",
          choices = metricas_banca_española_df$metrica,
          selected = c("Ingresos Netos Intereses España", "Comisiones España", "Gastos de Explotación España", "Deterioros y Provisiones España"),
          multiple = TRUE,
          options = list(create = TRUE)
        ),
        selectizeInput(
          inputId = ns("banca_española_entidad_input"),
          label = "Entidad",
          choices = metricas_banca_española_df$entidad,
          selected = c("Santander", "BBVA", "CaixaBank", "Sabadell", "Bankinter", "Unicaja"),
          multiple = TRUE,
          options = list(create = TRUE)
        ),
        dateRangeInput(
          inputId = ns("fecha_rango"),
          label = "Selecciona el rango de fechas",
          start = "2020-12-31",
          end = Sys.Date()
        ),
        checkboxInput(ns("base100"), "Mostrar en base 100", value = TRUE),
        checkboxInput(ns("porcentaje"), "Mostrar en porcentaje", value = FALSE),
        checkboxInput(ns("mostrar_nombre"), "Mostrar nombre del gráfico", value = TRUE),
        numericInput(ns("yaccuracy"), "Precisión de los ejes Y", value = 1),
        numericInput(ns("grosor_linea"), "Grosor de la línea", value = 1, min = 0.1),
        numericInput(ns("angulo_ejex"), "Angulo eje x: 0 es en horizontal", value = 90), # De 0 a 360
        numericInput(ns("ancho_grafico"), "Ancho del gráfico (pulgadas)", value = 4),
        numericInput(ns("largo_grafico"), "Largo del gráfico (pulgadas)", value = 4),
        numericInput(ns("size_tooltip"), "Tamaño del tooltip", value = 0.1),
        selectInput(ns("xbreaks"), "Periodicidad eje X", choices = c("Trimestre" = "quarter", "Año" = "year", "Mes" = "month")),
        downloadButton(outputId = ns("download_data"), label = "Guardar en Excel", class = "btn-lg btn-block"),
        downloadButton(outputId = ns("download_data_docx"), label = "Guardar Gráficos", class = "btn-lg btn-block")
      ),
      mainPanel(
        fluidRow(
          column(width = 6, ggiraph::girafeOutput(outputId = ns("metricas_banca_española_grafico_plt_1"))),
          column(width = 6, ggiraph::girafeOutput(outputId = ns("metricas_banca_española_grafico_plt_2")))
        ),
        fluidRow(
          column(width = 6, ggiraph::girafeOutput(outputId = ns("metricas_banca_española_grafico_plt_3"))),
          column(width = 6, ggiraph::girafeOutput(outputId = ns("metricas_banca_española_grafico_plt_4")))
        )
      )
    )
  )
}

# metricas_banca_española_Server ----
metricas_banca_española_Server <- function(id, tabset_id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Preparar datos seleccionados
    metricas_banca_española_selected_df <- reactive({
      req(input$banca_española_metrica_input, input$banca_española_entidad_input, input$fecha_rango)
      
      # Filtrar los datos según las métricas, entidades y el rango de fechas seleccionadas
      datos <- metricas_banca_española_df %>%
        filter(metrica %in% input$banca_española_metrica_input) %>%
        filter(entidad %in% input$banca_española_entidad_input) %>%
        filter(fecha >= as.Date(input$fecha_rango[1]) & fecha <= as.Date(input$fecha_rango[2]))
      
      # Aplicar Base 100 si está seleccionado
      if (input$base100) {
        datos <- datos %>%
          group_by(metrica, entidad) %>% 
          mutate(valores = (valores / last(valores)) * 100)
      }
      
      # Convertir a porcentaje si está seleccionado
      if (input$porcentaje) {
        datos <- datos %>%
          mutate(valores = valores * 100)
      }
      
      # Fijar los colores que dependen de las entidades seleccionadas
      colores_bancos_modificado <- tesorotools::colores_bancos
      colores_bancos_modificado["Sabadell"] <- "#00BFFF"  # Un azul más profundo
      colores_bancos_modificado["Caixabank"] <- "#007ACC" # Un azul claro pero más suave
      names(colores_bancos_modificado)[names(colores_bancos_modificado) == "Caixabank"] <- "CaixaBank"
      
      # Usar match() para garantizar que los colores sigan el orden de entidades seleccionadas
      colores_ordenados <- colores_bancos_modificado[match(input$banca_española_entidad_input, names(colores_bancos_modificado))]
      
      return(list(datos = datos, colores = colores_ordenados))
    })
    
    # Función para generar gráficos con generar_lineas_plot
    generar_grafico_metricas_banca_española <- function(datos, colores, idx, nombre_serie) {
      if (nrow(datos) == 0) {
        return(NULL)
      }
      
      ysuffix <- if (input$porcentaje) "%" else ""
      
      # Ajustar el eje X según la periodicidad seleccionada
      xbreaks_fun <- switch(input$xbreaks,
                            "month" = scales::date_breaks("1 month"),
                            "quarter" = scales::date_breaks("3 months"),
                            "year" = scales::date_breaks("1 year"))
      
      plot <- generar_lineas_plot(
        .data = datos,
        .fecha = "fecha",
        .valores = "valores",
        .nombres = "entidad",
        .yaccuracy = input$yaccuracy,
        .ysuffix = ysuffix,
        .trans = "",
        .xbreaks = xbreaks_fun,
        .grosor_linea = input$grosor_linea,
        .angulo_ejex = input$angulo_ejex
      ) +
        ggiraph::geom_point_interactive(
          mapping = aes(tooltip = paste0(entidad, ": ", scales::comma(valores, accuracy = input$yaccuracy, big.mark = ".", decimal.mark = ","), ysuffix, "\n", "fecha: ", fecha)),
          size = input$size_tooltip  # Tamaño del tooltip ajustable
        ) + 
        scale_color_manual(values = colores)
      
      # Mostrar o no el nombre del gráfico
      if (input$mostrar_nombre) {
        plot <- plot + labs(title = nombre_serie)
      }
      
      return(plot)
    }
    
    # Renderización de los 4 gráficos
    observe({
      result <- metricas_banca_española_selected_df()
      datos <- result$datos
      colores <- result$colores
      series_seleccionadas <- input$banca_española_metrica_input
      
      if (length(series_seleccionadas) > 0 && !is.null(datos)) {
        for (i in 1:min(4, length(series_seleccionadas))) {
          local({
            idx <- i
            output[[paste0("metricas_banca_española_grafico_plt_", idx)]] <- ggiraph::renderGirafe({
              datos_serie <- datos %>% filter(metrica == series_seleccionadas[idx])
              if (nrow(datos_serie) > 0) {
                plot_plt <- generar_grafico_metricas_banca_española(datos_serie, colores, idx, series_seleccionadas[idx])
                ggiraph::girafe(ggobj = plot_plt, 
                                width_svg = input$ancho_grafico,  # Tamaño ajustable del gráfico
                                height_svg = input$largo_grafico, 
                                options = list(opts_tooltip(opacity = 0.9)))
              } else {
                ggiraph::girafe(ggobj = NULL)  # Evitar gráficos vacíos
              }
            })
          })
        }
      }
    })
  })
}




# UI y Server
ui <- navbarPage("Información financiera de Bancos Españoles", metricas_banca_española_UI("metricas_banca_española")) # Información financiera de Bancos Españoles

server <- function(input, output, session) {
  metricas_banca_española_Server("metricas_banca_española", "main_tabset_metricas_banca_española")
}

shinyApp(ui = ui, server = server)
