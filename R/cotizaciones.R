source("utilidades.R", local = T) # datos si se ejecuta desde aquí

cotizaciones_UI <- function(id, label = "Cotizaciones") {
  ns <- NS(id)
  
  tabPanel(
    "Cotizaciones",
    value = "main_tabset_cotizaciones",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        fluidRow(
          selectizeInput(
            inputId = ns("cotizaciones_grafico_input"),
            choices = diccionario_bancos_df$nombres,
            label = "Entidades",
            selected = c("BBVA", "Santander"),
            multiple = TRUE,
            options = list(create = TRUE)
          ),
          dateRangeInput(
            inputId = ns("fecha_rango"),
            label = "Selecciona el rango de fechas",
            start = "2024-01-01",
            end = Sys.Date()
          ),
          checkboxInput(ns("base100"), "Mostrar en base 100", value = FALSE),
          numericInput(ns("grosor_linea"), "Grosor de la línea", value = 1, min = 0.1),
          numericInput(ns("angulo_ejex"), "Ángulo del eje X", value = 90, min = 0, max = 360),
          numericInput(ns("hjust_ejex"), "Ajuste horizontal (hjust)", value = 0.5, min = 0, max = 1),
          numericInput(ns("ancho_grafico"), "Ancho del gráfico (pulgadas)", value = 6),
          numericInput(ns("largo_grafico"), "Largo del gráfico (pulgadas)", value = 4),
          numericInput(ns("size_tooltip"), "Tamaño del tooltip", value = 0.1),
          selectInput(ns("xbreaks"), "Periodicidad eje X", choices = c("Mes" = "month", "Año" = "year", "Trimestre" = "quarter")),
          downloadButton(outputId = ns("download_data"), label = "Guardar en Excel", class = "btn-lg btn-block")
        )
      ),
      mainPanel(
        fluidRow(
          ggiraph::girafeOutput(outputId = ns("cotizaciones_grafico_plt"))
        )
      )
    )
  )
}



cotizaciones_Server <- function(id, tabset_id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      cotizaciones_df <- reactive({
        req(input$cotizaciones_grafico_input, input$fecha_rango)
        
        selected_codigos <- diccionario_bancos_df %>%
          filter(nombres %in% input$cotizaciones_grafico_input) %>%
          pull(codigos)
        
        .data_df <- tryCatch(
          {
            tidyquant::tq_get(
              selected_codigos,
              from = input$fecha_rango[1],
              to = input$fecha_rango[2]
            ) |> 
              rename(fecha = "date") |> 
              rename(valores = "close") |> 
              rename(nombres = "symbol")
          },
          error = function(e) {
            message("Error descargando tickers: ", e)
            return(NULL)
          }
        )
        
        if (!is.null(.data_df) && input$base100) {
          .data_df <- .data_df %>%
            group_by(nombres) %>% 
            mutate(across(where(is.numeric), ~ (.x / first(.x)) * 100)) %>%
            ungroup()
        }
        
        return(.data_df)
      })
      
      cotizaciones_grafico_plt <- reactive({
        req(cotizaciones_df())
        
        if (is.null(cotizaciones_df())) {
          return(NULL)
        }
        
        # Ajustar el eje X según la periodicidad seleccionada
        xbreaks_fun <- switch(input$xbreaks,
                              "month" = scales::date_breaks("1 month"),
                              "quarter" = scales::date_breaks("3 months"),
                              "year" = scales::date_breaks("1 year"))
        
        plot_plt <- generar_lineas_plot(
          .data = cotizaciones_df(),
          .fecha = "fecha",
          .valores = "valores",
          .nombres = "nombres",
          .yaccuracy=0.01,
          .ysuffix="", 
          .grosor_linea = input$grosor_linea,      # Grosor parametrizado
          .angulo_ejex = input$angulo_ejex,        # Ángulo del eje X parametrizado
          .hjust_ejex = input$hjust_ejex,          # Ajuste horizontal parametrizado
          .xbreaks = xbreaks_fun
        ) +
          ggiraph::geom_point_interactive(
            mapping = aes(tooltip = paste0(input$cotizaciones_grafico_input, ": ", scales::comma(valores, accuracy = 0.01, big.mark = ".", decimal.mark = ","), "\n", "fecha: ", fecha)),
            size = input$size_tooltip  # Tamaño ajustable del tooltip
          )
        
        ggiraph::girafe(ggobj = plot_plt, width_svg = input$ancho_grafico, height_svg = input$largo_grafico)
      })
      
      output$cotizaciones_grafico_plt <- ggiraph::renderGirafe(cotizaciones_grafico_plt())
      
      output$download_data <- downloadHandler(
        filename = function() {
          paste("cotizaciones-", Sys.Date(), ".xlsx", sep = "")
        },
        content = function(file) {
          openxlsx::write.xlsx(cotizaciones_df(), file)
        }
      )
    }
  )
}


ui <- navbarPage("Cotizaciones", cotizaciones_UI("cotizaciones"))

server <- function(input, output, session) {
  cotizaciones_Server("cotizaciones", "main_tabset_cotizaciones")
}

shinyApp(ui = ui, server = server)