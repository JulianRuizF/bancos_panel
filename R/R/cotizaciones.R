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
          downloadButton(outputId = ns("download_data"), label = "Guardar en Excel", class = "btn-lg btn-block")
        )
      ),
      mainPanel(
        fluidRow(
          ggiraph::girafeOutput(outputId = ns("cotizaciones_grafico_plt"))
        ),
        fluidRow(
          column(width = 6,
                 textInput(
                   inputId = ns("prueba_textinput"),
                   label = "Prueba"
                 )
          ),
          column(width = 6,
                 textInput(
                   inputId = ns("prueba_textinput2"),
                   label = "Prueba"
                 )
          )
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
            group_by(nombres) %>% # Si se hace group_by tiene que estar en el diccionario
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
        
        plot_plt <- generar_lineas_plot(
          .data = cotizaciones_df(),
          .fecha = "fecha",
          .valores = "valores",
          .nombres = "nombres",
          .yaccuracy=0.01,
          .ysuffix="", # Se puede poner %
          .trans="",
          .xbreaks=NULL
        ) +
          # plot_plt <- ggplot(
          #   data = cotizaciones_df(),
          #   mapping = aes(
          #     x = fecha,
          #     y = valores,
          #     color = nombres
          #   )) +
          ggiraph::geom_line_interactive(
            mapping = aes(
              tooltip = paste0(nombres, ": ", valores, "\n",
                               "fecha: ", fecha)
            )
          ) +
          ggiraph::geom_point_interactive(
            mapping = aes(
              tooltip = paste0(
                nombres, ": ", sprintf("%.2f", valores), "\n",
                "fecha: ", fecha 
              )
            )
          )
        
        ggiraph::girafe(ggobj = plot_plt)
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

ui <- navbarPage(
  "Cotizaciones",
  cotizaciones_UI("cotizaciones")
)

server <- function(input, output, session) {
  cotizaciones_Server("cotizaciones", "main_tabset_cotizaciones")
}

shinyApp(ui = ui, server = server)