
cotizaciones_UI<- function(id, label = "Cotizaciones") {
  ns <- NS(id)
  
  tabPanel(
    "Cotizaciones",
    value="main_tabset_cotizaciones",
    sidebarLayout(
      sidebarPanel(
        width=3,
        fluidRow(
          selectizeInput(
            inputId=ns("cotizaciones_grafico_input"),
            choices = c(""),
            label="Entidades",
            # placeholder = "Introducir id de yahoo finance",
            multiple=TRUE,
            size = "100%",
            options = list(create = TRUE)
          ), 
          # textInput(ns("cotizaciones_grafico_input"), "Ingrese el ticker:", value = "AAPL"),
          checkboxInput(ns("base100"), "Mostrar en base 100", value = FALSE)
        )
      ),
      mainPanel(
        fluidRow(
          ggiraph::girafeOutput(outputId = ns("cotizaciones_grafico_plt"))
        ),
        fluidRow(
          column(width=6,
                textInput(
                  inputId=ns("prueba_textinput"),
                  label="Prueb"
                ) 
          ),
          column(width=6,
                textInput(
                  inputId=ns("prueba_textinput2"),
                  label="Prueb"
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
    .data_df <-  tryCatch(
        {
          tidyquant::tq_get(
            input$cotizaciones_grafico_input
          )
        },
        error = function(e) { message("Error descargando tickers: ", e)}
      )
    
    if(input$base100) {
      .data_df <- .data_df %>%
        mutate(across(where(is.numeric), ~ (.x / .x[1]) * 100))
    }
    
      
      return(.data_df)
    })
      
    cotizaciones_grafico_plt <- reactive({
      
      # browser()
      
      if(
        is.null(cotizaciones_df())
      ) {
        return(NULL)
      }
      
      plot_plt <- ggplot(
        data=cotizaciones_df(),
        mapping = aes(
          x=date,
          y=close,
          color=symbol
        )) + 
          ggiraph::geom_line_interactive(
            mapping=aes(
              tooltip = paste0(
                symbol
              )
            )
          ) + 
          ggiraph::geom_point_interactive(
            mapping=aes(
              tooltip = paste0(
                symbol, ": ", close, "\n",
                "fecha: ", date 
              )
            )
          )  
      
      
      ggiraph::girafe(ggobj=plot_plt)
      
    })  
    
    output$cotizaciones_grafico_plt <- ggiraph::renderGirafe(cotizaciones_grafico_plt())
      
    }
  )
}