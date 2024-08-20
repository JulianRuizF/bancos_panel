source("utilidades.R") # datos si se ejecuta desde aquí

# cascada_UI.R
cascada_UI <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    "Cascada",
    value = "main_tabset_cascada",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        textInput(ns("labels"), "Labels", value = "BN 1Q2024,Ingres. inter.,Gastos inter.,Grava men,Gastos gles.,Provisio.,Otros,BN 2Q2024"),
        textInput(ns("valores"), "Valores", value = "201,38,-33,95,17,-14,3,273"),
        checkboxInput(ns("generar_otros"), "Generar Otros", value = FALSE),
        checkboxInput(ns("calc_total"), "Calcular Total", value = FALSE),
        textInput(ns("positive_color"), "Color Positivo", value = "lightgreen"),
        textInput(ns("negative_color"), "Color Negativo", value = "red"),
        textInput(ns("start_color"), "Color Inicial", value = "yellow"),
        textInput(ns("end_color"), "Color Final", value = "grey"),
        textInput(ns("total_rect_color"), "Color Total", value = "blue"),
        textInput(ns("total_axis_text"), "Texto Total", value = "Total"),
        numericInput(ns("yscale"), "Escala Y", value = 1),
        numericInput(ns("yaccuracy"), "Precisión Y", value = 1),
        textInput(ns("ysuffix"), "Sufijo Y", value = " M"),
        numericInput(ns("rect_width"), "Ancho del Rectángulo", value = 0.7, min = 0.1, max = 1, step = 0.1),
        numericInput(ns("plot_width"), "Ancho del Gráfico", value = 700),
        numericInput(ns("plot_height"), "Alto del Gráfico", value = 400),
        fluidRow(
          column(4, numericInput(ns("x_axis_size"), "Tamaño X", value = 10)),
          column(4, numericInput(ns("y_axis_size"), "Tamaño  Y", value = 10)),
          column(4, numericInput(ns("values_size"), "Tamaño Valores", value = 5))
        )
      ),
      mainPanel(
        fluidRow(
          plotOutput(ns("cascada_plot"))
        ) 
      )
    )
  )
}













cascada_Server <- function(id, tabset_id = NULL) {
  moduleServer(
    id, 
    function(input, output, session) {
      ns <- session$ns
      
      # Reactive para procesar los datos basados en los inputs
      cascada_df <- reactive({
        req(input$labels, input$valores)
        
        labels <- unlist(strsplit(input$labels, ","))
        valores <- as.numeric(unlist(strsplit(input$valores, ",")))
        
        if(length(labels) != length(valores)) {
          showNotification("La cantidad de etiquetas y valores no coinciden.", type = "error")
          return(NULL)
        }
        
        # Crear un tibble con los datos
        tibble::tibble(labels = labels, valores = valores)
      })
      
      # Reactive para calcular la diferencia y crear la categoría "Otros"
      cascada_df_otros <- reactive({
        .data <- cascada_df()
        req(.data)
        
        total_valores <- sum(.data$valores[-length(.data$valores)])
        ultimo_valor <- .data$valores[length(.data$valores)]
        
        if (total_valores != ultimo_valor) {
          otros_valor <- ultimo_valor - total_valores
          .data <- .data %>% 
            add_row(labels = "Otros", valores = otros_valor, .before = nrow(.data))
        }
        
        return(.data)
      })
      
      # Reactive para generar el gráfico
      cascada_plot <- reactive({
        .data <- if (input$generar_otros) cascada_df_otros() else cascada_df()
        
        # Asegurarse de que los datos no sean NULL
        req(.data)
        
        plot <- generar_cascada_plot(
          .data = .data,
          valores = .data$valores,
          labels = .data$labels,
          calc_total = input$calc_total,
          positive_color = input$positive_color,
          negative_color = input$negative_color,
          start_color = input$start_color,
          end_color = input$end_color,
          total_rect_color = input$total_rect_color,
          total_axis_text = input$total_axis_text,
          rect_width = input$rect_width,
          .yscale = input$yscale,
          .yaccuracy = input$yaccuracy,
          .ysuffix = input$ysuffix,
          rect_text_size = input$values_size,
          theme_text_family = "Work Sans"
        )
        
        plot + 
          theme(
            axis.text.x = element_text(size = input$x_axis_size),
            axis.text.y = element_text(size = input$y_axis_size),
            text = element_text(size = input$values_size)
          )
      })
      
      # Renderizar el gráfico de cascada
      output$cascada_plot <- renderPlot({
        cascada_plot()
      }, width = reactive(input$plot_width), height = reactive(input$plot_height))
      
      # Utilizar el tabset_id para acciones específicas de la pestaña si es necesario
      observe({
        if (!is.null(tabset_id)) {
          current_tab <- tabset_id()
          # Realiza acciones específicas basadas en la pestaña actual
        }
      })
    }
  )
}




ui <- navbarPage(
  "Aplicación para hacer Cascadas-Bridges",
  cascada_UI("cascada")
)

server <- function(input, output, session) {
  cascada_Server("cascada", "main_tabset_cascada")
}

shinyApp(ui = ui, server = server)