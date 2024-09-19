# Cargar diccionarios ----

diccionario_bancos_df <- readxl::read_xlsx(path = paste0(datos_path, "diccionario_bancos_df.xlsx"), col_names = TRUE)
diccionario_ibex_df <- readxl::read_xlsx(path = paste0(datos_path, "diccionario_ibex_df.xlsx"), col_names = TRUE)
diccionario_paises_df <- readxl::read_xlsx(path = paste0(datos_path, "diccionario_paises_df.xlsx"), col_names = TRUE)
diccionario_EBA_series_df <- readxl::read_xlsx(path = paste0(datos_path, "diccionario_EBA_series_df.xlsx"), col_names = TRUE)
diccionario_metricas_bancos_df <- readxl::read_xlsx(path = paste0(datos_path, "diccionario_metricas_bancos_df.xlsx"), col_names = TRUE)


# Work Sans será la fuente que sustituye a Cabinet Grotesk
# colores_new <- c(
#   '#001e93',  # AZUL
#   '#ffbd4c',  # AMARILLO
#   '#6d6d6d',  # GRIS
#   '#000000',  # NEGRO
#   '#e6821e',  # NARANJA
#   '#5d8a00',  # VERDE_OSCURO
#   '#65db82',  # VERDE_CLARO
#   '#aab620',  # MOSTAZA
#   '#0cd7fb',  # AZUL_CIELO
#   '#9a7100',  # MARRON
#   '#24a669',  # VERDE
#   '#0086b6'   # AZUL_PROFUNDO
# )

# Función para Generar gráficos de lineas
generar_lineas_plot <- function(
    .data,
    .fecha="fecha",
    .valores="valores",
    .nombres="nombres",
    .title="",
    .xscale=1,
    .xaccuracy=1,
    .xsuffix="",
    .yscale=1,
    .yaccuracy=0.01,
    .ysuffix="",
    .grosor_linea=1.5,
    .linetype="dashed", # dotdash, longdash, dooted originalmente
    .angulo_ejex=0,
    .hjust_ejex=0.5,  # Nuevo parámetro para hjust
    .trans="",
    .xbreaks=NULL,
    .axis.line.x = TRUE,
    .axis.line.y = FALSE,
    .axis.line.size = 0.25,
    .debug = FALSE
) {
  # Crear el plot inicial con ggplot2 y geom_line
  plot_gg <- ggplot2::ggplot(data=.data, ggplot2::aes(x=!!as.symbol(.fecha), y=!!as.symbol(.valores), color=!!as.symbol(.nombres))) +
    ggplot2::geom_line(size = .grosor_linea) +
    ggplot2::labs(title=.title) +
    tesorotools::tema_gabinete_load() +
    theme(
      axis.text.x = element_text(angle = .angulo_ejex, hjust = .hjust_ejex),
      panel.grid.major = ggplot2::element_line(linetype = .linetype),
      legend.margin = margin(t = -8, r = 0, b = 0, l = 0),
      # axis.ticks.x = element_line(size = .axis.line.size) # El tick con el mismo tamaño que la línea horizontal
      # axis.ticks.length = unit(0, "cm")  # Cambia la longitud de los ticks
      ) +  # Se agrega hjust
    scale_color_manual(values = tesorotools::colores_new) +
    scale_y_continuous(position = "right",
                       labels = scales::comma_format(scale=.yscale, 
                                                     big.mark = ".",
                                                     decimal.mark = ",",
                                                     suffix=.ysuffix,
                                                     accuracy=.yaccuracy))
  
  # Líneas en el Eje x e y con grosor ajustable
  if (.axis.line.x){ 
    plot_gg <- plot_gg + theme(axis.line.x = element_line(size = .axis.line.size))
  }
  
  if (.axis.line.y){ 
    plot_gg <- plot_gg + theme(axis.line.y = element_line(size = .axis.line.size))
  }
  
  # Configurar los breaks del eje X para fechas
  if (!is.null(.xbreaks)) {
    plot_gg <- plot_gg + ggplot2::scale_x_date(breaks = .xbreaks, date_labels = "%Y-%m", expand = expansion(mult = c(0, 0)))
  } else {
    plot_gg <- plot_gg + ggplot2::scale_x_date(date_labels = "%Y-%m", expand = expansion(mult = c(0, 0)))
  }
  
  # Aplicar transformaciones si es necesario
  if (.trans != "") {
    plot_gg <- plot_gg +
      ggplot2::scale_x_date(trans = .trans, breaks = .xbreaks, date_labels = "%Y-%m", expand = expansion(mult = c(0, 0)))
  }
  
  return(plot_gg)
}

# Función para Generar gráficos de cascada
generar_cascada_plot <- function (
    .data = NULL, 
    valores, 
    labels, 
    rect_text_labels = valores, 
    rect_text_size = 10,  # Parametriza el tamaño de la letra de los valores
    rect_text_labels_anchor = "centre", 
    put_rect_text_outside_when_value_below = 0.05 * (max(cumsum(valores)) - min(cumsum(valores))), 
    calc_total = FALSE, 
    total_axis_text = "Total", 
    total_rect_text = sum(valores), 
    total_rect_color = "blue", 
    total_rect_border_color = NA, 
    total_rect_text_color = "white", 
    fill_colours = NULL, 
    fill_by_sign = TRUE, 
    positive_color = "lightgreen", 
    negative_color = "red", 
    start_color = "yellow", 
    end_color = "grey", 
    rect_width = 0.7, 
    rect_border = NA, 
    draw_lines = TRUE, 
    lines_color = "#D9E1FC",  # Parametrizando el color de las líneas discontinuas
    lines_anchors = c("right", "left"), 
    linetype = "dashed", 
    draw_axis.x = "behind", 
    theme_text_family = "", 
    scale_y_to_waterfall = TRUE, 
    print_plot = FALSE, 
    ggplot_object_name = "cascada_plot", 
    .xscale = 1, 
    .xaccuracy = 1, 
    .xsuffix = "", 
    .yscale = 1, 
    .yaccuracy = 1, 
    .ysuffix = ""
) {
  if (!is.null(.data)) {
    if (!is.data.frame(.data)) {
      stop(".data was a ", class(.data)[1], ", but must be a data.frame.")
    }
    if (ncol(.data) < 2L) {
      stop(".data had fewer than two columns, yet two are required: labels and valores.")
    }
    dat <- as.data.frame(.data)
    labels <- dat[[1]]
    valores <- dat[[2]]
  }
  if (!(length(valores) == length(labels) && length(valores) == length(rect_text_labels))) {
    stop("valores, labels, fill_colours, and rect_text_labels must all have same length")
  }
  if (rect_width > 1) 
    warning("rect_width > 1, your chart may look terrible")
  
  number_of_rectangles <- length(valores)
  north_edge <- cumsum(valores)
  south_edge <- c(0, cumsum(valores)[-length(valores)])
  
  # Ajuste de colores
  if (is.null(fill_colours)) {
    if (calc_total) {
      fill_colours <- c(
        start_color, 
        ifelse(valores[-c(1, length(valores))] >= 0, positive_color, negative_color), 
        end_color, 
        total_rect_color
      )
    } else {
      fill_colours <- c(
        start_color, 
        ifelse(valores[-c(1, length(valores))] >= 0, positive_color, negative_color), 
        end_color
      )
    }
  }
  
  rect_border_matching <- length(rect_border) == number_of_rectangles
  if (!(rect_border_matching || length(rect_border) == 1)) {
    stop("rect_border must be a single colour or one colour for each rectangle")
  }
  if (!(grepl("^[lrc]", lines_anchors[1]) && grepl("^[lrc]", lines_anchors[2]))) 
    stop("lines_anchors must be a pair of any of the following: left, right, centre")
  if (grepl("^l", lines_anchors[1])) 
    anchor_left <- rect_width / 2
  if (grepl("^c", lines_anchors[1])) 
    anchor_left <- 0
  if (grepl("^r", lines_anchors[1])) 
    anchor_left <- -1 * rect_width / 2
  if (grepl("^l", lines_anchors[2])) 
    anchor_right <- -1 * rect_width / 2
  if (grepl("^c", lines_anchors[2])) 
    anchor_right <- 0
  if (grepl("^r", lines_anchors[2])) 
    anchor_right <- rect_width / 2
  
  # Gráfico cuando calc_total es FALSE
  if (!calc_total) {
    p <- ggplot2::ggplot(data.frame(x = labels, y = valores), ggplot2::aes_string(x = "x", y = "y")) +
      ggplot2::geom_blank() + 
      ggplot2::theme(axis.title = ggplot2::element_blank()) +
      ggplot2::scale_x_discrete(labels = labels)
    
    # Ajustar los bordes norte y sur del último valor
    north_edge[number_of_rectangles] <- valores[number_of_rectangles]
    south_edge[number_of_rectangles] <- 0
  } 
  # Gráfico cuando calc_total es TRUE
  else {
    p <- ggplot2::ggplot(data.frame(x = c(labels, total_axis_text), y = c(valores, north_edge[number_of_rectangles])), 
                         ggplot2::aes_string(x = "x", y = "y")) +
      ggplot2::geom_blank() + 
      ggplot2::theme(axis.title = ggplot2::element_blank()) +
      ggplot2::scale_x_discrete(labels = c(labels, total_axis_text))
  }
  
  # Dibuja los rectángulos y las líneas discontínuas
  for (i in seq_along(valores)) {
    p <- p + ggplot2::annotate("rect", xmin = i - rect_width / 2, xmax = i + rect_width / 2, ymin = south_edge[i], ymax = north_edge[i], 
                               colour = rect_border[[if (rect_border_matching) i else 1]], fill = fill_colours[i])
    if (i > 1 && draw_lines) {
      p <- p + ggplot2::annotate("segment", x = i - 1 - anchor_left, xend = i + anchor_right, linetype = linetype, 
                                 y = south_edge[i], yend = south_edge[i], color = lines_color)
    }
  }
  
  # Añade la línea discontinua entre la penúltima y última barra
  if (!calc_total && draw_lines && number_of_rectangles > 1) {
    p <- p + ggplot2::annotate("segment", 
                               x = number_of_rectangles - 1 - anchor_left, 
                               xend = number_of_rectangles + anchor_right, 
                               y = north_edge[number_of_rectangles - 1], 
                               yend = north_edge[number_of_rectangles], 
                               linetype = linetype, color = lines_color)
  }
  
  # Añadir etiquetas de texto a los rectángulos
  for (i in seq_along(valores)) {
    label_val <- scales::number_format(scale = .yscale, suffix = .ysuffix, accuracy = .yaccuracy)(valores[i])
    if (abs(valores[i]) > put_rect_text_outside_when_value_below) {
      p <- p + ggplot2::annotate("text", x = i, y = 0.5 * (north_edge[i] + south_edge[i]), family = theme_text_family, 
                                 label = ifelse(rect_text_labels[i] == valores[i], label_val, rect_text_labels[i]), size = rect_text_size)
    } else {
      p <- p + ggplot2::annotate("text", x = i, y = north_edge[i], family = theme_text_family, 
                                 label = ifelse(rect_text_labels[i] == valores[i], label_val, rect_text_labels[i]), 
                                 vjust = ifelse(valores[i] >= 0, -0.2, 1.2), size = rect_text_size)
    }
  }
  
  # Dibuja la columna del total si calc_total es TRUE
  if (calc_total) {
    total_label_val <- scales::number_format(scale = .yscale, suffix = .ysuffix, accuracy = .yaccuracy)(total_rect_text)
    p <- p + ggplot2::annotate("rect", xmin = number_of_rectangles + 1 - rect_width / 2, xmax = number_of_rectangles + 1 + rect_width / 2, 
                               ymin = 0, ymax = north_edge[number_of_rectangles], colour = total_rect_border_color, fill = total_rect_color) + 
      ggplot2::annotate("text", x = number_of_rectangles + 1, y = 0.5 * north_edge[number_of_rectangles], family = theme_text_family, 
                        label = ifelse(total_rect_text == sum(valores), total_label_val, total_rect_text), color = total_rect_text_color, size = rect_text_size)
    
    if (draw_lines) {
      p <- p + ggplot2::annotate("segment", x = number_of_rectangles - anchor_left, 
                                 xend = number_of_rectangles + 1 + anchor_right, 
                                 y = north_edge[number_of_rectangles], 
                                 yend = north_edge[number_of_rectangles], linetype = linetype, color = lines_color)
    }
  }
  
  if (grepl("front", draw_axis.x)) {
    p <- p + ggplot2::geom_hline(yintercept = 0)
  }
  
  # Aplicar el tema personalizado y configurar el eje Y
  p <- p + tesorotools::tema_gabinete_load() +
    ggplot2::scale_y_continuous(labels = scales::number_format(scale = .yscale, suffix = .ysuffix, accuracy = .yaccuracy))
  
  # Mostrar o devolver el gráfico
  if (print_plot) {
    if (ggplot_object_name %in% ls(.GlobalEnv)) 
      warning("Overwriting ", ggplot_object_name, " in global environment.")
    assign(ggplot_object_name, p, inherits = TRUE)
    print(p)
  } else {
    return(p)
  }
}

# Función para guardar gráficos en PNG desde ggplot
guardar_graficos <- function(graficos, nombres, ancho, largo, archivo_word) {
  # browser()
  # Crear documento Word
  doc <- officer::read_docx()
  
  for (i in seq_along(graficos)) {
    if (!is.null(graficos[[i]])) {
      temp_file <- tempfile(fileext = ".png")
      cat("Generando gráfico temporal en:", temp_file, "\n")
      
      # Guardar el gráfico como PNG
      tryCatch({
        # Guardar el gráfico en un archivo temporal
        ggsave(filename = temp_file, plot = graficos[[i]], width = ancho, height = largo, dpi = 300)
        
        # Agregar el nombre del gráfico al documento
        doc <- doc %>% body_add_par(nombres[i], style = "heading 1")
        
        # Insertar la imagen del gráfico en el documento
        doc <- doc %>% body_add_img(src = temp_file, width = 5, height = 5)
        
      }, error = function(e) {
        cat("Error al guardar el gráfico:", e$message, "\n")
      })
    } else {
      cat("Gráfico no generado para:", nombres[i], "\n")
    }
  }
  
  # Guardar el documento Word final
  print(doc, target = archivo_word)
}



