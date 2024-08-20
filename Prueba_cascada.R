generar_cascada_plot <- function (
    .data = NULL, 
    valores, 
    labels, 
    rect_text_labels = valores, 
    rect_text_size = 1, 
    rect_text_labels_anchor = "centre", 
    put_rect_text_outside_when_value_below = 0.05 * (max(cumsum(valores)) - min(cumsum(valores))), 
    calc_total = FALSE, 
    total_axis_text = "Total", 
    total_rect_text = sum(valores), 
    total_rect_color = "blue", 
    total_rect_border_color = "black", 
    total_rect_text_color = "white", 
    fill_colours = NULL, 
    fill_by_sign = TRUE, 
    positive_color = "lightgreen", 
    negative_color = "red", 
    start_color = "yellow", 
    end_color = "grey", 
    rect_width = 0.7, 
    rect_border = "black", 
    draw_lines = TRUE, 
    lines_anchors = c("right", "left"), 
    linetype = "dashed", 
    draw_axis.x = "behind", 
    theme_text_family = "", 
    scale_y_to_waterfall = TRUE, 
    print_plot = FALSE, 
    ggplot_object_name = "cascada_plot", 
    .xscale=1, 
    .xaccuracy=1, 
    .xsuffix="", 
    .yscale=1, 
    .yaccuracy=1, 
    .ysuffix=""
) {
  if (!is.null(.data)) {
    if (!is.data.frame(.data)) {
      stop(".data was a ", class(.data)[1], ", but must be a data.frame.")
    }
    if (ncol(.data) < 2L) {
      stop(".data had fewer than two columns, yet two are required: labels and valores.")
    }
    dat <- as.data.frame(.data)
    char_cols <- vapply(dat, is.character, FALSE)
    factor_cols <- vapply(dat, is.factor, FALSE)
    num_cols <- vapply(dat, is.numeric, FALSE)
    if (!xor(num_cols[1], num_cols[2]) || sum(char_cols[1:2], factor_cols[1:2], num_cols[1:2]) != 2L) {
      const_width_name <- function(noms) {
        if (is.data.frame(noms)) {
          noms <- names(noms)
        }
        max_width <- max(nchar(noms))
        formatC(noms, width = max_width)
      }
      stop(".data did not contain exactly one numeric column and exactly one character or factor column in its first two columns.\n\t", 
           "1st column: '", const_width_name(dat)[1], "'\t", sapply(dat, class)[1], "\n\t", 
           "2nd column: '", const_width_name(dat)[2], "'\t", sapply(dat, class)[2])
    }
    if (num_cols[1L]) {
      .data_valores <- .subset2(dat, 1L)
      .data_labels <- .subset2(dat, 2L)
    }
    else {
      .data_valores <- .subset2(dat, 2L)
      .data_labels <- .subset2(dat, 1L)
    }
    if (!missing(valores) && !missing(labels)) {
      warning(".data and valores and labels supplied, .data ignored")
    }
    else {
      valores <- .data_valores
      labels <- as.character(.data_labels)
    }
  }
  if (!(length(valores) == length(labels) && length(valores) == length(rect_text_labels))) {
    stop("valores, labels, fill_colours, and rect_text_labels must all have same length")
  }
  if (rect_width > 1) 
    warning("rect_width > 1, your chart may look terrible")
  
  number_of_rectangles <- length(valores)
  north_edge <- cumsum(valores)
  south_edge <- c(0, cumsum(valores)[-length(valores)])
  
  if (is.null(fill_colours)) {
    fill_colours <- c(start_color, ifelse(valores[-c(1, length(valores))] >= 0, positive_color, negative_color), end_color)
    if (calc_total) {
      fill_colours <- c(start_color, ifelse(valores[-c(1, length(valores) - 1)] >= 0, positive_color, negative_color), total_rect_color)
    }
  }
  
  rect_border_matching <- length(rect_border) == number_of_rectangles
  if (!(rect_border_matching || length(rect_border) == 1)) {
    stop("rect_border must be a single colour or one colour for each rectangle")
  }
  # Si calc_total es False (por defecto)
  if (!calc_total) {
    p <- if (scale_y_to_waterfall) {
      ggplot2::ggplot(data.frame(x = c(labels, labels), y = c(south_edge, north_edge)), 
                      ggplot2::aes_string(x = "x", y = "y"))
    }
    else {
      ggplot2::ggplot(data.frame(x = labels, y = valores), ggplot2::aes_string(x = "x", y = "y"))
    }
    p <- p + ggplot2::geom_blank() + ggplot2::theme(axis.title = ggplot2::element_blank())
  }
  else {
    p <- if (scale_y_to_waterfall) {
      ggplot2::ggplot(data.frame(x = c(labels, total_axis_text, labels, total_axis_text), 
                                 y = c(south_edge, north_edge, south_edge[number_of_rectangles], 
                                       north_edge[number_of_rectangles])), ggplot2::aes_string(x = "x", y = "y"))
    }
    else {
      ggplot2::ggplot(data.frame(x = c(labels, total_axis_text), y = c(valores, north_edge[number_of_rectangles])), 
                      ggplot2::aes_string(x = "x", y = "y"))
    }
    p <- p + ggplot2::geom_blank() + ggplot2::theme(axis.title = ggplot2::element_blank())
  }
  if (grepl("behind", draw_axis.x)) {
    p <- p + ggplot2::geom_hline(yintercept = 0)
  }
  for (i in seq_along(valores)) {
    p <- p + ggplot2::annotate("rect", xmin = i - rect_width/2, xmax = i + rect_width/2, ymin = south_edge[i], ymax = north_edge[i], 
                               colour = rect_border[[if (rect_border_matching) i else 1]], fill = fill_colours[i])
    if (i > 1 && draw_lines) {
      p <- p + ggplot2::annotate("segment", x = i - 1 - anchor_left, xend = i + anchor_right, linetype = linetype, 
                                 y = south_edge[i], yend = south_edge[i])
    }
  }
  for (i in seq_along(valores)) {
    label_val <- scales::number_format(scale = .yscale, suffix = .ysuffix, accuracy = .yaccuracy)(valores[i])
    if (abs(valores[i]) > put_rect_text_outside_when_value_below) {
      p <- p + ggplot2::annotate("text", x = i, y = 0.5 * (north_edge[i] + south_edge[i]), family = theme_text_family, 
                                 label = ifelse(rect_text_labels[i] == valores[i], label_val, rect_text_labels[i]), size = rect_text_size/(5/14))
    }
    else {
      p <- p + ggplot2::annotate("text", x = i, y = north_edge[i], family = theme_text_family, 
                                 label = ifelse(rect_text_labels[i] == valores[i], label_val, rect_text_labels[i]), 
                                 vjust = ifelse(valores[i] >= 0, -0.2, 1.2), size = rect_text_size/(5/14))
    }
  }
  # Si calc_total es TRUE (no defecto)
  if (calc_total) {
    total_label_val <- scales::number_format(scale = .yscale, suffix = .ysuffix, accuracy = .yaccuracy)(total_rect_text)
    p <- p + ggplot2::annotate("rect", xmin = number_of_rectangles + 1 - rect_width/2, xmax = number_of_rectangles + 1 + rect_width/2, 
                               ymin = 0, ymax = north_edge[number_of_rectangles], colour = total_rect_border_color, fill = total_rect_color) + 
      ggplot2::annotate("text", x = number_of_rectangles + 1, y = 0.5 * north_edge[number_of_rectangles], family = theme_text_family, 
                        label = ifelse(total_rect_text == sum(valores), total_label_val, total_rect_text), color = total_rect_text_color, size = rect_text_size/(5/14)) + 
      ggplot2::scale_x_discrete(labels = c(labels, total_axis_text))
    if (draw_lines) {
      p <- p + ggplot2::annotate("segment", x = number_of_rectangles - anchor_left, xend = number_of_rectangles + 1 + anchor_right, 
                                 y = north_edge[number_of_rectangles], yend = north_edge[number_of_rectangles], linetype = linetype)
    }
    # Si calc_total es FALSE (defecto)
    if (!calc_total) {
      # Agregar un rectángulo para el último valor cuando calc_total es FALSE
      p <- p + ggplot2::annotate("rect", xmin = number_of_rectangles - rect_width/2, xmax = number_of_rectangles + rect_width/2, 
                                 ymin = 0, ymax = north_edge[number_of_rectangles -1], colour = rect_border[[if (rect_border_matching) number_of_rectangles else 1]], fill = fill_colours[number_of_rectangles]) +
        ggplot2::annotate("text", x = number_of_rectangles, y = 0.5 * north_edge[number_of_rectangles], family = theme_text_family, 
                          label = sum(valores), color = total_rect_text_color, size = rect_text_size/(5/14)) + 
        ggplot2::scale_x_discrete(labels = labels)
    }
    if (grepl("front", draw_axis.x)) {
      p <- p + ggplot2::geom_hline(yintercept = 0)
    }
    p <- p + tesorotools::tema_gabinete_load() +
      ggplot2::scale_y_continuous(labels = scales::number_format(scale = .yscale, suffix = .ysuffix, accuracy = .yaccuracy))
    
    if (print_plot) {
      if (ggplot_object_name %in% ls(.GlobalEnv)) 
        warning("Overwriting ", ggplot_object_name, " in global environment.")
      assign(ggplot_object_name, p, inherits = TRUE)
      print(p)
    }
    else {
      return(p)
    }
  }
}  
  
generar_cascada_plot(.data, calc_total = F)