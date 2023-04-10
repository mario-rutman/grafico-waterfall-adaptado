library(dplyr)
library(ggplot2)

# 1. Função waterfalls adaptada. ------------------------------------------

# Tenho usado a função para gráfico de cascata para demonstras as variações nas quantidade de processos, porém entendi que foram necessárias muitas adaptações.
# Algumas foram após a aplicação da waterfalls, entendi que deveria fazer outras modificaçôes que só foram possíveis usando o próprio script da função watarfalls original.
# Assim nasceu a função waterfall_adaptado.

# A função waterfall origina está em...
# https://github.com/HughParsonage/waterfalls/blob/master/R/waterfall.R

waterfall_adaptado <- function(.data = NULL,
                      values, labels,
                      rect_text_labels = values,
                      # Vou testar com os valores 1.2, 1.5 etc.
                      rect_text_size = 1.6,
                      # Podemos usar outros valores.
                      # "center", "top", "bottom", "left", "right", "topleft", "topright", "bottomleft", e "bottomright"
                      rect_text_labels_anchor = "center",
                      put_rect_text_outside_when_value_below = 0.05*(max(cumsum(values)) - min(cumsum(values))),
                      calc_total = FALSE,
                      total_axis_text = "Total",
                      total_rect_text = sum(values),
                      total_rect_color = "black",
                      total_rect_border_color = "black",
                      total_rect_text_color = "white",
                      fill_colours = NULL,
                      fill_by_sign = TRUE,
                      rect_width = 0.7,
                      rect_border = "black",
                      draw_lines = TRUE,
                      lines_anchors = c("right", "left"),
                      # Outros valores solid, dashed, dotted, dotdash, longdash. twodash
                      linetype = "solid",
                      draw_axis.x = "behind",
                      theme_text_family = "",
                      scale_y_to_waterfall = TRUE,
                      print_plot = FALSE,
                      ggplot_object_name = "mywaterfall") {
  if (!is.null(.data)) {
    
    if (!is.data.frame(.data)) {
      stop("`.data` was a ", class(.data)[1], ", but must be a data.frame.")
    }
    
    if (ncol(.data) < 2L) {
      stop("`.data` had fewer than two columns, yet two are required: labels and values.")
    }
    
    dat <- as.data.frame(.data)
    char_cols <- vapply(dat, is.character, FALSE)
    factor_cols <- vapply(dat, is.factor, FALSE)
    num_cols <- vapply(dat, is.numeric, FALSE)
    
    if (!xor(num_cols[1], num_cols[2]) ||
        sum(char_cols[1:2], factor_cols[1:2], num_cols[1:2]) != 2L) {
      const_width_name <- function(noms) {
        if (is.data.frame(noms)) {
          noms <- names(noms)
        }
        max_width <- max(nchar(noms))
        formatC(noms, width = max_width)
      }
      
      stop("`.data` did not contain exactly one numeric column and exactly one character or factor ",
           "column in its first two columns.\n\t", 
           "1st column: '", const_width_name(dat)[1], "'\t", sapply(dat, class)[1], "\n\t",
           "2nd column: '", const_width_name(dat)[2], "'\t", sapply(dat, class)[2])
    }
    
    if (num_cols[1L]) {
      .data_values <- .subset2(dat, 1L)
      .data_labels <- .subset2(dat, 2L)
    } else {
      .data_values <- .subset2(dat, 2L)
      .data_labels <- .subset2(dat, 1L)
    }
    
    if (!missing(values) && !missing(labels)) {
      warning(".data and values and labels supplied, .data ignored")
    } else {
      values <- .data_values
      labels <- as.character(.data_labels)
    }
  }
  
  if (!(length(values) == length(labels) &&
        length(values) == length(rect_text_labels))) {
    stop("values, labels, fill_colours, and rect_text_labels must all have same length")
  }
  
  if (rect_width > 1)
    warning("rect_Width > 1, your chart may look terrible")
  
  number_of_rectangles <- length(values)
  north_edge <- cumsum(values)
  south_edge <- c(0, cumsum(values)[-length(values)])
  
  # fill by sign means rectangles' fill colour is given by whether they are going up or down
  gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    grDevices::hcl(h = hues, l = 65, c = 100)[seq_len(n)]
  }
  if(fill_by_sign){
    if (!is.null(fill_colours)){
      warning("fill_colours is given but fill_by_sign is TRUE so fill_colours will be ignored.")
    }
    fill_colours <- ifelse(values >= 0,
                           gg_color_hue(2)[2],
                           gg_color_hue(2)[1])
  } else {
    if (is.null(fill_colours)){
      fill_colours <- gg_color_hue(number_of_rectangles)
    }
  }
  
  # Check if length of rectangle border colors matches the number of rectangles
  rect_border_matching <- length(rect_border) == number_of_rectangles
  if (!(rect_border_matching || length(rect_border) == 1)) {
    stop("rect_border must be a single colour or one colour for each rectangle")
  }
  
  if(!(grepl("^[lrc]", lines_anchors[1]) && grepl("^[lrc]", lines_anchors[2])))  # left right center
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
  
  if (!calc_total) {
    p <- 
      if (scale_y_to_waterfall) {
        ggplot2::ggplot(data.frame(x = c(labels, labels),
                                   y = c(south_edge, north_edge)),
                        ggplot2::aes_string(x = "x", y = "y")) 
      } else {
        ggplot2::ggplot(data.frame(x = labels, y = values),
                        ggplot2::aes_string(x = "x", y = "y"))
      }
    p <- p +
      ggplot2::geom_blank() +
      ggplot2::theme(axis.title = ggplot2::element_blank())
  } else {
    p <-
      if (scale_y_to_waterfall) {
        ggplot2::ggplot(data.frame(x = c(labels, total_axis_text,
                                         labels, total_axis_text),
                                   y = c(south_edge, north_edge,
                                         south_edge[number_of_rectangles],
                                         north_edge[number_of_rectangles])),
                        ggplot2::aes_string(x = "x", y = "y"))
      } else {
        ggplot2::ggplot(data.frame(x = c(labels, total_axis_text),
                                   y = c(values, north_edge[number_of_rectangles])),
                        ggplot2::aes_string(x = "x", y = "y"))
      } 
    p <- p +
      ggplot2::geom_blank() +
      ggplot2::theme(axis.title = ggplot2::element_blank())
  }
  
  if (grepl("behind", draw_axis.x)){
    p <- p + ggplot2::geom_hline(yintercept = 0)
  }
  
  for (i in seq_along(values)){
    p <- p + ggplot2::annotate("rect",
                               xmin = i - rect_width/2,
                               xmax = i + rect_width/2,
                               ymin = south_edge[i],
                               ymax = north_edge[i],
                               colour = rect_border[[if (rect_border_matching) i else 1]],
                               fill = fill_colours[i])
    if (i > 1 && draw_lines){
      p <- p + ggplot2::annotate("segment",
                                 x = i - 1 - anchor_left,
                                 xend = i + anchor_right,
                                 linetype = linetype,
                                 y = south_edge[i],
                                 yend = south_edge[i])
    }
  }
  
  # rect_text_labels
  
  for (i in seq_along(values)){
    if(abs(values[i]) > put_rect_text_outside_when_value_below){
      p <- p + ggplot2::annotate("text",
                                 x = i,
                                 y = 0.5 * (north_edge[i] + south_edge[i]),
                                 family = theme_text_family,
                                 label = ifelse(rect_text_labels[i] == values[i],
                                                ifelse(values[i] < 0,
                                                       paste0("\U2212", -1 * values[i]),
                                                       values[i]),
                                                rect_text_labels[i]),
                                 size = rect_text_size/(5/14),
                                 fontface = "bold") # Fazendo negrito.
    } else {
      p <- p + ggplot2::annotate("text",
                                 x = i,
                                 y = north_edge[i],
                                 family = theme_text_family,
                                 label = ifelse(rect_text_labels[i] == values[i],
                                                ifelse(values[i] < 0,
                                                       paste0("\U2212", -1 * values[i]),
                                                       values[i]),
                                                rect_text_labels[i]),
                                 vjust = ifelse(values[i] >= 0, -0.2, 1.2),
                                 size = rect_text_size/(5/14),
                                 fontface = "bold") # Fazendo negrito.
    }
  }
  
  
  if (calc_total){
    p <- p + ggplot2::annotate("rect",
                               xmin = number_of_rectangles + 1 - rect_width/2,
                               xmax = number_of_rectangles + 1 + rect_width/2,
                               ymin = 0,
                               ymax = north_edge[number_of_rectangles],
                               colour = total_rect_border_color,
                               fill = total_rect_color)  +
      ggplot2::annotate("text",
                        x = number_of_rectangles + 1,
                        y = 0.5 * north_edge[number_of_rectangles],
                        family = theme_text_family,
                        label = ifelse(total_rect_text == sum(values),
                                       ifelse(north_edge[number_of_rectangles] < 0,
                                              paste0("\U2212", -1 * north_edge[number_of_rectangles]),
                                              north_edge[number_of_rectangles]),
                                       total_rect_text),
                        color = total_rect_text_color,
                        size = rect_text_size/(5/14),
                        fontface = "bold") + # Fazendo negrito. 
      ggplot2::scale_x_discrete(labels = c(labels, total_axis_text))
    if (draw_lines){
      p <- p + ggplot2::annotate("segment",
                                 x = number_of_rectangles - anchor_left,
                                 xend = number_of_rectangles + 1 + anchor_right,
                                 y = north_edge[number_of_rectangles],
                                 yend = north_edge[number_of_rectangles],
                                 linetype = linetype)
    }
  } else {
    p <- p + ggplot2::scale_x_discrete(labels = labels)
  }
  
  if (grepl("front", draw_axis.x)){
    p <- p + ggplot2::geom_hline(yintercept = 0)
  }
  if (print_plot){
    # Allow modifications beyond the function call
    if (ggplot_object_name %in% ls(.GlobalEnv))
      warning("Overwriting ", ggplot_object_name, " in global environment.")
    assign(ggplot_object_name, p, inherits = TRUE)
    print(p)
  } else {
    return(p)
  }
}


# 2. Usando a waterfall_adaptado na faz_grafico_cascata_processos.--------

# Aqui uso as adaptações feitas em waterfall_adaptado para cria a função faz_grafico_cascata_procesos.

faz_grafico_cascata_processos <- function(audr) {
  # Pegando o valor dos EI.
  jto <- grafico_cascata_proc_sei |> 
    filter(caixa_SEI == audr) |>
    select(variacao) |> 
    slice(1) |> 
    pull()
  
  # Fazendo o gráfico.
  grafico_cascata_proc_sei |>
    filter(caixa_SEI == audr) |> 
    select(periodo, variacao) |> 
    waterfall_adaptado(calc_total = TRUE,
              fill_by_sign = FALSE,
              fill_colours = c("red","red","green", "red","green",
                                    "red", "green", "red","green"),
                                    total_rect_color = "#dd4814",
              total_rect_text_color = "black") +
    labs(title = paste("Estoque, entradas e saídas de processos na caixa da",audr),
         caption = "fonte: SEI-RJ/Estatísticas") +
    geom_hline(yintercept = jto*0.75, linetype = "dotted", size = 0.8, color="black", alpha = 0.7) +
    # annotate("text", x = "EI mar-23", y = jto*0.9, label = "-10%", vjust = -0.35, hjust = -10) +
    geom_hline(yintercept = jto*0.5, linetype = "dotted", size = 0.8, color="black", alpha = 0.7) +
    # annotate("text", x = "EI mar-23", y = jto*0.8, label = "-20%", vjust = -0.35, hjust = -10) +
    geom_hline(yintercept = jto*0.25, linetype = "dotted", size = 0.8, color="black", alpha = 0.7) +
    #annotate("text", x = "EI mar-23", y = jto*0.7, label = "-30%", vjust = -0.35, hjust = -10) +
    scale_x_discrete(labels=c("Estoque Inicial\nmar-23", "Entradas\nmar-23","Saídas\nmar-23",
                              "Entradas\nabr-23","Saídas\nabr-23",
                              "Entradas\nmai-23","Saídas\nmai-23",
                              "Entradas\njun-23","Saídas\njun-23","Estoque\nFinal")) +
    theme_bw() +
    theme(axis.title = element_blank(),
          axis.text.x = element_text(size = 12.8, colour = "black"),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          
          plot.title = element_text(size=24),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank())
}

# Quanto ao estoque de processos o gráfico indica...
# Quanto a fluxo, mostra...

# Exemplo.
faz_grafico_cascata_processos("07.01")
faz_grafico_cascata_processos("64.12")
faz_grafico_cascata_processos("DAC")
faz_grafico_cascata_processos("33.01")
faz_grafico_cascata_processos("SUACO")
