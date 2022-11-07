################################################################################
# this script contains functions used in other scripts in this project
################################################################################
save.plot <- function(plot.name) {
  ggsave(paste0(dir.assets, .Platform$file.sep, plot.name), bg = "white", dpi = 400, scale = 2.5)
}

gg_boxplot <- function(data, column, ylab = "", xlab = "", title = "", x.ticks = FALSE, offset = NULL) {
  #'
  #'
  #' @param data
  #' @param column
  #' @param ylab
  #' @param xlab
  #' @param title
  #' @param x.ticks
  #' @param offset
  #'
  if (is.null(offset)) {
    offset <- seq(from = min(data[, ..column], na.rm = TRUE), to = max(data[, ..column], na.rm = TRUE), len = 25)
    offset <- offset[2] - offset[1]
  }
  
  quantiles <- fivenum(unlist(data[, ..column]))
  plt <- ggplot(data) +
    geom_boxplot(aes(y = unlist(data[, ..column]))) +
    ylab(ylab) +
    xlab(xlab) +
    ggtitle(title) +
    annotate("text",  y = quantiles[2] + offset, x = -0.2,
             label = round(quantiles[2], 2),
             color = "red", fontface = "bold") +
    annotate("text",  y = quantiles[3] + offset, x = -0.2,
             label = round(quantiles[3], 2),
             color = "red", fontface = "bold") +
    annotate("text",  y = quantiles[4] + offset, x = -0.2,
             label = round(quantiles[4], 2),
             color = "red", fontface = "bold") +
    default_theme
  if (!x.ticks) {
    plt <- plt +
      theme(axis.ticks.x = element_blank(),
            axis.text.x = element_blank())
  }
  plt
}
gg_histogram <- function(data, column, interval = FALSE, interval.devisor = 1, fill = NULL) {
  #'
  #'
  #' @param data
  #' @param column
  #' @param interval
  #' @param interval.devisor
  #'
  #'
  #'
  if (interval) {
    assertString(column, any.missing = FALSE, len = 2)
    #TODO: assert time data
    start <- column[1]
    end <- column[2]
    interval(data$`T-date`, data$`T-dls`)
    str(data$`T-date`)
    str(as.vector(data[, ..start]))
    as.Date(unlist(data[, ..start]), origin = 0)
    
    interval(as.vector(data[, ..start]), as.vector(data[, ..end]))
    
    data[, test := as.Date(..start)]
    
    data.interval <- interval(start, end) / interval.divisor
  } else {
    assertString(column, any.missing = FALSE, len = 1)
    assert(all(column %in% colnames(data)))
  }
  if (!is.null(fill)) {
    assertString(fill, any.missing = FALSE, len = 1)
    assert(fill %in% colnames(data))
  }
  ggplot(data) +
    geom_histogram(mapping = aes(x = unlist(data[, ..start])))
}
