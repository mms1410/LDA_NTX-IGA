################################################################################
# this script contains functions used in other scripts in this project
################################################################################
save.plot <- function(plot.name) {
  # TODO check .jpg
  ggsave(paste0(dir.assets, .Platform$file.sep, plot.name), bg = "white", dpi = 400, scale = 2.5)
}

write.summary <- function(data, file.name) {
  #'
  #'
  #'
  #'
  #'
  data.numeric <- data[, .SD, .SDcols = is.numeric]
  data.factor <- data[, .SD, .SDcols = is.factor]
  rstatix::get_summary_stats(data.numeric)
  
  data.table(psych::describe(data, na.rm = TRUE))[, vars := colnames(data)][]
  
  data.table(psych::describe(data, na.rm = TRUE))[, vars := colnames(data)][] %>% 
    fwrite(file = paste0(dir.assets, .Platform$file.sep, "summary_numeric_iga_all.csv"))
}

gg.boxplot <- function(data, y.column, x.column = NULL, ylab = "", xlab = "", title = "", x.ticks = NULL, offset = NULL, annotate.quantile = FALSE, ylims = NULL) {
  # TODO:
  #' ... for additional ggplot commands?
  #'
  #'
  #' @param data
  #' @param y.column
  #' @param x.column
  #' @param ylab
  #' @param xlab
  #' @param title
  #' @param x.ticks
  #' @param offst
  #'
  if (!is.null(x.ticks)) {
    assert(!is.null(x.column), "x.column must be provided with x.ticks")
    x.levels <-levels(unlist(data[, ..x.column]))
    assert(length(x.levels) == length(x.ticks), "Length provided by x.ticks and levels of x.columns differ.")
    assertNamed(x.ticks, type = "named", "x.ticks must be a named vector (<level> = 'name'")
    assert(all(names(x.ticks) %in% x.levels), "x.ticks requires named vector with exact match of x.column levels.")
    ## reorder
    x.ticks <- unname(x.ticks[order(names(x.ticks), x.levels)])
  }
  
  if (is.null(offset)) {
    offset <- seq(from = min(data[, ..y.column], na.rm = TRUE), to = max(data[, ..y.column], na.rm = TRUE), len = 25)
    offset <- offset[2] - offset[1]
  }
  
  if (!is.null(x.column)) {
    plt <- ggplot(data) +
      geom_boxplot(aes(y = unlist(data[, ..y.column]),
                               x = unlist(data[, ..x.column]))) +
      default_theme
    if (!is.null(x.ticks)) {
      plt <- plt + scale_x_discrete(labels = x.ticks)
    }
  } else {
    plt <- ggplot(data) +
      geom_boxplot(aes(y = unlist(data[, ..y.column]))) +
      default_theme
  }
  
  quantiles <- fivenum(unlist(data[, ..y.column]))
  plt <- plt + 
    ylab(ylab) +
    xlab(xlab) +
    ggtitle(title)
  if (!is.null(ylims)) {
    plt <- plt + ylim(ylims)
  }
  
  if (annotate.quantile) {
    plt <- plt +
      scale_y_continuous(breaks = unname(quantiles)) +
      theme(axis.text.y = element_text(color = "red", size = 11))
    
      #annotate("text",  y = quantiles[2] + offset, x = -0.2,
      #         label = round(quantiles[2], 2),
      #         color = "red", fontface = "bold") +
      #annotate("text",  y = quantiles[3] + offset, x = -0.2,
      #         label = round(quantiles[3], 2),
      #         color = "red", fontface = "bold") +
      #annotate("text",  y = quantiles[4] + offset, x = -0.2,
      #         label = round(quantiles[4], 2),
      #         color = "red", fontface = "bold")
  }
  if (is.null(x.ticks)) {
    plt <- plt +
      theme(axis.ticks.x = element_blank(),
            axis.text.x = element_blank())
  }
  plt
}

gg.density <- function(data, density.column, fill.column = NULL, xlab = "", ylab = "", title = "", legend.title = NULL, legend.names = NULL) {
  #'
  #'
  #'
  #'
  #'
  #'
  #'
  #'
  assert(density.column %in% colnames(data))
  assert(fill.column %in% colnames(data))
  
  if (!is.null(legend.title)) {
    assert(!is.null(fill.column), "fill.column must be provided if you want to set legend title.")
  }
  if (!is.null(legend.names)) {
    fill.levels <- levels(unlist(data[, ..fill.column]))
    assert(length(fill.levels) == length(legend.names), "#levels of fill.column must match length of legend.names")
    assertNamed(legend.names, type = "named")
    assert(all(names(legend.names) %in% fill.levels), "legend.names must be a matching named vector")
    legend.names <- unname(legend.names[order(names(legend.names), fill.levels)])
  }
  plt<- ggplot() +
    geom_density(aes(x = unlist(data[, ..density.column]), fill = unlist(data[, ..fill.column])),
                 alpha = 0.8) +
    xlab(xlab) +
    ylab(ylab) +
    ggtitle(title) +
    default_theme
  if (!is.null(legend.title)) {
    plt <- plt + labs(fill = "Geschlecht")
  }
  if (!is.null(legend.names)) {
    #plt <- plt + scale_fill_discrete(labels = legend.names,
    #                                 values = scale_fill_manual_values)
    plt <- plt + scale_fill_manual(labels = legend.names, values = scale_fill_manual_values)
  } else {
    plt <- plt + two_scale_fill
  }
  plt
}

gg.histogram <- function(data, column, interval = FALSE, interval.devisor = 1, fill = NULL) {
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

cold.time.add <- function(time.h, time.m) {
  #'
  #' @param time.h
  #' @param time.m
  #'
  #'
  time.m[is.na(time.m)] <- 0
  time.h[is.na(time.h)] <- 0
  
  time.h * 60 + time.m
}

mismatch.out <- function(data) {
  stderr <- function(x) sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))
  sum_all <- data[, .(as.numeric(as.character(`mm-A`)), as.numeric(as.character(`mm-DR`)), as.numeric(as.character(`mm-B`)))]
  sum_all <- apply(X = sum_all, MARGIN = 1, FUN = sum, na.rm = TRUE)
  sum_all[sum_all == 0] <- NA
  c(mean = mean(sum_all, na.rm = TRUE), std.err = stderr(sum_all))
}
stderr <- function(x) sd(x, na.rm = TRUE) / sqrt(length(!is.na(x)))


create_iga_regime1 <- function(data_iga) {
  data_iga <- data_iga %>% 
    mutate(status_date = case_when(
      ## graft-loss within follow up period
      !is.na(`graft loss date`) & `graft loss date` <  `T-date` + follow_up ~ time_graft_loss,
      ## graft-loss after follow up period
      !is.na(`graft loss date`) & `graft loss date` >  `T-date` + follow_up ~ time_date_follow_up,
      ## no graft-loss and last seen within follow up
      is.na(`graft loss date`) & !is.na(`T-dls`) & `T-dls` < `T-date` + follow_up ~ time_t_dls,
      ## no graft-loss and last seen after follow up
      is.na(`graft loss date`) & !is.na(`T-dls`) & `T-dls` > `T-date` + follow_up ~ time_date_follow_up,
      ## no graft loss and no last seen 
      is.na(`graft loss date`) & is.na(`T-dls`) ~ time_date_follow_up
    )
    )
  data_iga <- data_iga %>% 
    mutate(status = case_when(
      ## graft-loss within follow up period
      !is.na(`graft loss date`) & `graft loss date` <  `T-date` + follow_up ~ 1,
      ## else censored
      TRUE ~ 0,
    )
    )
  data_iga
}

create_iga_regime2 <- function(data_iga) {
  data_iga <- data_iga %>% 
    mutate(status_date = case_when(
      ## patient death and death date within follow up
      (`Pat death (0=alive, 1= dead)` == 1) & `T-dls` < `T-date` + follow_up ~ time_t_dls,
      ## patient dead but after follow up
      (`Pat death (0=alive, 1= dead)` == 1) & `T-dls` > `T-date` + follow_up ~ time_date_follow_up,
      ## patient not death but dropped within follow up
      (`Pat death (0=alive, 1= dead)` == 0) & `T-dls` < `T-date` + follow_up ~ time_t_dls,
      ## patient not death but dropped after follow up
      (`Pat death (0=alive, 1= dead)` == 0) & `T-dls` > `T-date` + follow_up ~ time_date_follow_up,
      ## NOTE: T-dls never NA
    ))
  data_iga
}

create_ntx_regime1 <- function(data_ntx) {
  data_ntx <- data_ntx %>%
    mutate(status_date = case_when(
      ## patient experienced graft loss
      !is.na(Transplantatfunktionsende) & Transplantatfunktionsende <= (Datum + follow_up) ~ interval(Datum, Transplantatfunktionsende) / years(1),
      ## patient died within follow up
      `Todesdatum[NTX PatientenInformation]` < (Datum + follow_up) ~ interval(Datum, `Todesdatum[NTX PatientenInformation]`) / years(1),
      ## patiend last seen within follow up
      `Date last seen[NTX PatientenInformation]` < (Datum + follow_up) ~ interval(Datum, `Date last seen[NTX PatientenInformation]`) / years(1),
      ## else follow up
      TRUE ~ interval(Datum, (Datum + follow_up)) / years(1)
    ))
  data_ntx <- data_ntx %>%
    mutate(status = case_when(
      ## patient experienced graft loss
      !is.na(Transplantatfunktionsende) & Transplantatfunktionsende <= (Datum + follow_up) ~ 1,
      ## patient died within follow up 
      `Todesdatum[NTX PatientenInformation]` < (Datum + follow_up) ~ 0,
      ## patiend last seen within follow up
      `Date last seen[NTX PatientenInformation]` < (Datum + follow_up) ~ 0,
      ## else follow up
      TRUE ~ 0
    ))
  data_ntx
}

create_ntx_regime2 <- function(data_ntx) {
  data_ntx <- data_ntx %>%
    mutate(status_date = case_when(
      ## patient died within follow up
      `Todesdatum[NTX PatientenInformation]` <= (Datum + follow_up) ~ interval(Datum, `Todesdatum[NTX PatientenInformation]`) / years(1),
      ## patient died after follow up
      `Todesdatum[NTX PatientenInformation]` > (Datum + follow_up) ~ interval(Datum, (Datum + follow_up)) / years(1),
      ## patient dropped within follow up
      `Date last seen[NTX PatientenInformation]` <= (Datum + follow_up) ~ interval(Datum, `Date last seen[NTX PatientenInformation]`) / years(1),
      ## patient dropped after follow up
      `Date last seen[NTX PatientenInformation]` > (Datum + follow_up) ~ interval(Datum, (Datum + follow_up)) / years(1)
    ))
  data_ntx <- data_ntx %>%
    mutate(status = case_when(
      ## patient died within follow up
      `Todesdatum[NTX PatientenInformation]` <= (Datum + follow_up) ~ 1,
      ## patient died after follow up
      `Todesdatum[NTX PatientenInformation]` > (Datum + follow_up) ~ 0,
      ## patient dropped within follow up
      `Date last seen[NTX PatientenInformation]` <= (Datum + follow_up) ~ 0,
      ## patient dropped after follow up
      `Date last seen[NTX PatientenInformation]` > (Datum + follow_up) ~ 0
    ))
  data_ntx
}
simple.cox.surv <- function(data, covariate) {
  #'
  #' @data data.table with status and status_date column
  #' @covariate string of column name in 'data' used as regressor
  #'
  #'
  assertString(covariate, na.ok = FALSE)
  assertDataTable(data)
  assert(all(c("status", "status_date") %in% colnames(data)))
  assert(covariate %in% colnames(data))
  ##
  coxph(formula = Surv(time = as.numeric(status_date),
                       event = status) ~ data[[covariate]], data = data_iga)
}
  