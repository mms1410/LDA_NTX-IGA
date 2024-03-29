################################################################################
# this script contains functions used in other scripts in this project
################################################################################
stderr <- function(x){
  if (length(x) > 1) {
    sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))
  } else {
   0 
  }
}

save.plot <- function(plot.name) {
  # TODO check .jpg
  ggsave(paste0(dir.assets.png, .Platform$file.sep, plot.name), bg = "white", dpi = 400, scale = 2.5)
}

write.summary <- function(data, file.name) {
  #'
  #'create data.table pf psych output and write csv into assets folder 
  #'
  #'@param data: data.table with data
  #'@param filename: string containing file name inclusive of ending format (csv)
  #'
  #'
  data.numeric <- data[, .SD, .SDcols = is.numeric]
  data.factor <- data[, .SD, .SDcols = is.factor]
  rstatix::get_summary_stats(data.numeric)
  
  data.table(psych::describe(data, na.rm = TRUE))[, vars := colnames(data)][]
  
  data.table(psych::describe(data, na.rm = TRUE))[, vars := colnames(data)][] %>% 
    fwrite(file = paste0(dir.assets, .Platform$file.sep, "summary_numeric_iga_all.csv"))
}

gg.boxplot <- function(data, y.column, x.column = NULL, ylab = "", xlab = "", title = "", x.ticks = NULL, offset = NULL, annotate.quantile = FALSE, ylims = NULL, width = 0.5) {
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
                               x = unlist(data[, ..x.column])), width=width) +
      default_theme
    if (!is.null(x.ticks)) {
      plt <- plt + scale_x_discrete(labels = x.ticks)
    }
  } else {
    plt <- ggplot(data) +
      geom_boxplot(aes(y = unlist(data[, ..y.column])), width=width) +
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
    assertCharacter(column, any.missing = FALSE, len = 2)
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
    assertCharacter(column, any.missing = FALSE, len = 1)
    assert(all(column %in% colnames(data)))
  }
  if (!is.null(fill)) {
    assertCharacter(fill, any.missing = FALSE, len = 1)
    assert(fill %in% colnames(data))
  }
  ggplot(data) +
    geom_histogram(mapping = aes(x = unlist(data[, ..start])))
}

cold.time.add <- function(time.h, time.m, h = FALSE) {
  #'
  #' @param time.h
  #' @param time.m
  #'
  #'
  time.m[is.na(time.m)] <- 0
  time.h[is.na(time.h)] <- 0
  
  time <- time.h * 60 + time.m
  if (h) {
    time / 60
  } else {
    time
  }
}

create.summary.num.vec <- function(data, var.name, subset.name) {
  #'
  #' create named vector of essential summary statistics
  #'
  #' @param data_iga: data.table with data
  #' @var.name: string of variable name in column of data
  #' @subset.name: string of names given to iga subsets
  #'
  #'
  data <- unlist(data[, ..var.name])
  c("name" = subset.name, "mean" = mean(data, na.rm = TRUE), "mean.stdr" = stderr(data),
    "median" = median(data, na.rm = TRUE),
    "q1" = summary(data)[[2]],
    "q2" = summary(data)[[5]],
    "IQR" = IQR(data, na.rm = TRUE),
    "n_total" = length(data),
    "n" = sum(!is.na(data)),
    "n_na" = sum(is.na(data))
  )
}


create.summary.num.ntx <- function(data_ntx, var.name, name) {
  #'
  #'create data.table of essential summary statistics
  #'
  #' @param data_ntx: data.table with data
  #' @param var.name: string with columnname
  #' @param name: name to be given in name column
  #'
  #'
  assertDataTable(data_ntx)
  assert(var.name %in% colnames(data_ntx))
  assert("numeric" %in% class(unlist((data_ntx[, ..var.name]))))
  data <- unlist(data_ntx[, ..var.name])
  create.summary.num(data_ntx, "follow_up_age", "all")
  data.table(name = name, mean = mean(data, na.rm = TRUE), mean.stdr = stderr(data),
             median = median(data, na.rm = TRUE), q1 = summary(data)[[2]],
             q2 = summary(data)[[5]], IQR = IQR(data, na.rm = TRUE),
             n_total = length(data), n = sum(!is.na(data)), n_na = sum(is.na(data))
            )
}

create.summary.num <- function(data,
                               var.name,
                               subset.names,
                               include.all = TRUE,
                               colname.split = "biopsy_proven_recurrence(0=no,1=yes)") {
  #'
  #'
  #' @data
  #' @var.name
  #' @subset.name
  #' @inlude.all
  #' @colname.split
  #'
  if (!is.null(colname.split)){
    assert(colname.split %in% colnames(data))
    assertFactor(data[[colname.split]])
  }
  if (is.null(colname.split)) {
    assert(include.all == TRUE)
    assert(length(subset.names) == 1)
  }
  assert(var.name %in% colnames(data))
  assertLogical(include.all)
  assertDataTable(data)
  
  if (!is.null(colname.split)){
    levels.data <- levels(data[[colname.split]])
    n.levels <- length(levels.data)
    if (include.all) {
      assert(length(subset.names) == (n.levels + 1))
      offset.for.all <- 1
      tbl <- create.summary.num.vec(data, var.name, subset.names[[offset.for.all]])
    } else {
      assert(length(subset.names) == n.levels)
      offset.for.all <- 0
      tbl <- NULL
    }
    for (level.idx in seq(n.levels)) {
      level.subset <- levels.data[level.idx]
      idx.subset <- data[[colname.split]] == level.subset
      name.subset <- subset.names[[level.idx + offset.for.all]]
      data.subset <- data[idx.subset,]
      tbl <- rbind(
        tbl,
        create.summary.num.vec(data.subset, var.name, name.subset)
      )
    }
    tbl <- as.data.table(tbl)
  } else {
      tbl <- create.summary.num.vec(data, var.name, subset.names)
      tbl <- as.data.table(as.list(tbl))
  }
  tbl
}


mismatch.out <- function(data) {
  # stderr <- function(x) sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))
  sum_all <- data[, .(as.numeric(as.character(`mm-A`)), as.numeric(as.character(`mm-DR`)), as.numeric(as.character(`mm-B`)))]
  sum_all <- apply(X = sum_all, MARGIN = 1, FUN = sum, na.rm = TRUE)
  sum_all[sum_all == 0] <- NA
  c(mean = mean(sum_all, na.rm = TRUE), std.err = stderr(sum_all))
}

stderr <- function(x) sd(x, na.rm = TRUE) / sqrt(length(!is.na(x)))


create.iga.regime1 <- function(data,
                                col.date.graftloss = "Transplantatfunktionsende",
                                col.date.tx = "Datum_TX",
                                col.date.death = "Todesdatum") {
  #'
  #'
  #'
  #'
  #'
  #'
  
  graftloss.within.follow <- !is.na(data[[col.date.graftloss]]) & (data[[col.date.graftloss]] <= (data[[col.date.tx]] + follow_up))
  graftloss.after.follow <- !is.na(data[[col.date.graftloss]]) & (data[[col.date.graftloss]] > (data[[col.date.tx]] + follow_up))
  dead.within.follow <- !is.na(data[[col.date.death]]) & (data[[col.date.death]] <= (data[[col.date.tx]] + follow_up))

  cat(paste0(rep("=", 78), collapse = ""))
  cat("\n")
  cat("Call create.iga.regime.1")
  cat("\n")
  cat(paste0("N graftloss.within.follow: ", sum(graftloss.within.follow), collapse = ""))
  cat("\n")
  cat(paste0("N graftloss.after.follow: ", sum(graftloss.after.follow), collapse = ""))
  cat("\n")
  cat(paste0("N dead.within.follow: ", sum(dead.within.follow), collapse = ""))
  cat("\n")
  
  data <- data %>% 
    mutate(status_date = case_when(
      ## graft-loss within follow up period
      graftloss.within.follow ~ interval(data[[col.date.tx]],data[[col.date.graftloss]]) / years(1),
      ## graft-loss after follow up period
      graftloss.after.follow ~ interval(data[[col.date.tx]], (data[[col.date.tx]] + follow_up)) / years(1),
      ## no graft-loss within follow up period and died within follow up
      (!graftloss.within.follow & dead.within.follow) ~ interval(data[[col.date.tx]], data[[col.date.death]]) / years(1),
      ## no graft-loss within follow up period and survived
      (!graftloss.within.follow & !dead.within.follow) ~ interval(data[[col.date.tx]], (data[[col.date.tx]] + follow_up)) / years(1)
    ))
  data <- data %>% 
    mutate(status = case_when(
      ## graft-loss within follow up period
      graftloss.within.follow ~ 1,
      ## graft-loss after follow up period
      graftloss.after.follow ~ 0,
      ## no graft-loss within follow up period and died within follow up
      (!graftloss.within.follow & dead.within.follow) ~ 0,
      ## no graft-loss within follow up period and survived
      (!graftloss.within.follow & !dead.within.follow) ~ 0
    ))
  cat("created iga.regime.1")
  cat("\n")
  cat(paste0(rep("=", 78), collapse = ""))
  cat("\n")
  data
}

create.iga.regime2 <- function(data,
                               col.date.death = "Todesdatum",
                               col.date.tx = "Datum_TX",
                               col.date.dls ="Date_last_seen") {
  #'
  #'
  #'
  #'
  #'
  death.within.follow <- !is.na(data[[col.date.death]]) & data[[col.date.death]] <= (data[[col.date.tx]] + follow_up)
  death.after.follow <- !is.na(data[[col.date.death]]) & data[[col.date.death]] > (data[[col.date.tx]] + follow_up)
  no.death.droppen.within.follow <- is.na(data[[col.date.death]]) & (data[[col.date.dls]] <= (data[[col.date.tx]] + follow_up))
  no.death.dropped.after.follow <- is.na(data[[col.date.death]]) & (data[[col.date.dls]] > (data[[col.date.tx]] + follow_up))
  
  
  cat(paste0(rep("=", 78), collapse = ""))
  cat("\n")
  cat("Call create.iga.regime.2")
  cat("\n")
  cat(paste0("N death.within.follow: ", sum(death.within.follow), collapse = ""))
  cat("\n")
  cat(paste0("N death.after.follow: ", sum(death.after.follow), collapse = ""))
  cat("\n")
  cat(paste0("N no.death.droppen.within.follow: ", sum(no.death.droppen.within.follow), collapse = ""))
  cat("\n")
  cat(paste0("N no.death.dropped.after.follow: ", sum(no.death.dropped.after.follow), collapse = ""))
  cat("\n")
  
  assert(sum(no.death.dropped.after.follow | no.death.droppen.within.follow) +
           sum(death.within.follow |death.after.follow) == nrow(data))
  
  data <- data %>% 
    mutate(status_date = case_when(
      ## death within follow up
      death.within.follow ~ interval(data[[col.date.tx]], data[[col.date.death]]) / years(1),
      ## death after follow up
      death.after.follow ~ interval(data[[col.date.tx]], (data[[col.date.tx]] + follow_up)) / years(1),
      ## no death and dropped within follow up
      no.death.droppen.within.follow ~ interval(data[[col.date.tx]], data[[col.date.dls]]) / years(1),
      ## no death and dropped after follow up
      no.death.dropped.after.follow ~ interval(data[[col.date.tx]], (data[[col.date.tx]] + follow_up)) / years(1)
    ))
  data <- data %>% 
    mutate(status = case_when(
      ## death within follow up
      death.within.follow ~ 1,
      ## death after follow up
      death.after.follow ~ 0,
      ## no death and dropped within follow up
      no.death.droppen.within.follow ~ 0,
      ## no death and dropped after follow up
      no.death.dropped.after.follow ~ 0
    ))
  cat("created iga.regime.2")  
  cat("\n")
  cat(paste0(rep("=", 78), collapse = ""))
  cat("\n")
  data
}

create.ntx.regime1 <- function(data,
                               col.date.graftloss = "Transplantatfunktionsende",
                               col.date.tx = "Datum_TX",
                               col.date.death = "Todesdatum",
                               col.date.dls = "Date_last_seen") {
  #'
  #'
  #'
  #'
  #'
  #'
  graft.loss <- !is.na(data[[col.date.graftloss]]) & (data[[col.date.graftloss]] < (data[[col.date.tx]] + follow_up))
  death.within.follow <- !is.na(data[[col.date.death]]) & (data[[col.date.death]] <= (data[[col.date.tx]] + follow_up))
  lastseen.within.follow <- is.na(data[[col.date.death]]) & (data[[col.date.dls]] <= (data[[col.date.tx]] + follow_up))
  
  cat(paste0(rep("=", 78), collapse = ""))
  cat("\n")
  cat("Call create.ntx.regime.1")
  cat("\n")
  cat(paste0("N graft.loss: ", sum(graft.loss, na.rm = TRUE), collapse = ""))
  cat("\n")
  cat(paste0("N death.within.follow: ", sum(death.within.follow, na.rm = TRUE), collapse = ""))
  cat("\n")
  cat(paste0("N lastseen.within.follow: ", sum(lastseen.within.follow, na.rm = TRUE), collapse = ""))
  cat("\n")
  
  
  data <- data %>%
    mutate(status_date = case_when(
      ## patient experienced graft loss
      graft.loss ~ as.numeric(interval(data[[col.date.tx]], data[[col.date.graftloss]]) / years(1)),
      # patient died within follow up
      death.within.follow ~ as.numeric(interval(data[[col.date.tx]], data[[col.date.death]]) / years(1)),
      ## patient last seen within follow up
      lastseen.within.follow ~ as.numeric(interval(data[[col.date.tx]], data[[col.date.dls]]) / years(1)),
      ## else follow up
      TRUE ~ as.numeric(interval(data[[col.date.tx]], (data[[col.date.tx]] + follow_up)) / years(1))
    ))
  data <- data %>%
    mutate(status = case_when(
      ## patient experienced graft loss
      graft.loss ~ 1,
      # patient died within follow up
      death.within.follow ~ 0,
      ## patient last seen within follow up
      lastseen.within.follow ~ 0,
      ## else follow up
      TRUE ~ 0
    ))
  cat("created ntx.regime.1")
  cat("\n")
  cat(paste0(rep("=", 78), collapse = ""))
  cat("\n")
  data
}

create.ntx.regime2 <- function(data,
                               col.date.death = "Todesdatum",
                               col.date.tx = "Datum_TX",
                               col.date.dls = "Date_last_seen") {
  #'
  #'
  #'
  #'
  
  dead.within.follow <- !is.na(data[[col.date.death]]) & (data[[col.date.death]] <= (data[[col.date.tx]] + follow_up))
  dead.after.follow <- !is.na(data[[col.date.death]]) & (data[[col.date.death]] > (data[[col.date.tx]] + follow_up))
  lastseen.within.follow <- is.na(data[[col.date.death]]) & (data[[col.date.dls]] <= (data[[col.date.tx]] + follow_up))
  lastseen.after.follow <- is.na(data[[col.date.death]]) & (data[[col.date.dls]] > (data[[col.date.tx]] + follow_up))
  
  cat(paste0(rep("=", 78), collapse = ""))
  cat("\n")
  cat("Call create.ntx.regime.2")
  cat("\n")
  cat(paste0("N dead.within.follow: ", sum(dead.within.follow), collapse = ""))
  cat("\n")
  cat(paste0("N dead.after.follow: ", sum(dead.after.follow), collapse = ""))
  cat("\n")
  cat(paste0("N lastseen.within.follow: ", sum(lastseen.within.follow), collapse = ""))
  cat("\n")
  cat(paste0("N lastseen.after.follow: ", sum(lastseen.after.follow), collapse = ""))
  
  data <- data %>%
    mutate(status_date = case_when(
      ## patient died within follow up
      dead.within.follow ~ (interval(data[[col.date.tx]], data[[col.date.death]]) / years(1)),
      ## patient died after follow up
      dead.after.follow ~ (interval(data[[col.date.tx]], (data[[col.date.tx]] + follow_up)) / years(1)),
      ## patient dropped within follow up
      lastseen.within.follow ~ (interval(data[[col.date.tx]], data[[col.date.dls]]) / years(1)),
      ## patient dropped after follow up
      lastseen.after.follow ~ interval(data[[col.date.tx]], (data[[col.date.tx]] + follow_up)) / years(1)
    ))
  data <- data %>%
    mutate(status = case_when(
      ## patient died within follow up
      dead.within.follow ~ 1,
      ## patient died after follow up
      dead.after.follow ~ 0,
      ## patient dropped within follow up
      lastseen.within.follow ~ 0,
      ## patient dropped after follow up
      lastseen.after.follow ~ 0
    ))
  cat("created ntx.regime.2")
  cat("\n")
  cat(paste0(rep("=", 78), collapse = ""))
  cat("\n")
  data
}

simple.cox.surv <- function(data, covariate, write.summary = TRUE, name.prefix = NULL, name.suffix = NULL, exponentiate = TRUE) {
  #'
  #' @data data.table with status and status_date column
  #' @covariate string of column name in 'data' used as regressor
  #'
  #'
  assertString(covariate, na.ok = FALSE)
  assertDataTable(data)
  assert(all(c("status", "status_date") %in% colnames(data)))
  assert(covariate %in% colnames(data))
  if( write.summary) {
    assert("dir.assets.csv" %in% ls(envir=.GlobalEnv))
    
  }
  ##
  model <- coxph(formula = Surv(time = as.numeric(status_date),
                       event = status) ~ data[[covariate]], data = data)
  file.name <- paste0(dir.assets.csv, .Platform$file.sep, paste0("cox_",name.prefix, covariate, name.suffix, ".csv"))
  fwrite(broom::tidy(model, conf.int = TRUE, exponentiate = exponentiate), file = file.name)
  model
}
merge.cox.files <- function(){
  #'
  #'
  #'
  #'
  #'
  assert("dir.assets.csv" %in% ls(envir=.GlobalEnv))
  cox.files <- list.files(dir.assets.csv)
  cox.files.iga.noclass <- cox.files[grep(pattern = "^cox_iga_\\d_(?!class)", cox.files, perl = TRUE)]
}

gg.binhist <- function(data, bin.breaks, colname, group.name = NULL,
                       levels.name = NULL, legend.title = NULL, include.all = NULL,
                       xlab = "", ylab = "", title = "", lowest = TRUE,
                       count.stat = TRUE, include.na = TRUE) {
  #'
  #'
  #' create a binned histogram according to given bin.breaks
  #'
  #' @data
  #' @bins
  #' @colname
  #' @group.name
  #' @xlab
  #' @ylab
  #' @title
  #' @lowest
  #' @count
  #'
  #' TODO: include.all
  assert(sum(is.na(cut(unlist(data[, ..colname]), bin.breaks, include.lowest = lowest))) == sum(is.na(data[, ..colname])),
         "binning introduces NA values")
  
  if(is.null(group.name)) {
    assert(is.null(levels.name))
    assert(is.null(include.all))
  }
  
  if(!is.null(levels.name)) assertFactor(unlist(data[, ..group.name]))
  if(!is.null(levels.name) & include.all){
    assert((length(levels.name) - 1) == length(levels(unlist(data[, ..group.name]))), "#levels name must equal group levels")
  } 
  if(!is.null(levels.name) & !include.all){
    assert((length(levels.name)) == length(levels(unlist(data[, ..group.name]))), "#levels name must equal group levels")
  } 
  if(!is.null(levels.name)) assert(all(names(levels.name) == c(levels(unlist(data[, ..group.name])), "all")))
  # assert level name match
  # data[`biopsy proven recurrence (0=no, 1=yes)` == 1, ..colname]
  
  
  #if (!is.null(group.name)) {
  #  n.levels <- length(levels(unlist(data[, ..group.name])))
  #}
  group.values <- cut(unlist(data[, ..colname]), bin.breaks, include.lowest = lowest)
  #tbl[, group.values := group.values]
  data$group.values <- group.values
  if (include.all){
    tbl <- data[, group := "all"]
  } else {
    tbl <- data.table()
  }
  for (lev in levels(unlist(data[, ..group.name]))) {
    idx <- as.vector(data[, ..group.name] == lev)
    tbl.tmp <- data[idx, ]
    tbl.tmp[, group := lev]
    tbl <- rbindlist(list(tbl.tmp, tbl))
  }
  #tbl$group.values
  set.geom.group <- function() {
    if(include.na) {
      g <- ggplot(tbl, aes(unlist(tbl[, group.values]))) 
    } else {
      g <- ggplot(tbl[!is.na(group.values)],
                  aes(unlist(tbl[!is.na(group.values), group.values])))
    }
    g +
      geom_histogram(aes(fill = group),stat = "count",
                     position = "dodge")
  }
  set.geom <- function() {
    ## return
    ggplot(data, aes(group.values)) + geom_histogram(stat="count")
  }
  
  if(is.null(group.name)) {
    gplot <-set.geom()
  } else {
    gplot <- set.geom.group() + scale_fill_discrete(labels = levels.name)
  }
  
  qplot <- gplot +
    ylab(ylab) +
    xlab(xlab) +
    ggtitle(title)
  if(!is.null(legend.title)) {
    qplot <- qplot + scale_fill_discrete(name = legend.title)
  }
  
  qplot +
    default_theme 
  
  if(!is.null(levels.name)) {
    if (!is.null(legend.title)) {  # stupid ggplot overwrites ALL scale_fill attributes
      qplot <- qplot + scale_fill_discrete(labels = levels.name, name = legend.title) 
    } else {
      qplot <- qplot + scale_fill_discrete(labels = levels.name) 
    }
    
  }
  qplot + default_theme
}
tidy.name <- function(name) {
  #'
  #'
  #'
  #'
  #'
  #'
  assertCharacter(name)
  tidy.string <- function(string) {
    string.new <- gsub(pattern = "\\s*", replacement = "", x = string)
    string.new <- gsub(pattern = "-", replacement = "_", x = string.new)
    string.new <- gsub(pattern = "/", replacement = "-", x = string.new)
    string.new <- gsub(pattern = "\\.", replacement = "_", x = string.new)
    string.new <- gsub(pattern = "männlich", replacement = "M", x = string.new)
    string.new <- gsub(pattern = "weiblich", replacement = "W", x = string.new)
    string.new
  }
  unlist(lapply(X = name, FUN = tidy.string))
}
tidy.column <- function(column) {
  #'
  #'
  #'
  #'
  if (is.character(column)) {
    tidy.name(column)
  } else {
    column
  }
}

check.format <- function(x){
  #'
  #'
  #'
  #'
  out <- tryCatch({
    as.Date(x, tryFormats  = c("%m-%d-%Y", "%d-%m-%Y"))
  }, error = function(e)FALSE)
  !any(isFALSE(out))
}

check.dateformat.string <- function(string, summarize = TRUE) {
  #'
  #'
  #'
  #'
  #'
  assertCharacter(string)
  if (summarize) {
    all(unlist(lapply(X = string, FUN = check.format)))
  } else {
    unlist(lapply(X = string, FUN = check.format))
  }
}
iso.format.date <- function(string) {
  #'
  #'
  #'
  #'
  #'
  assert(check.dateformat.string(string))
  as.Date(string, tryFormats  = c("%m-%d-%Y", "%d-%m-%Y"))  #ISO 8601
}
set.variables <- function(data,
                          colname.surgery = "Datum_TX",
                          colname.dls = "Date_last_seen",
                          colname.death = "Todesdatum",
                          colname.birth = "Geburtsdatum") {
  #'
  #'
  #'
  #data <- data.iga
  colname.surgery = "Datum_TX"
  colname.dls = "Date_last_seen"
  colname.death = "Todesdatum"
  colname.birth = "Geburtsdatum"
  follow_up_year <- lapply(X = data[, ..colname.surgery], FUN = function(x) x+years(follow_up))
  data$follow_up_year <- follow_up_year
  follow_up_age <- interval(data[[eval(colname.birth)]], data[, follow_up_year]) / years(1)
  data$follow_up_age <- follow_up_age
  tdls_age <- interval(data[[eval(colname.birth)]], data[[eval(colname.dls)]]) / years(1)
  data$tdls_age <- tdls_age
  data$follow_up_min_age <- ifelse(data$follow_up_age < data$tdls_age, data$follow_up_age, data$tdls_age)
  data
}


set.follow.up <- function(data,
                          colname.surgery = "Datum_TX",
                          colname.dls = "Date_last_seen",
                          colname.death = "Todesdatum",
                          colname.birth = "Geburtsdatum") {
  #'
  #'
  #'
  assert(colname.surgery %in% colnames(data))
  assert(colname.dls %in% colnames(data))
  assert(colname.death %in% colnames(data))
  assert(colname.birth %in% colnames(data))
  assertIntegerish(follow_up, lower = 0)
  
  #colname.surgery = "Datum_TX"
  #colname.dls = "Date_last_seen"
  #colname.death = "Todesdatum"
  #colname.birth = "Geburtsdatum"
  condition <- interval(data[[eval(colname.surgery)]],
                        data[[eval(colname.dls)]]) / years(1) > follow_up
  n.critical <- sum(interval(data[[eval(colname.surgery)]],
                             data[[eval(colname.dls)]]) / years(1) > follow_up)
  msg <- paste0(n.critical, " oberservations had dls > ", follow_up, " years")
  if (n.critical >0) warning(msg)
  # data[[eval(colname.surgery)]]
  # ifelse(condition,)
}
create.summary.fac <- function(data, colnames.fac, percentage = FALSE, group.name = NULL) {
  #'
  #'
  #'
  assert(all(colnames.fac %in% colnames(data)))
  assert(all("factor" %in% unlist(lapply(FUN = class,X =  data[, ..colnames.fac])))) 
  tbl <- table(data[, ..colnames.fac])
  if (percentage) {
    tbl.percentage <- as.data.table(prop.table(tbl))
    tbl <- as.data.table(tbl)
    n.precentage <- tbl.percentage$N
    tbl <- cbind(tbl, N.perc = n.precentage)
  } else {
    tbl <- as.data.table(tbl)
  }
  if(!is.null(group.name)) {
    tbl <- cbind(group = group.name, tbl)
  }
  tbl
}
