data_iga1$`Date last seen`
data_iga1$Todesdatum
date1 <- data_iga1$`Date last seen`[4]
date2 <- data_iga1$Todesdatum[1]
nrow(data_iga1[!is.na(data_iga1$`Date last seen`) | !is.na(data_iga1$Todesdatum)])
col_names <- c("Date last seen", "Todesdatum")
data_iga1[, col_names, with=FALSE]
data_iga1[, censor_date :=  min(`Date last seen`, Todesdatum, na.rm = TRUE)] # columnwise ((
data_iga2[, censor_date := mapply(FUN = min())]
date_min <- function(x) {
  min(x, na.rm = TRUE)
}

censoring <- c("Date last seen", "Todesdatum")
event <- "TX Status"
tbl_censoring <- data_iga1[ , censoring, with = FALSE]
tbl_event <- data_iga1[ , event, with = FALSE]

kaplan_meier_plot <- function(tbl_censoring, tbl_event, strata = NULL) {
  
}
