###=============================================================================
#dir.project <- rstudioapi::getSourceEditorContext()$path  #.../R/main.R
#dir.project <- dirname(dir.project)  #.../R
#dir.project <- dirname(dir.project)  #...
#source(paste0(dir.project, .Platform$file.sep, "R", .Platform$file.sep,
#              "functions.R"))
#packages <- scan(file = paste0(dir.project, .Platform$file.sep, "requirements.txt"),
#                 sep = "\t", what = character())
#packages <- sapply(packages, require, character.only = TRUE)#
#
#if (!all(packages)) {
#  stop(paste0("Could not load all packages:\n",
#              paste0(names(packages), ":",packages, "\n", collapse = "")
#              ))
#}
#path.data <- paste0(dir.project, .Platform$file.sep, "data")
###=============================================================================
## read data
data.iga <- fread(paste0(dir.data, .Platform$file.sep, "allIgA.csv"),
                  na.strings = c("-", ""))
data.ntx <- fread(paste0(dir.data, .Platform$file.sep, "allNTX.csv"),
                  na.strings = c("-", ""))
###=============================================================================
## remove whitespaces etc.
tmp.colnames.iga <- tidy.name(colnames(data.iga))
colnames(data.iga) <- tmp.colnames.iga
tmp.colnames.ntx <- tidy.name(colnames(data.ntx))
colnames(data.ntx) <- tmp.colnames.ntx
data.iga[, (tmp.colnames.iga) := lapply(.SD, tidy.column), .SDcols = tmp.colnames.iga]
data.ntx[, (tmp.colnames.ntx) := lapply(.SD, tidy.column), .SDcols = tmp.colnames.ntx]
###=============================================================================
tmp.col.date.iga <- c("Transplantatfunktionsende", "Todesdatum", "date_of_biopsy",
                      "Geburtsdatum", "Datum_TX", "Date_last_seen")
tmp.col.date.ntx <- c("Datum_TX", "Geburtsdatum", "Transplantatfunktionsende",
                      "Date_last_seen", "Todesdatum")

if (!all(tmp.col.date.iga %in% colnames(data.iga))) {
  stop("Some given column names in tmp.col.date.iga seem not to exist in data:\n",
       paste0(tmp.col.date.iga,":",tmp.col.date.iga %in% colnames(data.iga), "\n", collapse = ""))
}
if (!all(tmp.col.date.ntx %in% colnames(data.ntx))) {
  stop("Some given column names in tmp.col.date.ntx seem not to exist in data:\n",
       paste0(tmp.col.date.ntx,":",tmp.col.date.ntx %in% colnames(data.ntx), "\n", collapse = ""))
}
tmp.col.parse.iga <- unlist(data.iga[, lapply(.SD, check.dateformat.string), .SDcols = tmp.col.date.iga])
tmp.col.parse.ntx <- unlist(data.ntx[, lapply(.SD, check.dateformat.string), .SDcols = tmp.col.date.ntx])
if (!all(tmp.col.parse.iga)) {
  stop("Could not parse all date columns for IgA:\n",
       paste0(names(tmp.col.parse.iga), ":", tmp.col.parse.iga, "\n", collapse = ""))
}
if (!all(tmp.col.parse.ntx)) {
  stop("Could not parse all date columns for NTX:\n",
       paste0(names(tmp.col.parse.ntx), ":", tmp.col.parse.ntx, "\n", collapse = ""))
}
tmp.col.mismatch <- c("mm_A", "mm_B", "mm_DR")
if (!(all(tmp.col.mismatch %in% colnames(data.iga)) & all(tmp.col.mismatch %in% colnames(data.ntx)))){
  stop("tmp.col.mismatch not in dataset")
}
follow_up <- 10
###=============================================================================
## parse all date-format columns in ISO format
data.iga[, (tmp.col.date.iga) := lapply(.SD, iso.format.date), .SDcols = tmp.col.date.iga]
data.ntx[, (tmp.col.date.ntx) := lapply(.SD, iso.format.date), .SDcols = tmp.col.date.ntx]
###=============================================================================
## convert character to factor
tmp.col.factor.iga <- colnames(data.iga)[which(as.vector(data.iga[,lapply(.SD, class)]) == "character")]
tmp.col.factor.ntx <- colnames(data.ntx)[which(as.vector(data.ntx[,lapply(.SD, class)]) == "character")]
data.iga[, (tmp.col.factor.iga) := lapply(.SD, as.factor), .SDcols = tmp.col.factor.iga]
data.ntx[, (tmp.col.factor.ntx) := lapply(.SD, as.factor), .SDcols = tmp.col.factor.ntx]
## convert specified columns to factor
tmp.col.factor.iga <- c("graft_loss(0=functial,1=loss)", "Pat_death(0=alive,1=dead)",
                        "biopsy_after_KTX(0=no,1=yes)", "biopsy_proven_recurrence(0=no,1=yes)")
if (!all(tmp.col.factor.iga %in% colnames(data.iga))) {
  stop()
}
data.iga[, (tmp.col.factor.iga) := lapply(.SD, as.factor), .SDcols = tmp.col.factor.iga]
###=============================================================================
### Date_last_seen in iga contains either date of death or last occasion patent was seen, but not for ntx
### data.tables `fifelse` is used to preserve Date format
data.ntx[, Date_last_seen := fifelse(!is.na(Date_last_seen), Date_last_seen, Todesdatum)]
###=============================================================================
## age last seen
data.ntx[, R_age_Tdls := interval(Geburtsdatum, Date_last_seen) / years(1)]
data.iga[, R_age_Tdls := interval(Geburtsdatum, Date_last_seen) / years(1)]
## age surgery
data.ntx[, R_age_surgery := interval(Geburtsdatum, Datum_TX) / years(1)]
data.iga[, R_age_surgery := interval(Geburtsdatum, Datum_TX) / years(1)]
## cold time sum minutes
data.ntx[, cold_time_minutes := cold.time.add(Cold_ischaemic_period_hours, Cold_ischaemic_period_minutes)]
data.iga[, cold_time_minutes := cold.time.add(Cold_ischaemic_period_hours, Cold_ischaemic_period_minutes)]
data.ntx[, cold_time_minutes := ifelse(cold_time_minutes == 0, NA, cold_time_minutes)]
data.iga[, cold_time_minutes := ifelse(cold_time_minutes == 0, NA, cold_time_minutes)]
## age follow_up
### NOTE: `Date_last_seen` if "min" of last seen timepoint and death
#data.ntx[, age_at_follow_up := fifelse((Date_last_seen > (Datum_TX + years(follow_up))), (Datum_TX + years(follow_up)), as.Date(NA))]
#data.iga[, age_at_follow_up := fifelse((Date_last_seen > (Datum_TX + years(follow_up))), (Datum_TX + years(follow_up)), as.Date(NA))]
## mismatch sum
data.ntx[, mismatch_sum := rowSums(.SD, na.rm = TRUE), .SDcols = tmp.col.mismatch]
data.ntx[, mismatch_sum := ifelse(mismatch_sum == 0, NA, mismatch_sum)]
data.iga[, mismatch_sum := rowSums(.SD, na.rm = TRUE), .SDcols = tmp.col.mismatch]
data.iga[, mismatch_sum := ifelse(mismatch_sum == 0, NA, mismatch_sum)]
## years_within_follow_up
data.ntx[, years_within_follow_up := R_age_Tdls - R_age_surgery]
data.iga[, years_within_follow_up := R_age_Tdls - R_age_surgery]
###=============================================================================
## patients younger than 18 years are not considered
data.ntx <- data.ntx[R_age_surgery >= 18]
data.iga <- data.iga[R_age_surgery >= 18] 
###=============================================================================
data.iga.pos <- data.iga[`biopsy_after_KTX(0=no,1=yes)` == 1]
data.iga.neg <- data.iga[`biopsy_after_KTX(0=no,1=yes)` == 0]
###=============================================================================
# remove tmp variables not used any more
rm( list = ls()[grep(x = ls(), pattern = "^tmp")])
Sys.sleep(1)
cat(paste0(rep("=", 78,), collapse = ""))
cat("\n")
cat("read_data.R terminated")
cat("\n")
cat(paste0(rep("=", 78,), collapse = ""))
cat("\n")
Sys.sleep(1)
