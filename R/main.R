#TODO:
#boxplot and fivenum seems not to fit
################################################################################
# main
################################################################################
## set path to project folder
dir.project <- rstudioapi::getSourceEditorContext()$path  #.../R/main.R
dir.project <- dirname(dir.project)  #.../R
dir.project <- dirname(dir.project)  #...

packages <- scan(file = paste0(dir.project, .Platform$file.sep, "requirements.txt"),
     sep = "\t", what = character())

sapply(packages, require, character.only = TRUE)
#######
default_theme <- theme_minimal()
scale_fill_manual_values <- c("#69b3a2", "#404080")
two_scale_fill <- scale_fill_manual(values= scale_fill_manual_values)
## create folder for assets
if (!file.exists(paste0(dir.project, .Platform$file.sep, "assets"))) {
  dir.create(paste0(dir.project, .Platform$file.sep, "assets"))
}
dir.assets <- paste0(dir.project, .Platform$file.sep, "assets")
##
dir.scripts <- rstudioapi::getSourceEditorContext()$path  #.../R/main.R
dir.scripts <- dirname(dir.scripts)  #.../R

################################################################################
# preliminaries
################################################################################
source(paste0(dir.scripts, .Platform$file.sep, "read_data.R"))
source(paste0(dir.scripts, .Platform$file.sep, "functions.R"))
follow_up <- years(10)
##############################################################################
## create variable containing minimum of date last seen (T-dls) and end of
## follow-up period (T-date + follow_up)
data_iga$follow_up_truncated <-pmin(data_iga$`T-dls`, data_iga$`T-date` + follow_up)
## create variable containing row wise sum of mm-A, mm-B and mm-DR
data_iga$mismatch_sum <- as.numeric(as.character(data_iga$`mm-A`)) + as.numeric(as.character(data_iga$`mm-B`)) + as.numeric(as.character(data_iga$`mm-DR`))
## add cold isch time hours and cold isch time minutes (in total minutes)
data_iga[, cold_time_sum_min := cold.time.add(`Cold ischaemic period hours`, `Cold ischaemic period minutes`)]
## partition iga data into positive (with recurrence after biopsy) and
## negative (no recurrence after biopsy)
data_iga_pos <- data_iga[`biopsy proven recurrence (0=no, 1=yes)` == 1]
data_iga_neg <- data_iga[`biopsy proven recurrence (0=no, 1=yes)` == 0]
####
source(paste0(dir.scripts, .Platform$file.sep, "descriptive_plots.R"))
source(paste0(dir.scripts, .Platform$file.sep, "descriptive_metrics.R"))
