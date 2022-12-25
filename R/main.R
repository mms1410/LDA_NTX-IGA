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
## see http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
scale_fill_manual_values <- c("#69b3a2", "#404080")
two_scale_fill <- scale_fill_manual(values= scale_fill_manual_values)
three_scale_fill <- scale_fill_manual(values=c("#69b3a2", "#404080", "#CC6600"))
## create folder for assets
if (!file.exists(paste0(dir.project, .Platform$file.sep, "assets"))) {
  dir.create(paste0(dir.project, .Platform$file.sep, "assets"))
}
dir.assets <- paste0(dir.project, .Platform$file.sep, "assets")
if (!file.exists(paste0(dir.assets, .Platform$file.sep, "csv"))) {
  dir.create(paste0(dir.assets, .Platform$file.sep, "csv"))
}
dir.assets.csv <- paste0(dir.assets, .Platform$file.sep, "csv")
if (!file.exists(paste0(dir.assets, .Platform$file.sep, "png"))) {
  dir.create(paste0(dir.assets, .Platform$file.sep, "png"))
}
dir.assets.png <- paste0(dir.assets, .Platform$file.sep, "png")
##
dir.scripts <- rstudioapi::getSourceEditorContext()$path  #.../R/main.R
dir.scripts <- dirname(dir.scripts)  #.../R

################################################################################
# preliminaries
################################################################################
source(paste0(dir.scripts, .Platform$file.sep, "read_data.R"))
source(paste0(dir.scripts, .Platform$file.sep, "functions.R"))
follow_up <- years(10)
## follow-up period (T-date + follow_up)
data_iga$follow_up_truncated <-pmin(data_iga$`T-dls`, data_iga$`T-date` + follow_up)
##############################################################################
# partition iga data into positive (with recurrence after biopsy) and
# negative (no recurrence after biopsy)
data_iga_pos <- data_iga[`biopsy proven recurrence (0=no, 1=yes)` == 1]
data_iga_neg <- data_iga[`biopsy proven recurrence (0=no, 1=yes)` == 0]
####
source(paste0(dir.scripts, .Platform$file.sep, "descriptive_plots.R"))
source(paste0(dir.scripts, .Platform$file.sep, "descriptive_metrics.R"))
source(paste0(dir.scripts, .Platform$file.sep, "survival.R"))
