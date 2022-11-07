################################################################################
# main
################################################################################
## set path to project folder
dir.project <- rstudioapi::getSourceEditorContext()$path  #.../R/main.R
dir.project <- dirname(dir.project)  #.../R
dir.project <- dirname(dir.project)  #...

packages <- scan(file = paste0(dir.project, .Platform$file.sep, "requirements.txt"),
     sep = "\t", what = character())

install.packages(packages[!packages %in% installed.packages()])
sapply(packages, require, character.only = TRUE)
#######
default_theme <- theme_minimal()
two_scale_fill <- scale_fill_manual(values=c("#69b3a2", "#404080"))
## create folder for graphics
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
## partition iga data into positive (with recurrence after biopsy) and
## negative (no recurrence after biopsy)
data_iga_pos <- data_iga[`biopsy proven recurrence (0=no, 1=yes)` == 1]
data_iga_neg <- data_iga[`biopsy proven recurrence (0=no, 1=yes)` == 0]

data.table(psych::describe(data_iga))[, vars := colnames(data_iga)][] %>% 
  fwrite(file = paste0(dir.assets, .Platform$file.sep, "summary_iga_all.csv"))
data.table(psych::describe(data_iga_neg))[, vars := colnames(data_iga_neg)][] %>% 
  fwrite(file = paste0(dir.assets, .Platform$file.sep, "summary_iga_neg.csv"))
data.table(psych::describe(data_iga_pos))[, vars := colnames(data_iga_pos)][] %>% 
  fwrite(file = paste0(dir.assets, .Platform$file.sep, "summary_iga_pos.csv"))
data.table(psych::describe(data_ntx))[, vars := colnames(data_ntx)][] %>% 
  fwrite(file = paste0(dir.assets, .Platform$file.sep, "summary_ntx_all.csv"))

################################################################################
# descriptive statistics
################################################################################
source(paste0(dir.scripts, .Platform$file.sep, "descriptive_iga_patient.R"))
source(paste0(dir.scripts, .Platform$file.sep, "descriptive_iga_donator.R"))
################################################################################
iga_table <- rbindlist(
  list(
    iga_dropout = data.frame(
      cbind(name = "dropout",tbl_iga_pat_drop)),
    iga_graftloss = data.frame(
      cbind(name = "graftloss", tbl_iga_graft_loss)),
    iga_graftloss_follow_up = data.frame(
      cbind(name = "graftloss_followup", tbl_iga_graft_loss_follow_up)),
    iga_pat_death = data.frame(
      cbind(name = "pat_death", tbl_iga_pat_death)),
    iga_don_dead_abs = data.frame(
      cbind(name = "don_dead_abs", tbl_iga_don_dead_abs)),
    iga_don_dead_rel = data.frame(
      cbind(name = "don_dead_rel", tbl_iga_don_dead_rel)),
    iga_don_living_abs = data.frame(
      cbind(name = "don_living_abs", tbl_iga_don_living_abs)),
    iga_don_living_rel = data.frame(
      cbind(name = "don_living_rel", tbl_iga_don_living_rel)),
    iga_cis_h_mean = data.frame(
      cbind(name = "cold_h_mean", tbl_iga_cis_mean)),
    iga_cis_h_sd = data.frame(
      cbind(name = "cold_h_sd", tbl_iga_cis_sd))
  ))
