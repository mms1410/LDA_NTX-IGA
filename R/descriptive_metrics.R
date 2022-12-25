################################################################################
# descriptive metrics
################################################################################
data.table(psych::describe(data_iga))[, vars := colnames(data_iga)][] %>% 
  fwrite(file = paste0(dir.assets.csv, .Platform$file.sep, "summary_iga_all(psych).csv"))
data.table(psych::describe(data_iga_neg))[, vars := colnames(data_iga_neg)][] %>% 
  fwrite(file = paste0(dir.assets.csv, .Platform$file.sep, "summary_iga_neg(psych).csv"))
data.table(psych::describe(data_iga_pos))[, vars := colnames(data_iga_pos)][] %>% 
  fwrite(file = paste0(dir.assets.csv, .Platform$file.sep, "summary_iga_pos(psych).csv"))
data.table(psych::describe(data_ntx))[, vars := colnames(data_ntx)][] %>% 
  fwrite(file = paste0(dir.assets.csv, .Platform$file.sep, "summary_ntx_all(psych).csv"))
################################################################################
# TODO: age in years
metadata <- rbindlist(list(
  create.summary.num.iga(data_iga, "R_age_Tdate", c("R_age_Tdate_iga", "R_age_Tdate_iga_0", "R_age_Tdate_iga_1")),
  create.summary.num.ntx(data_ntx, "R_age_Datum", "R_age_Tdate_ntx"),
  create.summary.num.iga(data_iga, "R_age_Tdls", c("R_age_Tdls_iga", "R_age_Tdls_iga_0", "R_age_Tdls_iga_1")),
  create.summary.num.ntx(data_ntx, "R_age_Tdls", "R_age_Tdls_ntx"),
  create.summary.num.iga(data_iga, "cold_time_sum_min", c("cold_iga", "cold_iga_0", "cold_iga_1")),
  create.summary.num.iga(data_iga, "D-age", c("D_age_iga", "D_age_iga_0", "D_age_iga_1")),
  create.summary.num.iga(data_iga,
                         "Current PRA%",
                         c("cur_pra_iga", "cur_pra_iga_0", "cur_pra_iga_1")),
  create.summary.num.iga(data_iga,
                         "Highest PRA%",
                         c("high_pra_iga", "high_pra_iga_0", "high_pra_iga_1"))
))
fwrite(metadata, file = paste0(dir.assets.csv, .Platform$file.sep, "metadata_numeric.csv"))


