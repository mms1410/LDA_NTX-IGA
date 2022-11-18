################################################################################
# descriptive metrics
################################################################################
data.table(psych::describe(data_iga))[, vars := colnames(data_iga)][] %>% 
  fwrite(file = paste0(dir.assets, .Platform$file.sep, "summary_iga_all(psych).csv"))
data.table(psych::describe(data_iga_neg))[, vars := colnames(data_iga_neg)][] %>% 
  fwrite(file = paste0(dir.assets, .Platform$file.sep, "summary_iga_neg(psych).csv"))
data.table(psych::describe(data_iga_pos))[, vars := colnames(data_iga_pos)][] %>% 
  fwrite(file = paste0(dir.assets, .Platform$file.sep, "summary_iga_pos(psych).csv"))
data.table(psych::describe(data_ntx))[, vars := colnames(data_ntx)][] %>% 
  fwrite(file = paste0(dir.assets, .Platform$file.sep, "summary_ntx_all(psych).csv"))
################################################################################