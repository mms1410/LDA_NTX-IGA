###=============================================================================
# descriptive metrics
###=============================================================================
data.table(psych::describe(data_iga))[, vars := colnames(data_iga)][] %>% 
  fwrite(file = paste0(dir.assets.csv, .Platform$file.sep, "summary_iga_all(psych).csv"))
data.table(psych::describe(data_iga_neg))[, vars := colnames(data_iga_neg)][] %>% 
  fwrite(file = paste0(dir.assets.csv, .Platform$file.sep, "summary_iga_neg(psych).csv"))
data.table(psych::describe(data_iga_pos))[, vars := colnames(data_iga_pos)][] %>% 
  fwrite(file = paste0(dir.assets.csv, .Platform$file.sep, "summary_iga_pos(psych).csv"))
data.table(psych::describe(data_ntx))[, vars := colnames(data_ntx)][] %>% 
  fwrite(file = paste0(dir.assets.csv, .Platform$file.sep, "summary_ntx_all(psych).csv"))
###=============================================================================
metadata <- rbindlist(list(
  create.summary.num(data.iga, "cold_time_minutes", c("cold_iga", "cold_iga_0", "cold_iga_1")),
  create.summary.num(data.ntx, var.name="cold_time_minutes", subset.names="cold_ntx",
                     colname.split = NULL),
  create.summary.num(data.iga, "D_age", c("D_age_iga", "D_age_iga_0", "D_age_iga_1")),
  create.summary.num(data.ntx, var.name="D_age", subset.names="D_age_ntx",
                     colname.split = NULL),
  create.summary.num(data.iga,"current_PRA%" ,
                     c("high_pra_iga_all", "high_pra_iga_0", "high_pra_iga_1")),
  create.summary.num(data.ntx, var.name="current_PRA", subset.names="curr_pra_ntx",
                     colname.split = NULL),
  create.summary.num(data.iga,"Highest_PRA%" ,
                     c("high_pra_iga_all", "high_pra_iga_0", "high_pra_iga_1")),
  create.summary.num(data.ntx, var.name="highest_PRA", subset.names="high_pra_iga",
                     colname.split = NULL),
  create.summary.num(data.iga, "mismatch_sum",
                     c("mmm_sum_iga_all", "mm_sum_iga_0", "mm_sum_iga_1")),
  create.summary.num(data.ntx, var.name="mismatch_sum", subset.name="mm_sum_ntx",
                     colname.split=NULL),
  create.summary.num(data.iga, var.name="years_within_follow_up",
                     c("years_follow_iga_all", "years_follow_iga_0", "years_follow_iga_1")),
  create.summary.num(data.ntx, var.name="years_within_follow_up", subset.names="years_follow_ntx",
                     colname.split=NULL),
  create.summary.num(data.iga[`Pat_death(0=alive,1=dead)`==1], var.name="years_within_follow_up",
                     c("years_follow_death_iga_all", "years_follow_death_iga_0", "years_follow_death_iga_1")),
  create.summary.num(data.ntx[`Patienten_Status[NTXPatientenInformation]` == "2_verstorben"],
                     var.name="years_within_follow_up", subset.names="years_follow_death_ntx",
                     colname.split=NULL),
  create.summary.num(data.iga[`Pat_death(0=alive,1=dead)`==0], var.name="years_within_follow_up",
                     c("years_follow_alive_iga_all", "years_follow_alive_iga_0", "years_follow_alive_iga_1")),
  create.summary.num(data.ntx[`Patienten_Status[NTXPatientenInformation]` == "1_lebt"],
                     var.name="years_within_follow_up", subset.names="years_follow_alive_ntx",
                     colname.split=NULL),
  create.summary.num(data.iga, var.name="Krea_1Y",
                     c("krea_1Y_iga_all", "krea_1Y_iga_0", "krea_1Y_iga_1")),
  create.summary.num(data.ntx, var.name="Krea_1Y", subset.names="krea_1Y_ntx",
                     colname.split=NULL),
  create.summary.num(data.iga, var.name="Krea_5Y",
                     c("krea_5Y_iga_all", "krea_5Y_iga_0", "krea_5Y_iga_1")),
  create.summary.num(data.ntx, var.name="Krea_5Y", subset.names="krea_5Y_ntx",
                     colname.split=NULL),
  create.summary.num(data.iga, var.name="Krea_10Y",
                     c("krea_10Y_iga_all", "krea_10Y_iga_0", "krea_10Y_iga_1")),
  create.summary.num(data.ntx, var.name="Krea_10Y", subset.names="krea_10Y_ntx",
                     colname.split=NULL)
))
fwrite(metadata, file = paste0(dir.assets.csv, .Platform$file.sep, "metadata_numeric.csv"))
###=============================================================================
## HLA
summary(aov(mismatch_sum ~ Geschlecht, data = data.iga))
summary(aov(mismatch_sum ~ Geschlecht, data = data.iga.pos))
summary(aov(mismatch_sum ~ Geschlecht, data = data.iga.neg))

rbindlist(list(
  data.iga[, .(mismatch_sum = mismatch_sum, group = as.factor("iga"))],
  data.ntx[, .(mismatch_sum = mismatch_sum, group = as.factor("ntx"))]
)) %>% aov(formula = mismatch_sum ~ group) %>% summary()
###=============================================================================
## current PRA
summary(aov(`current_PRA%` ~ Geschlecht, data = data.iga))
summary(aov(`current_PRA%` ~ Geschlecht, data = data.iga.pos))
summary(aov(`current_PRA%` ~ Geschlecht, data = data.iga.neg))
summary(aov(current_PRA ~ Geschlecht, data = data.ntx)) #!

rbindlist(list(
  data.iga[, .(c_pra = `current_PRA%`, group = as.factor("iga"))],
  data.ntx[, .(c_pra = current_PRA, group = as.factor("ntx"))]
)) %>% aov(formula = c_pra ~ group) %>% summary()
###=============================================================================
## highest PRA
summary(aov(`Highest_PRA%` ~ Geschlecht, data = data.iga))
summary(aov(`Highest_PRA%` ~ Geschlecht, data = data.iga.pos))
summary(aov(`Highest_PRA%` ~ Geschlecht, data = data.iga.neg))
summary(aov(highest_PRA ~ Geschlecht, data = data.ntx)) #!

rbindlist(list(
  data.iga[, .(h_pra = `Highest_PRA%`, group = as.factor("iga"))],
  data.ntx[, .(h_pra = highest_PRA, group = as.factor("ntx"))]
)) %>% aov(formula = h_pra ~ group) %>% summary()
###=============================================================================
## time biopsy
summary(aov(`time_of_biopsy_(years_after_KTX)`~`biopsy_proven_recurrence(0=no,1=yes)`, data.iga))
summary(aov(`time_of_biopsy_(years_after_KTX)`~`biopsy_proven_recurrence(0=no,1=yes)`, data.iga.pos))
# summary(aov(`time_of_biopsy_(years_after_KTX)`~`biopsy_proven_recurrence(0=no,1=yes)`, data.iga.neg))
###=============================================================================



