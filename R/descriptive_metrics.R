###=============================================================================
# descriptive metrics
###=============================================================================
data.table(psych::describe(data.iga))[, vars := colnames(data.iga)][] %>% 
  fwrite(file = paste0(dir.assets.csv, .Platform$file.sep, "summary_iga_all(psych).csv"))
data.table(psych::describe(data.iga.neg))[, vars := colnames(data.iga.neg)][] %>% 
  fwrite(file = paste0(dir.assets.csv, .Platform$file.sep, "summary_iga_neg(psych).csv"))
data.table(psych::describe(data.iga.pos))[, vars := colnames(data.iga.pos)][] %>% 
  fwrite(file = paste0(dir.assets.csv, .Platform$file.sep, "summary_iga_pos(psych).csv"))
data.table(psych::describe(data.ntx))[, vars := colnames(data.ntx)][] %>% 
  fwrite(file = paste0(dir.assets.csv, .Platform$file.sep, "summary_ntx_all(psych).csv"))
###=============================================================================
metadata <- rbindlist(list(
  # cold_time
  create.summary.num(data.iga, "cold_time_minutes", c("cold_iga", "cold_iga_0", "cold_iga_1")),
  create.summary.num(data.ntx, var.name="cold_time_minutes", subset.names="cold_ntx",
                     colname.split = NULL),
  # D_age
  create.summary.num(data.iga, "D_age", c("D_age_all-IgA", "D_age_r+IgA", "D_age_r-IgA")),
  create.summary.num(data.ntx, var.name="D_age", subset.names="D_age_ntx",
                     colname.split = NULL),
  # cPRA
  create.summary.num(data.iga,"current_PRA%" ,
                     c("high_pra_all-IgA", "high_pra_r+IgA", "high_pra_r-IgA")),
  create.summary.num(data.ntx, var.name="current_PRA", subset.names="curr_pra_ntx",
                     colname.split = NULL),
  # hPRA
  create.summary.num(data.iga,"Highest_PRA%" ,
                     c("high_pra_all-IgA", "high_pra_r+IgA", "high_pra_r-IgA")),
  create.summary.num(data.ntx, var.name="highest_PRA", subset.names="high_pra_iga",
                     colname.split = NULL),
  # HLAmm
  create.summary.num(data.iga, "mismatch_sum",
                     c("mmm_sum_all-IgA", "mm_sum_r+IgA", "mm_sum_r-IgA")),
  create.summary.num(data.ntx, var.name="mismatch_sum", subset.name="mm_sum_ntx",
                     colname.split=NULL),
  # years follow
  create.summary.num(data.iga, var.name="years_within_follow_up",
                     c("years_follow_all-IgA", "years_follow_r+IgA", "years_follow_r-IgA")),
  create.summary.num(data.ntx, var.name="years_within_follow_up", subset.names="years_follow_ntx",
                     colname.split=NULL),
  # years follow (death)
  create.summary.num(data.iga[`Pat_death(0=alive,1=dead)`==1], var.name="years_within_follow_up",
                     c("years_follow_death_all-IgA", "years_follow_death_r+IgA", "years_follow_death_r-IgA")),
  create.summary.num(data.ntx[`Patienten_Status[NTXPatientenInformation]` == "2_verstorben"],
                     var.name="years_within_follow_up", subset.names="years_follow_death_ntx",
                     colname.split=NULL),
  # year follow (alive)
  create.summary.num(data.iga[`Pat_death(0=alive,1=dead)`==0], var.name="years_within_follow_up",
                     c("years_follow_alive_all-IgA", "years_follow_alive_r+IgA", "years_follow_alive_r-IgA")),
  create.summary.num(data.ntx[`Patienten_Status[NTXPatientenInformation]` == "1_lebt"],
                     var.name="years_within_follow_up", subset.names="years_follow_alive_ntx",
                     colname.split=NULL),
  # Krea1Y
  create.summary.num(data.iga, var.name="Krea_1Y",
                     c("krea_1Y_all-IgA", "krea_1Y_r+IgA", "krea_1Y_r-IgA")),
  create.summary.num(data.ntx, var.name="Krea_1Y", subset.names="krea_1Y_ntx",
                     colname.split=NULL),
  # KREA5Y
  create.summary.num(data.iga, var.name="Krea_5Y",
                     c("krea_5Y_all-IgA", "krea_5Y_r+IgA", "krea_5Y_r-IgA")),
  create.summary.num(data.ntx, var.name="Krea_5Y", subset.names="krea_5Y_ntx",
                     colname.split=NULL),
  # Krea10Y
  create.summary.num(data.iga, var.name="Krea_10Y",
                     c("krea_10Y_all-IgA", "krea_10Y_r+IgA", "krea_10Y_r-IgA")),
  create.summary.num(data.ntx, var.name="Krea_10Y", subset.names="krea_10Y_ntx",
                     colname.split=NULL)
  
))
fwrite(metadata, file = paste0(dir.assets.csv, .Platform$file.sep, "metadata_numeric.csv"))
###=============================================================================
metadata <- rbindlist(list(
  create.summary.fac(data.ntx, colnames.fac = c("Geschlecht", "D_type"),
                     group.name = "all-NTX", percentage = TRUE),
  create.summary.fac(data.iga, colnames.fac = c("Geschlecht", "D_type"),
                     group.name = "all-IgA", percentage = TRUE),
  create.summary.fac(data.iga.pos, colnames.fac = c("Geschlecht", "D_type"),
                     group.name = "r+Iga", percentage = TRUE),
  create.summary.fac(data.iga.neg, colnames.fac = c("Geschlecht", "D_type"),
                     group.name = "r-IgA", percentage = TRUE)
  
))
metadata
fwrite(metadata, file = paste0(dir.assets.csv, .Platform$file.sep, "metadata_factor.csv"))
###=============================================================================
### ANOVA tests
###=============================================================================
## HLA
summary(aov(mismatch_sum ~ Geschlecht, data = data.iga))
#summary(aov(mismatch_sum ~ Geschlecht, data = data.iga.pos))
summary(aov(mismatch_sum ~ Geschlecht, data = data.iga.neg))

rbindlist(list(
  data.iga[, .(mismatch_sum = mismatch_sum, group = as.factor("iga"))],
  data.ntx[, .(mismatch_sum = mismatch_sum, group = as.factor("ntx"))]
)) %>% aov(formula = mismatch_sum ~ group) %>% summary()
###=============================================================================
## current PRA
summary(aov(`current_PRA%` ~ Geschlecht, data = data.iga))
#summary(aov(`current_PRA%` ~ Geschlecht, data = data.iga.pos))
summary(aov(`current_PRA%` ~ Geschlecht, data = data.iga.neg))
summary(aov(current_PRA ~ Geschlecht, data = data.ntx)) #!

rbindlist(list(
  data.iga[, .(c_pra = `current_PRA%`, group = as.factor("iga"))],
  data.ntx[, .(c_pra = current_PRA, group = as.factor("ntx"))]
)) %>% aov(formula = c_pra ~ group) %>% summary()
###=============================================================================
## highest PRA
summary(aov(`Highest_PRA%` ~ Geschlecht, data = data.iga))
#summary(aov(`Highest_PRA%` ~ Geschlecht, data = data.iga.pos))
summary(aov(`Highest_PRA%` ~ Geschlecht, data = data.iga.neg))
summary(aov(highest_PRA ~ Geschlecht, data = data.ntx)) #!

rbindlist(list(
  data.iga[, .(h_pra = `Highest_PRA%`, group = as.factor("iga"))],
  data.ntx[, .(h_pra = highest_PRA, group = as.factor("ntx"))]
)) %>% aov(formula = h_pra ~ group) %>% summary()
###=============================================================================
## time biopsy
summary(aov(`time_of_biopsy_(years_after_KTX)`~`biopsy_proven_recurrence(0=no,1=yes)`, data.iga))
#summary(aov(`time_of_biopsy_(years_after_KTX)`~`biopsy_proven_recurrence(0=no,1=yes)`, data.iga.pos))
# summary(aov(`time_of_biopsy_(years_after_KTX)`~`biopsy_proven_recurrence(0=no,1=yes)`, data.iga.neg))
###=============================================================================
## WILCOX KREA
wilcox.test(data.iga.neg$Krea_10Y, data.iga.pos$Krea_10Y)
wilcox.test(data.iga.neg$Krea_5Y, data.iga.pos$Krea_5Y)
wilcox.test(data.iga.neg$Krea_1Y, data.iga.pos$Krea_1Y)
###=============================================================================
rm( list = ls()[grep(x = ls(), pattern = "^meta")])
cat(paste0(rep("=", 78,), collapse = ""))
cat("\n")
cat("descriptive_metrics.R terminated")
cat("\n")
cat(paste0(rep("=", 78,), collapse = ""))
cat("\n")