###=============================================================================
# survival analysis
## time: min(T, C), T=EventTime, C=CensoringTime
## event: 1 if event observed, 0 if censored
###=============================================================================
## preliminaries
covariates.iga <- c("R_age_surgery", "D_age", "Geschlecht",  "D_sex",  "D_type", "current_PRA%", "Highest_PRA%",
                    "cold_time_minutes", "mismatch_sum")
covariates.ntx <- c("R_age_surgery", "D_age", "Geschlecht", "D_sex", "D_type", "current_PRA", "highest_PRA",
                    "cold_time_minutes", "mismatch_sum")
covariates.iga.class <- c("R_age_surgery_class","D_age", "Geschlecht", "D_sex",
                          "D_type","current_PRA%", "Highest_PRA%", "cold_time_minutes_class", "mismatch_sum_class")
if (!all(covariates.iga %in% colnames(data.iga))){
  stop(paste0("Not all names provided incovariates.iga could be found:\n"),
              paste(covariates.iga, ":", covariates.iga %in% colnames(data.iga), "\n"))
}
# data.iga.backup <- data.iga
# data.ntx.backup <- data.ntx
data.iga[, time_date_biopsy := interval(Datum_TX, date_of_biopsy) / years(1)]
data.iga[, time_t_dls := interval(Datum_TX, Date_last_seen) / years(1)]
data.iga[, time_date_birth := interval(Datum_TX, Geburtsdatum) / years(1)]
data.iga[, time_graft_loss := interval(Datum_TX, Transplantatfunktionsende) / years(1)]
data.iga[, time_date_follow_up := interval(Datum_TX, Datum_TX + follow_up) / years(1)]
###=============================================================================
# regime 1
################################################################################

data.iga <- create.iga.regime1(data.iga)
data.iga.pos <- data.iga[`biopsy_proven_recurrence(0=no,1=yes)` == 1]
data.iga.neg <- data.iga[`biopsy_proven_recurrence(0=no,1=yes)` == 0]

data.ntx <- create.ntx.regime1(data.ntx)

## IGA all
## Kaplan Meier all iga
model_iga_1 <- survfit(formula = Surv(time = status_date, event = status,
                                      type = "right") ~ 1, data = data.iga)
ggsurvplot(model_iga_1,
           conf.int = FALSE, cumevents = TRUE)
save.plot("kaplan-meier_regime1_iga1.jpg")
surv_median(model_iga_1)
summary(model_iga_1, times = c(1:10))

## Kaplan Meier iga+/iga-
model_iga_1.1 <- survfit(formula = Surv(time = status_date,
                                        event = status,
                                        type = "right") ~ data.iga$`biopsy_proven_recurrence(0=no,1=yes)`,
                                        data = data.iga)
ggsurvplot(model_iga_1.1, pval = TRUE, legend.labs = c("r-IgA", "r+IgA"))
save.plot("kapla-meier_regime1_iga_LogRank.jpg")


## IGA positive
model_iga_1.2 <- survfit(formula = Surv(time = status_date,
                                        event = status, type = "right")~ 1,
                         data = data.iga.pos)  
ggsurvplot(model_iga_1.2,
           conf.int = FALSE,
           cumevents = TRUE)
save.plot("kaplan-meier_regime1_igapos.jpg")
surv_median(model_iga_1.2)
summary(model_iga_1.2, times = c(1:10))


## IGA negative
model_iga_1.3 <- survfit(formula = Surv(time = status_date,
                                        event = status, type = "right")~ 1,
                         data = data.iga.neg)  
ggsurvplot(model_iga_1.3,
           conf.int = FALSE,
           cumevents = TRUE)
save.plot("kaplan-meier_regime1_iganeg.jpg")
surv_median(model_iga_1.3)
summary(model_iga_1.3, times = c(1:10))

## NTX
model_ntx_1 <- survfit(formula = Surv(time = status_date, event = status,
                                      type = "right") ~ 1, data = data.ntx)
ggsurvplot(model_ntx_1, conf.int = FALSE, cumevents = TRUE)
save.plot("kaplan-meier_regime1_ntx.jpg")
surv_median(model_ntx_1)
summary(model_ntx_1, times = c(1:10))
###############################################################################
# Log Rank Tests
data.stack <- rbindlist(list(
  data.iga[, .(status, status_date, group="iga")],
  data.ntx[, .(status, status_date, group ="ntx")]
))
### IGA-all vs NTX-all
survdiff(Surv(time = status_date, event = status) ~ group, data = data.stack)
#### kaplan meier
model_log_rank_1 <- survfit(formula = Surv(time = status_date,
                                           event = status,
                                           type = "right") ~ data.stack$group,
                            data = data.stack)
ggsurvplot(model_log_rank_1, pval = TRUE, legend.labs = c("all-IgA", "all-NTX"))
save.plot("kaplan-meier_log-rank_1.jpg")


### IGA-neg vs. NTX-all
data.stack <- rbindlist(list(
  data.iga[`biopsy_proven_recurrence(0=no,1=yes)` == 0, .(status, status_date, group="iga")],
  data.ntx[, .(status, status_date, group ="ntx")]
))
survdiff(Surv(time = status_date, event = status) ~ group, data = data.stack)

#### IGA-pos vs. NTX.all
data.stack <- rbindlist(list(
  data.iga[`biopsy_proven_recurrence(0=no,1=yes)` == 1, .(status, status_date, group="iga")],
  data.ntx[, .(status, status_date, group ="ntx")]
))
survdiff(Surv(time = status_date, event = status) ~ group, data = data.stack)

#### IGA-neg vs- IGA-pos
survdiff(Surv(time = status_date, event = status,
        type = "right") ~ data.iga$`biopsy_proven_recurrence(0=no,1=yes)`, data.iga)

################################################################################
# Cox Regression
### all 
model_cox_iga_1 <- coxph(formula = Surv(time = as.numeric(status_date),
                                                       event = status) ~ R_age_surgery +
                         D_age + Geschlecht + D_sex + D_type + 
                           cold_time_minutes + mismatch_sum + `current_PRA%` +
                           `Highest_PRA%` + `biopsy_proven_recurrence(0=no,1=yes)`, data = data.iga)

summary(model_cox_iga_1)
ggsurvplot(survfit(model_cox_iga_1, data = data.iga), conf.int = FALSE)
save.plot("cox-regression_iga_1.jpg")
fwrite(tidy(model_cox_iga_1, conf.int = TRUE, exponentiate = TRUE),
       file = paste0(dir.assets.csv, .Platform$file.sep, "model_cox_iga_1_all.csv"))
#### all permutations
lapply(X = c(covariates.iga, "biopsy_proven_recurrence(0=no,1=yes)"),
       FUN = function(x){simple.cox.surv(data = data.iga, covariate = x, name.prefix = "iga_1_")})

### all no mismatch
model_cox_iga_1_no_mismatch <- coxph(formula = Surv(time = as.numeric(status_date),
                                        event = status) ~ R_age_surgery +
                                       D_age + Geschlecht + D_sex + D_type +
                                       cold_time_minutes + mismatch_sum + `current_PRA%` +
                                       `Highest_PRA%` + `biopsy_proven_recurrence(0=no,1=yes)`, data = data.iga)
summary(model_cox_iga_1_no_mismatch)
ggsurvplot(survfit(model_cox_iga_1, data = data.iga), conf.int = FALSE)
save.plot("cox-regression_iga_1_no_mismatch.jpg")
fwrite(tidy(model_cox_iga_1_no_mismatch, conf.int = TRUE, exponentiate = TRUE),
       file = paste0(dir.assets.csv, .Platform$file.sep, "model_cox_iga_1_no_mismatch.csv"))

## all NTX
### with mismatch
model_cox_ntx_1 <- coxph(formula = Surv(time = as.numeric(status_date),
                                      event = status) ~ R_age_surgery +
                         D_age + Geschlecht + D_sex + D_type + current_PRA + highest_PRA +
                         cold_time_minutes + mismatch_sum, data = data.ntx)
summary(model_cox_ntx_1)
ggsurvplot(survfit(model_cox_ntx_1, data = data.ntx), conf.int = FALSE)
fwrite(tidy(model_cox_ntx_1, conf.int = TRUE, exponentiate = TRUE),
       file = paste0(dir.assets.csv, .Platform$file.sep, "model_cox_ntx_1.csv"))
### no mismatch
model_cox_ntx_1_no_mismatch <- coxph(formula = Surv(time = as.numeric(status_date),
                                        event = status) ~ R_age_surgery +
                           D_age + Geschlecht + D_sex + D_type + current_PRA + highest_PRA +
                           cold_time_minutes , data = data.ntx)
summary(model_cox_ntx_1_no_mismatch)
ggsurvplot(survfit(model_cox_ntx_1_no_mismatch, data = data.ntx), conf.int = FALSE)
fwrite(tidy(model_cox_ntx_1_no_mismatch, conf.int = TRUE, exponentiate = TRUE),
       file = paste0(dir.assets.csv, .Platform$file.sep, "model_cox_ntx_1_no_mismatch.csv"))
### all permutations
lapply(X = covariates.ntx, FUN = function(x){simple.cox.surv(data = data.ntx, covariate = x, name.prefix = "ntx_1_")})
###=============================================================================
###=============================================================================
#### age -> 18-39 ,40-59, >60
data.iga$R_age_surgery_class <- cut(data.iga$R_age_surgery, breaks = c(0, 39, 59, Inf), include.lowest = TRUE)
# sum(is.na(data_iga$R_age_Tdate)) == sum(is.na(data_iga$R_age_Tdate_class))
# table(data_iga$R_age_Tdate_class)

#### hla-mm 0-2, 3-5, > 5
data.iga$mismatch_sum_class <- cut(data.iga$mismatch_sum, breaks = c(0, 2, 5, Inf), include.lowest = TRUE)
# sum(is.na(data_iga$mismatch_sum)) == sum(is.na(data_iga$mismatch_sum_class))
# table(data_iga$mismatch_sum_class)

#### cold 0-12, >12
data.iga$cold_time_minutes_class <- cut(data.iga$cold_time_minutes, breaks = c(0, 12 * 60, Inf), include.lowest = TRUE)
# sum(is.na(data_iga$cold_time_sum_min)) == sum(is.na(data_iga$cold_time_sum_class))
# table(data_iga$cold_time_sum_class)
###=============================================================================
### all
model_cox_iga_1_class <- coxph(formula = Surv(time = as.numeric(status_date),
                                        event = status) ~ R_age_surgery_class +
                                 D_age + Geschlecht + D_sex + D_type + cold_time_minutes_class +
                                 mismatch_sum_class + `current_PRA%` + `Highest_PRA%`+ `biopsy_proven_recurrence(0=no,1=yes)` , data = data.iga)
summary(model_cox_iga_1_class)
ggsurvplot(survfit(model_cox_iga_1_class, data = data.iga), conf.int = FALSE)
save.plot("cox-regression_iga_1_class.jpg")
fwrite(tidy(model_cox_iga_1_class, conf.int = TRUE, exponentiate = TRUE),
       file = paste0(dir.assets.csv, .Platform$file.sep, "model_cox_iga_1_all_class.csv"))

#### all permutations
if (!(all(covariates.iga.class %in% colnames(data.iga)))){
  stop(paste0("some column names in covariates.iga.class not found in data:\n"),
       paste0(covariates.iga.class, " : ", covariates.iga.class %in% colnames(data.iga), "\n")
  )
}
lapply(X = c(covariates.iga.class, "biopsy_proven_recurrence(0=no,1=yes)"),
       FUN = function(x){simple.cox.surv(data = data.iga, covariate = x, name.prefix = "iga_1_class_")})

### no mismatch
model_cox_iga_1_class_no_mismatch <- coxph(formula = Surv(time = as.numeric(status_date),
                                              event = status) ~ R_age_surgery_class +
                                             D_age + Geschlecht + D_sex + D_type +
                                             cold_time_minutes_class + mismatch_sum +
                                             `current_PRA%` + `Highest_PRA%` + `biopsy_proven_recurrence(0=no,1=yes)`, data = data.iga)
summary(model_cox_iga_1_class_no_mismatch)
ggsurvplot(survfit(model_cox_iga_1_class, data = data.iga), conf.int = FALSE)
save.plot("cox-regression_iga_1_class_no_mismatch.jpg")
fwrite(tidy(model_cox_iga_1_class_no_mismatch, conf.int = TRUE, exponentiate = TRUE),
       file = paste0(dir.assets.csv, .Platform$file.sep, "model_cox_iga_1_no_mismatch_class.csv"))

 ################################################################################
#
# regime 2
#
################################################################################
data_ntx <- create.ntx.regime2(data.ntx)

data.iga <- create.iga.regime2(data.iga)
data.iga.pos <- data.iga[`biopsy_proven_recurrence(0=no,1=yes)` == 1]
data.iga.neg <- data.iga[`biopsy_proven_recurrence(0=no,1=yes)` == 0]

## IGA all
## kaplan-meier all iga
model_iga_2 <- survfit(formula = Surv(time = status_date,
                                      event = status, type = "right") ~ 1,
                       data = data.iga)  
ggsurvplot(model_iga_2,
           conf.int = FALSE, cumevents = 1)
save.plot("kaplan-meier_regime2_iga.jpg")
surv_median(model_iga_2)
summary(model_iga_2, times = c(1:10))
## kaplan-meier iga+/iga-
model_iga_2.1 <- survfit(formula = Surv(time = status_date,
                                        event = status,
                                        type = "right") ~ data.iga$`biopsy_proven_recurrence(0=no,1=yes)`,
                         data = data.iga)
ggsurvplot(model_iga_2.1, pval = TRUE, legend.labs = c("r-IgA", "r+IgA"))
save.plot("kapla-meier_regime2_iga_LogRank.jpg")

## IGA positive
model_iga_2_pos <- survfit(formula = Surv(time = status_date,
                                          event = status,
                                          type = "right") ~ data.iga.pos$`biopsy_proven_recurrence(0=no,1=yes)`,
                           data = data.iga.pos)  

ggsurvplot(model_iga_2_pos, conf.int = FALSE, cumevents = TRUE)
save.plot("kaplan-meier_regime2_igapos.jpg")
surv_median(model_iga_2_pos)

## IGA negative
model_iga_2_neg <- survfit(formula = Surv(time = status_date,
                                          event = status,
                                          type = "right") ~ data.iga.neg$`biopsy_proven_recurrence(0=no,1=yes)`,
                           data = data.iga.neg)  
ggsurvplot(model_iga_2_neg, conf.int = FALSE, cumevents = TRUE)
save.plot("kaplan-meier_regime2_iganeg.jpg")
surv_median(model_iga_2_neg)
summary(model_iga_2_neg, times = c(1:10))

## NTX
model_ntx_2 <- survfit(Surv(time = status_date, event = status,
                            type = "right") ~1, data = data.ntx)
ggsurvplot(model_ntx_2, conf.int = FALSE, cumevents = TRUE)
save.plot("kaplan-meier_regime2_ntx.jpg")
surv_median(model_ntx_2)
summary(model_ntx_2, times = c(1:10))
###############################################################################
# Log Rank Tests

#### IGA-all vs. NTX-all
data.stack <- rbindlist(list(
  data.iga[, .(status, status_date, group="iga")],
  data.ntx[, .(status, status_date, group ="ntx")]
))
survdiff(Surv(time = status_date, event = status) ~ group, data = data.stack)
## kaplan-meier
model_log_rank_2 <- survfit(formula = Surv(time = status_date,
                                           event = status,
                                           type = "right") ~ data.stack$group,
                            data = data.stack)
ggsurvplot(model_log_rank_2, pval = TRUE, legend.labs = c("all-IgA", "all-NTX"))
save.plot("kaplan-meier_log-rank_2.jpg")

#### IGA-neg vs. NTX-all
data.stack <- rbindlist(list(
  data.iga[`biopsy_proven_recurrence(0=no,1=yes)` == 0, .(status, status_date, group="iga")],
  data.ntx[, .(status, status_date, group ="ntx")]
))
survdiff(Surv(time = status_date, event = status) ~ group, data = data.stack)


#### IGA-pos vs. NTX-all
data.stack <- rbindlist(list(
  data.iga[`biopsy_proven_recurrence(0=no,1=yes)` == 1, .(status, status_date, group="iga")],
  data.ntx[, .(status, status_date, group ="ntx")]
))
survdiff(Surv(time = status_date, event = status) ~ group, data = data.stack)

#### IGA-neg vs. IGA-pos
survdiff(Surv(time = status_date, event = status,
              type = "right") ~ data.iga$`biopsy_proven_recurrence(0=no,1=yes)`, data.iga)

################################################################################
# Cox Regression
### all
model_cox_iga_2 <- coxph(formula = Surv(time = as.numeric(status_date),
                                        event = status) ~ R_age_surgery +
                           D_age + Geschlecht + D_sex + D_type + cold_time_minutes + mismatch_sum +
                           `current_PRA%` + `Highest_PRA%` + `biopsy_proven_recurrence(0=no,1=yes)`, data = data.iga)

summary(model_cox_iga_2)
ggsurvplot(survfit(model_cox_iga_2, data = data.iga), conf.int = FALSE)
save.plot("cox-regression_iga_2.jpg")
fwrite(tidy(model_cox_iga_2, conf.int = TRUE, exponentiate = TRUE),
       file = paste0(dir.assets.csv, .Platform$file.sep, "model_cox_iga_2_all.csv"))

#### all permutations
lapply(X = c(covariates.iga, "biopsy_proven_recurrence(0=no,1=yes)"),
       FUN = function(x){simple.cox.surv(data = data.iga, covariate = x, name.prefix = "iga_2_")})

## no mismatch
model_cox_iga_2_no_mismatch <- coxph(formula = Surv(time = as.numeric(status_date),
                                        event = status) ~ R_age_surgery +
                           D_age + Geschlecht + D_sex + D_type + cold_time_minutes +
                             mismatch_sum + `current_PRA%` + `Highest_PRA%` +`biopsy_proven_recurrence(0=no,1=yes)`, data = data.iga)

summary(model_cox_iga_2_no_mismatch)
ggsurvplot(survfit(model_cox_iga_2_no_mismatch, data = data.iga), conf.int = FALSE)
save.plot("cox-regression_iga_2_no_mismatch.jpg")
fwrite(tidy(model_cox_iga_2_no_mismatch, conf.int = TRUE, exponentiate = TRUE),
       file = paste0(dir.assets.csv, .Platform$file.sep, "model_cox_iga_2_no_mismatch.csv"))


## NTX
model_cox_ntx_2 <- coxph(formula = Surv(time = as.numeric(status_date),
                                        event = status) ~ R_age_surgery +
                           D_age + Geschlecht + D_sex + D_type + current_PRA + highest_PRA +
                           cold_time_minutes + mismatch_sum, data = data.ntx)
summary(model_cox_ntx_2)
ggsurvplot(survfit(model_cox_ntx_2, data = data.ntx), conf.int = FALSE)
fwrite(tidy(model_cox_ntx_2, conf.int = TRUE, exponentiate = TRUE),
       file = paste0(dir.assets.csv, .Platform$file.sep, "model_cox_ntx_2.csv"))
### no mismatch
model_cox_ntx_2_no_mismatch <- coxph(formula = Surv(time = as.numeric(status_date),
                                                    event = status) ~ R_age_surgery +
                                       D_age + Geschlecht + D_sex + D_type + current_PRA + highest_PRA +
                                       cold_time_minutes , data = data.ntx)
summary(model_cox_ntx_2_no_mismatch)
ggsurvplot(survfit(model_cox_ntx_2_no_mismatch, data = data.ntx), conf.int = FALSE)
fwrite(tidy(model_cox_ntx_2_no_mismatch, conf.int = TRUE, exponentiate = TRUE),
       file = paste0(dir.assets.csv, .Platform$file.sep, "model_cox_ntx_2_no_mismatch.csv"))

###=============================================================================
#### age -> 18-39 ,40-59, >60
data.iga$R_age_surgery_class <- cut(data.iga$R_age_surgery, breaks = c(0, 39, 59, Inf), include.lowest = TRUE)
# sum(is.na(data.iga$R_age_surgery)) == sum(is.na(data.iga$R_age_surgery_class))
# table(data.iga$R_age_surgery_class)

#### hla-mm 0-2, 3-5, > 5
data.iga$mismatch_sum_class <- cut(data.iga$mismatch_sum, breaks = c(0, 2, 5, Inf), include.lowest = TRUE)
# sum(is.na(data.iga$mismatch_sum)) == sum(is.na(data.iga$mismatch_sum_class))
# table(data.iga$mismatch_sum_class)

#### cold 0-12, >12
data.iga$cold_time_minutes_class <- cut(data.iga$cold_time_minutes, breaks = c(0, 12 * 60, Inf), include.lowest = TRUE)
# sum(is.na(data.iga$cold_time_minutes)) == sum(is.na(data.iga$cold_time_minutes_class))
# table(data.iga$cold_time_minutes_class)

### all
model_cox_iga_2_class <- coxph(formula = Surv(time = as.numeric(status_date),
                                              event = status) ~ R_age_surgery_class +
                                 D_age + Geschlecht + D_sex + D_type + cold_time_minutes_class +
                                 mismatch_sum + `current_PRA%` + `Highest_PRA%` * `biopsy_proven_recurrence(0=no,1=yes)` , data = data.iga)
summary(model_cox_iga_2_class)
ggsurvplot(survfit(model_cox_iga_2_class, data = data.iga), conf.int = FALSE)
save.plot("cox-regression_iga_2_class.jpg")
fwrite(tidy(model_cox_iga_2_class, conf.int = TRUE, exponentiate = TRUE),
       file = paste0(dir.assets.csv, .Platform$file.sep, "model_cox_iga_2_all_class.csv"))

#### all permutations
lapply(X = c(covariates.iga.class, "biopsy_proven_recurrence(0=no,1=yes)"), FUN = function(x){simple.cox.surv(data = data.iga, covariate = x, name.prefix = "iga_2_class_")})

### no mismatch
model_cox_iga_2_class_no_mismatch <- coxph(formula = Surv(time = as.numeric(status_date),
                                              event = status) ~ R_age_surgery_class +
                                 D_age + Geschlecht + D_sex + D_type + cold_time_minutes_class +
                                   mismatch_sum + `current_PRA%` + `Highest_PRA%` + `biopsy_proven_recurrence(0=no,1=yes)`,
                                 data = data.iga)
summary(model_cox_iga_2_class_no_mismatch)
ggsurvplot(survfit(model_cox_iga_2_class_no_mismatch, data = data.iga), conf.int = FALSE)
save.plot("cox-regression_iga_2_class.jpg")
fwrite(tidy(model_cox_iga_2_class_no_mismatch, conf.int = TRUE, exponentiate = TRUE),
       file = paste0(dir.assets.csv, .Platform$file.sep, "model_cox_iga_2_no_mismatch_class.csv"))

cat(paste0(rep("=", 78,), collapse = ""))
cat("\n")
cat("survival.R terminated")
cat("\n")
cat(paste0(rep("=", 78,), collapse = ""))
cat("\n")