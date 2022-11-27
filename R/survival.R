################################################################################
# survival analysis
## time: min(T, C), T=EventTime, C=CensoringTime
## event: 1 if event observed, 0 if censored
################################################################################
# Kaplan Meier estimation preliminaries
covariates_iga <- c("R_age_Tdate", "D-age", "R-sex",  "D-sex",  "D-type",
                    "cold_time_sum_min", "mismatch_sum")
covariates_iga_class <- c("R_age_Tdate_class","D-age", "R-sex", "D-sex",
                          "D-type", "cold_time_sum_class", "mismatch_sum_class")
data_iga[, time_date_biopsy := interval(`T-date`, `date of biopsy`) / years(1)]
data_iga[, time_t_dls := interval(`T-date`, `T-dls`) / years(1)]
data_iga[, time_date_birth := interval(`T-date`, `Date of birth`) / years(1)]
data_iga[, time_graft_loss := interval(`T-date`, `graft loss date`) / years(1)]
data_iga[, time_date_follow_up := interval(`T-date`,`T-date` + follow_up) / years(1)]

################################################################################
# regime 1
################################################################################

data_iga <- create_iga_regime1(data_iga)
data_iga_pos <- data_iga[`biopsy proven recurrence (0=no, 1=yes)` == 1]
data_iga_neg <- data_iga[`biopsy proven recurrence (0=no, 1=yes)` == 0]

## IGA all
model_iga_1 <- survfit(formula = Surv(time = status_date, event = status,
                                      type = "right") ~ 1, data = data_iga)
ggsurvplot(model_iga_1,
           conf.int = FALSE, cumevents = TRUE)
save.plot("kaplan-meier_regime1_iga1.jpg")
surv_median(model_iga_1)
summary(model_iga_1, times = c(1:10))

## IGA positive
model_iga_1.2 <- survfit(formula = Surv(time = status_date,
                                        event = status, type = "right")~ 1,
                         data = data_iga_pos)  
ggsurvplot(model_iga_1.2,
           conf.int = FALSE,
           cumevents = TRUE)
save.plot("kaplan-meier_regime1_igapos.jpg")
surv_median(model_iga_1.2)
summary(model_iga_1.2, times = c(1:10))


## IGA negative
model_iga_1.3 <- survfit(formula = Surv(time = status_date,
                                        event = status, type = "right")~ 1,
                         data = data_iga_neg)  
ggsurvplot(model_iga_1.3,
           conf.int = FALSE,
           cumevents = TRUE)
save.plot("kaplan-meier_regime1_iganeg.jpg")
surv_median(model_iga_1.3)
summary(model_iga_1.3, times = c(1:10))

## NTX
data_ntx <- create_ntx_regime1(data_ntx)
model_ntx_1 <- survfit(formula = Surv(time = status_date, event = status,
                                      type = "right") ~ 1, data = data_ntx)
ggsurvplot(model_ntx_1, conf.int = FALSE, cumevents = TRUE)
save.plot("kaplan-meier_regime1_ntx.jpg")
surv_median(model_ntx_1)
summary(model_ntx_1, times = c(1:10))
###############################################################################
# Log Rank Tests
data.stack <- rbindlist(list(
  data_iga[, .(status, status_date, group="iga")],
  data_ntx[, .(status, status_date, group ="ntx")]
))
### IGA-all vs NTX-all
survdiff(Surv(time = status_date, event = status) ~ group, data = data.stack)

### IGA-neg vs. NTX-all
data.stack <- rbindlist(list(
  data_iga[`biopsy proven recurrence (0=no, 1=yes)` == 0, .(status, status_date, group="iga")],
  data_ntx[, .(status, status_date, group ="ntx")]
))
survdiff(Surv(time = status_date, event = status) ~ group, data = data.stack)

#### IGA-pos vs. NTX.all
data.stack <- rbindlist(list(
  data_iga[`biopsy proven recurrence (0=no, 1=yes)` == 1, .(status, status_date, group="iga")],
  data_ntx[, .(status, status_date, group ="ntx")]
))
survdiff(Surv(time = status_date, event = status) ~ group, data = data.stack)

#### IGA-neg vs- IGA-pos
survdiff(Surv(time = status_date, event = status,
        type = "right") ~ data_iga$`biopsy proven recurrence (0=no, 1=yes)`, data_iga)

################################################################################
# Cox Regression
### all 
model_cox_iga_1 <- coxph(formula = Surv(time = as.numeric(status_date),
                                                       event = status) ~ R_age_Tdate +
                         `D-age` + `R-sex` + `D-sex` + `D-type` + cold_time_sum_min + mismatch_sum , data = data_iga)

summary(model_cox_iga_1)
ggsurvplot(survfit(model_cox_iga_1, data = data_iga), conf.int = FALSE)
save.plot("cox-regression_iga_1.jpg")

#### all permutations
lapply(X = covariates_iga, FUN = function(x){simple.cox.surv(data = data_iga, covariate = x)})

### all no mismatch
model_cox_iga_1_no_mismatch <- coxph(formula = Surv(time = as.numeric(status_date),
                                        event = status) ~ R_age_Tdate +
                           `D-age` + `R-sex` + `D-sex` + `D-type` + cold_time_sum_min , data = data_iga)
summary(model_cox_iga_1_no_mismatch)
ggsurvplot(survfit(model_cox_iga_1, data = data_iga), conf.int = FALSE)
save.plot("cox-regression_iga_1_no_mismatch.jpg")

#### age -> 18-39 ,40-59, >60
data_iga$R_age_Tdate_class <- cut(data_iga$R_age_Tdate, breaks = c(0, 39, 59, Inf), include.lowest = TRUE)
# sum(is.na(data_iga$R_age_Tdate)) == sum(is.na(data_iga$R_age_Tdate_class))
# table(data_iga$R_age_Tdate_class)

#### hla-mm 0-2, 3-5, > 5
data_iga$mismatch_sum_class <- cut(data_iga$mismatch_sum, breaks = c(0, 2, 5, Inf), include.lowest = TRUE)
# sum(is.na(data_iga$mismatch_sum)) == sum(is.na(data_iga$mismatch_sum_class))
# table(data_iga$mismatch_sum_class)

#### cold 0-12, >12
data_iga$cold_time_sum_class <- cut(data_iga$cold_time_sum_min, breaks = c(0, 12 * 60, Inf), include.lowest = TRUE)
# sum(is.na(data_iga$cold_time_sum_min)) == sum(is.na(data_iga$cold_time_sum_class))
# table(data_iga$cold_time_sum_class)


### all
model_cox_iga_1_class <- coxph(formula = Surv(time = as.numeric(status_date),
                                        event = status) ~ R_age_Tdate_class +
                           `D-age` + `R-sex` + `D-sex` + `D-type` + cold_time_sum_class + mismatch_sum_class , data = data_iga)
summary(model_cox_iga_1_class)
ggsurvplot(survfit(model_cox_iga_1_class, data = data_iga), conf.int = FALSE)
save.plot("cox-regression_iga_1_class.jpg")

#### all permutations
lapply(X = covariates_iga_class, FUN = function(x){simple.cox.surv(data = data_iga, covariate = x)})

### no mismatch
model_cox_iga_1_class_no_mismatch <- coxph(formula = Surv(time = as.numeric(status_date),
                                              event = status) ~ R_age_Tdate_class +
                                 `D-age` + `R-sex` + `D-sex` + `D-type` + cold_time_sum_class, data = data_iga)
summary(model_cox_iga_1_class_no_mismatch)
ggsurvplot(survfit(model_cox_iga_1_class, data = data_iga), conf.int = FALSE)
save.plot("cox-regression_iga_1_class_no_mismatch.jpg")
 ################################################################################
# regime 2
################################################################################
data_iga <- create_iga_regime2(data_iga)
data_iga_pos <- data_iga[`biopsy proven recurrence (0=no, 1=yes)` == 1]
data_iga_neg <- data_iga[`biopsy proven recurrence (0=no, 1=yes)` == 0]

## IGA all
model_iga_2 <- survfit(formula = Surv(time = status_date,
                                      event = status, type = "right") ~ 1,
                       data = data_iga)  
ggsurvplot(model_iga_2,
           conf.int = FALSE, cumevents = 1)
save.plot("kaplan-meier_regime2_iga.jpg")
surv_median(model_iga_2)
summary(model_iga_2, times = c(1:10))

## IGA positive
model_iga_2_pos <- survfit(formula = Surv(time = status_date,
                                          event = status, type = "right") ~ data_iga_pos$`biopsy proven recurrence (0=no, 1=yes)`,
                           data = data_iga_pos)  

ggsurvplot(model_iga_2_pos, conf.int = FALSE, cumevents = TRUE)
save.plot("kaplan-meier_regime2_igapos.jpg")
surv_median(model_iga_2_pos)
summary(model_iga_2_pos, times = c(1:10))

## IGA negative
model_iga_2_neg <- survfit(formula = Surv(time = status_date,
                                          event = status, type = "right") ~ data_iga_neg$`biopsy proven recurrence (0=no, 1=yes)`,
                           data = data_iga_neg)  
ggsurvplot(model_iga_2_neg, conf.int = FALSE, cumevents = TRUE)
save.plot("kaplan-meier_regime2_iganeg.jpg")
surv_median(model_iga_2_neg)
summary(model_iga_2_neg, times = c(1:10))

## NTX
data_ntx <- create_ntx_regime2(data_ntx)
model_ntx_2 <- survfit(Surv(time = status_date, event = status,
                            type = "right") ~1, data = data_ntx)
ggsurvplot(model_ntx_2, conf.int = FALSE, cumevents = TRUE)
save.plot("kaplan-meier_regime2_ntx.jpg")
surv_median(model_ntx_2)
summary(model_ntx_2, times = c(1:10))
###############################################################################
# Log Rank Tests

#### IGA-all vs. NTX-all
data.stack <- rbindlist(list(
  data_iga[, .(status, status_date, group="iga")],
  data_ntx[, .(status, status_date, group ="ntx")]
))
survdiff(Surv(time = status_date, event = status) ~ group, data = data.stack)

#### IGA-neg vs. NTX-all
data.stack <- rbindlist(list(
  data_iga[`biopsy proven recurrence (0=no, 1=yes)` == 0, .(status, status_date, group="iga")],
  data_ntx[, .(status, status_date, group ="ntx")]
))
survdiff(Surv(time = status_date, event = status) ~ group, data = data.stack)


#### IGA-pos vs. NTX-all
data.stack <- rbindlist(list(
  data_iga[`biopsy proven recurrence (0=no, 1=yes)` == 1, .(status, status_date, group="iga")],
  data_ntx[, .(status, status_date, group ="ntx")]
))
survdiff(Surv(time = status_date, event = status) ~ group, data = data.stack)

#### IGA-neg vs. IGA-pos
survdiff(Surv(time = status_date, event = status, type = "right") ~ data_iga$`biopsy proven recurrence (0=no, 1=yes)`, data_iga)

################################################################################
# Cox Regression

### all
model_cox_iga_2 <- coxph(formula = Surv(time = as.numeric(status_date),
                                        event = status) ~ R_age_Tdate +
                           `D-age` + `R-sex` + `D-sex` + `D-type` + cold_time_sum_min + mismatch_sum , data = data_iga)

summary(model_cox_iga_2)
ggsurvplot(survfit(model_cox_iga_2, data = data_iga), conf.int = FALSE)
save.plot("cox-regression_iga_2.jpg")

#### all permutations
lapply(X = covariates_iga, FUN = function(x){simple.cox.surv(data = data_iga, covariate = x)})


## no mismatch
model_cox_iga_2_no_mismatch <- coxph(formula = Surv(time = as.numeric(status_date),
                                        event = status) ~ R_age_Tdate +
                           `D-age` + `R-sex` + `D-sex` + `D-type` + cold_time_sum_min , data = data_iga)

summary(model_cox_iga_2_no_mismatch)
ggsurvplot(survfit(model_cox_iga_2_no_mismatch, data = data_iga), conf.int = FALSE)
save.plot("cox-regression_iga_2_no_mismatch.jpg")

#### age -> 18-39 ,40-59, >60
data_iga$R_age_Tdate_class <- cut(data_iga$R_age_Tdate, breaks = c(0, 39, 59, Inf), include.lowest = TRUE)
# sum(is.na(data_iga$R_age_Tdate)) == sum(is.na(data_iga$R_age_Tdate_class))
# table(data_iga$R_age_Tdate_class)

#### hla-mm 0-2, 3-5, > 5
data_iga$mismatch_sum_class <- cut(data_iga$mismatch_sum, breaks = c(0, 2, 5, Inf), include.lowest = TRUE)
# sum(is.na(data_iga$mismatch_sum)) == sum(is.na(data_iga$mismatch_sum_class))
# table(data_iga$mismatch_sum_class)

#### cold 0-12, >12
data_iga$cold_time_sum_class <- cut(data_iga$cold_time_sum_min, breaks = c(0, 12 * 60, Inf), include.lowest = TRUE)
# sum(is.na(data_iga$cold_time_sum_min)) == sum(is.na(data_iga$cold_time_sum_class))
# table(data_iga$cold_time_sum_class)

### all
model_cox_iga_2_class <- coxph(formula = Surv(time = as.numeric(status_date),
                                              event = status) ~ R_age_Tdate_class +
                                 `D-age` + `R-sex` + `D-sex` + `D-type` + cold_time_sum_class + mismatch_sum_class , data = data_iga)
summary(model_cox_iga_2_class)
ggsurvplot(survfit(model_cox_iga_2_class, data = data_iga), conf.int = FALSE)
save.plot("cox-regression_iga_2_class.jpg")

#### all permutations
lapply(X = covariates_iga_class, FUN = function(x){simple.cox.surv(data = data_iga, covariate = x)})

### no mismatch
model_cox_iga_2_class_no_mismatch <- coxph(formula = Surv(time = as.numeric(status_date),
                                              event = status) ~ R_age_Tdate_class +
                                 `D-age` + `R-sex` + `D-sex` + `D-type` + cold_time_sum_class , data = data_iga)
summary(model_cox_iga_2_class_no_mismatch)
ggsurvplot(survfit(model_cox_iga_2_class_no_mismatch, data = data_iga), conf.int = FALSE)
save.plot("cox-regression_iga_2_class.jpg")
