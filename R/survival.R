################################################################################
# survival analysis
## time: min(T, C), T=EventTime, C=CensoringTime
## event: 1 if event observed, 0 if censored
################################################################################
# Kaplan Meier estimation
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

