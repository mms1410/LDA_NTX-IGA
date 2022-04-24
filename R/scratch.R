data_iga1$`Date last seen`
data_iga1$Todesdatum
date1 <- data_iga1$`Date last seen`[4]
date2 <- data_iga1$Todesdatum[1]
nrow(data_iga1[!is.na(data_iga1$`Date last seen`) | !is.na(data_iga1$Todesdatum)])
col_names <- c("Date last seen", "Todesdatum")
data_iga1[, col_names, with=FALSE]
data_iga1[, censor_date :=  min(`Date last seen`, Todesdatum, na.rm = TRUE)] # columnwise ((
data_iga2[, censor_date := mapply(FUN = min())]
date_min <- function(x) {
  min(x, na.rm = TRUE)
}

censoring <- c("Date last seen", "Todesdatum")
event <- "TX Status"
tbl_censoring <- data_iga1[ , censoring, with = FALSE]
tbl_event <- data_iga1[ , event, with = FALSE]

################################################################################
# iga
## KTX:
## T-dls:
## T-date:

y <- regmatches(x,regexpr(pattern = "^([0]?[1-9]|[1|2][0-9]|[3][0|1])[./-]([0]?[1-9]|[1][0-2])[./-]([0-9]{4}|[0-9]{2})",
     text = x))



follow_up_time <- data_iga2$`T-date` + lubridate::years(10)
censoring_date <- c("T-dls" )
event_date <- data_iga2$`graft loss date`

##
obs <- data_iga2[4,]

apply(data_iga2, MARGIN = 1, FUN = survival_right)

data_iga2[, censor := survival_right(`graft loss date`, `T-date`, `T-dls`)]

###############################################################################
# numeric preprocessing
data_iga2[, num_t_date := as.numeric(`T-date` - `T-date`)]
data_iga2[, num_t_dls := as.numeric(`T-dls` - `T-date`)]
data_iga2[, num_date_biopsy := as.numeric(`date of biopsy` - `T-date`)]
data_iga2[, num_date_birth := as.numeric(`Date of birth` - `T-date`)]
data_iga2[, num_graft_loss := as.numeric(`graft loss date` - `T-date`)]

follow_up <- 10 * 365
################################################################################
censor <- ifelse(!is.na(data_iga2$num_graft_loss),
                 ## YES, graft loss occured
                 ifelse( data_iga2$num_graft_loss > data_iga2$num_t_date + follow_up,
                         ## YES, graft loss occured but after follow up period
                         c("censoring" = data_iga2$num_t_date + follow_up,
                           "event" = 0 ),
                         ## NO, graft loss occured withi follow up period
                         c("censoring" = data_iga2$num_graft_loss,
                           "event" = 1)
                 ),
                 ## NO, no graft loss occured
                 ifelse( (!is.na(data_iga2$num_t_dls) & (data_iga2$num_t_dls < (data_iga2$num_t_date + follow_up))),
                         ## YES, but right censored before follow_up
                         c("censoring" = data_iga2$num_t_dls,
                           "event" = 0),
                         ## NO, 
                         c("censoring" = data_iga2$num_t_date + follow_up,
                           "event" = 0)
                 )
)
event <- ifelse(!is.na(data_iga2$num_graft_loss),
                ## YES, graft loss occured
                ifelse( data_iga2$num_graft_loss > data_iga2$num_t_date + follow_up,
                        ## YES, graft loss occured but after follow up period
                        0,
                        ## NO, graft loss occured withi follow up period
                        1
                ),
                ## NO, no graft loss occured
                ifelse( (!is.na(data_iga2$num_t_dls) & (data_iga2$num_t_dls < (data_iga2$num_t_date + follow_up))),
                        ## YES, but right censored before follow_up
                        0,
                        ## NO, 
                        0
                )
)

surv_fit <- survfit(formula = Surv(time = censor,
                                   event = event, type = "right")~ 1,
                    data = data_iga2)
surv_fit
ggsurvplot(surv_fit)
################################################################################
################################################################################



censor <- ifelse(!is.na(data_iga2$`graft loss date`),
       ## YES, graft loss occured
       ifelse( data_iga2$`graft loss date` > data_iga2$`T-date` + follow_up,
               ## YES, graft loss occured but after follow up period
               c("censoring" = data_iga2$`T-date` + follow_up,
                 "event" = 0 ),
               ## NO, graft loss occured withi follow up period
               c("censoring" = data_iga2$`graft loss date`,
                 "event" = 1)
               ),
       ## NO, no graft loss occured
       ifelse( (!is.na(data_iga2$`T-dls`) & (data_iga2$`T-dls` < (data_iga2$`T-date` + follow_up))),
               ## YES, but right censored before follow_up
               c("censoring" = data_iga2$`T-dls`,
                 "event" = 0),
               ## NO, 
               c("censoring" = data_iga2$`T-date` + follow_up,
                 "event" = 0)
       )
)
event <- ifelse(!is.na(data_iga2$`graft loss date`),
                 ## YES, graft loss occured
                 ifelse( data_iga2$`graft loss date` > data_iga2$`T-date` + follow_up,
                         ## YES, graft loss occured but after follow up period
                         0,
                         ## NO, graft loss occured withi follow up period
                         1
                 ),
                 ## NO, no graft loss occured
                 ifelse( (!is.na(data_iga2$`T-dls`) & (data_iga2$`T-dls` < (data_iga2$`T-date` + follow_up))),
                         ## YES, but right censored before follow_up
                         0,
                         ## NO, 
                         0
                 )
)
surv_fit_biopsy <- survfit(formula = Surv(time = censor, event = event, type = "right") ~ data_iga2$`biopsy proven recurrence (0=no, 1=yes)`)
surv_fit <- survfit(formula = Surv(time = censor, event = event, type = "right") ~ 1)
p_biopsy <- ggsurvplot(surv_fit_biopsy, data = data_iga2)
p <- ggsurvplot(surv_fit, data = data_iga2)
p_biopsy
p
################################################################################
censor <- ifelse( data_iga2$`Pat death (0=alive, 1= dead)` == 1,
  ## YES patient died
  ifelse( data_iga2$`T-dls` > data_iga2$`T-date` + follow_up, 
    ## YES patient died but after follow up -> right censored
    data_iga2$`T-date` + follow_up,
    ## NO, patient died within follow up
    data_iga2$`T-dls`
  ),
  ifelse(data_iga2$`T-dls` < data_iga2$`T-date` + follow_up,
         data_iga2$`T-dls`,
         data_iga2$`T-date` + follow_up
  )
)
event <- ifelse( data_iga2$`Pat death (0=alive, 1= dead)` == 1,
                 ## YES patient died
                 ifelse( data_iga2$`T-dls` > data_iga2$`T-date` + follow_up, 
                         ## YES patient died but after follow up -> right censored
                         0,
                         ## NO, patient died within follow up
                         1
                 ),
                 ifelse(data_iga2$`T-dls` < data_iga2$`T-date` + follow_up,
                        0,
                        0
                 )
)
surv_fit <- survfit(formula = Surv(time = censor, event = event, type = "right") ~ 1)
p <- ggsurvplot(surv_fit, data = data_iga2)
p
###
surv_fit <- survfit(formula = Surv(time = data_iga2$`max FUP survivial (years)` * 365,
                          event = data_iga2$`Pat death (0=alive, 1= dead)`,
                          type = "right") ~ 1, data = data_iga2, conf.type = "none")
surv_fit
ggsurvplot(surv_fit)

data_iga2$`max FUP survivial (years)`
Surv(tome = )
################################################################################
data_iga2 %>% 
  ## censor/event date
  mutate(censor_date = case_when(
    ## patient dropped during follow up
    (`T-dls` <= `T-date` + follow_up) ~ `T-dls`,
    ## patient experienced graft loss but after follow up
    ((!is.na(`graft loss date`)) &  (`graft loss date` > `T-date` + follow_up)) ~ `T-date` + follow_up,
    ## patient experienced graft loss within follow up
    ((!is.na(`graft loss date`)) &  (`graft loss date` <= `T-date` + follow_up)) ~ `graft loss date`
  )) 
################################################################################

data_iga2[!is.na(data_iga2$`graft loss date`) && (data_iga2$`graft loss date` <= data_iga2$`T-date` + follow_up)]
