################################################################################
# this script contains descriptive statistics on iga patients containing patient
# characteristics only
################################################################################
################################################################################
##
## table containing mean time in years of interval between surgery date and date last seen
tbl_iga_follow_up_mean <- data.frame(
  iga_all = mean((interval(data_iga$`T-date`, data_iga$`T-dls`) / years(1))),
  iga_pos = mean((interval(data_iga_pos$`T-date`, data_iga_pos$`T-dls`) / years(1))),
  iga_neg = mean((interval(data_iga_neg$`T-date`, data_iga_neg$`T-dls`) / years(1)))
)
## table containing median time in years of interval between surgery date and date last seen
tbl_iga_follow_up_median <- data.frame(
  iga_all = median((interval(data_iga$`T-date`, data_iga$`T-dls`) / years(1))),
  iga_pos = median((interval(data_iga_pos$`T-date`, data_iga_pos$`T-dls`) / years(1))),
  iga_neg = median((interval(data_iga_neg$`T-date`, data_iga_neg$`T-dls`) / years(1)))
)
################################################################################
## plot box plots for time until date last seen
## ToDo: All IGA
ggplot() +
  geom_boxplot(aes(x = `biopsy proven recurrence (0=no, 1=yes)`,
                   y = interval(data_iga$`T-date`, data_iga$`T-dls`) / years(1)), data = data_iga) +
  default_theme +
  ggtitle("Zeitspanne zwischen Operation und Datum zuletzt gesehen") +
  ylab("Jahre") +
  xlab("") +
  scale_x_discrete(labels = c("ohne recurrence", "mit recurrence"))
save.plot("plot_1_lda.jpg")
## follow_up mean/median (truncated)
tbl_iga_follow_up_mean_truncated <- data.frame(
  iga_all = mean((interval(data_iga$`T-date`, data_iga$follow_up_truncated) / years(1))),
  iga_pos = mean((interval(data_iga_pos$`T-date`, data_iga_pos$follow_up_truncated) / years(1))),
  iga_neg = mean((interval(data_iga_neg$`T-date`, data_iga_neg$follow_up_truncated) / years(1)))
)
tbl_iga_follow_up_median_truncated <- data.frame(
  iga_all = median((interval(data_iga$`T-date`, data_iga$follow_up_truncated) / years(1))),
  iga_pos = median((interval(data_iga_pos$`T-date`, data_iga_pos$follow_up_truncated) / years(1))),
  iga_neg = median((interval(data_iga_neg$`T-date`, data_iga_neg$follow_up_truncated) / years(1)))
)
ggplot() +
  geom_boxplot(aes(x = `biopsy proven recurrence (0=no, 1=yes)`,
                   y = interval(data_iga$`T-date`, data_iga$follow_up_truncated) / years(1)), data = data_iga) +
  default_theme +
  ggtitle("Zeitspanne zwischen Operation und Datum zuletzt gesehen",
          subtitle = "(trunkiert ab follow up)") +
  ylab("Jahre") +
  xlab("") +
  scale_x_discrete(labels = c("ohne recurrence", "mit recurrence"))
save.plot("plot_1_lda_trunkiert.jpg")
################################################################################
## number of patients that died during follow-up period
tbl_iga_pat_death <- data.frame(
  iga_all = nrow(data_iga[(`T-dls` <= (`T-date` + follow_up)) & `Pat death (0=alive, 1= dead)` == 1]),
  iga_pos = nrow(data_iga_pos[(`T-dls` <= (`T-date` + follow_up)) & `Pat death (0=alive, 1= dead)` == 1]),
  iga_neg = nrow(data_iga_neg[(`T-dls` <= (`T-date` + follow_up)) & `Pat death (0=alive, 1= dead)` == 1])
)
################################################################################
## number of patient that dropped out during follow-up period
tbl_iga_pat_drop <- data.frame(
  iga_all = nrow(data_iga[(`T-dls` <= (`T-date` + follow_up)) & `Pat death (0=alive, 1= dead)` == 0]),
  iga_pos = nrow(data_iga_pos[(`T-dls` <= (`T-date` + follow_up)) & `Pat death (0=alive, 1= dead)` == 0]),
  iga_neg = nrow(data_iga_neg[(`T-dls` <= (`T-date` + follow_up)) & `Pat death (0=alive, 1= dead)` == 0])
)
################################################################################
## number of patients with graft loss 
tbl_iga_graft_loss <- data.frame(
  iga_all = nrow(data_iga[`graft loss (0=functial, 1=loss)` == 1]),
  iga_pos = nrow(data_iga_pos[`graft loss (0=functial, 1=loss)` == 1]),
  iga_neg = nrow(data_iga_neg[`graft loss (0=functial, 1=loss)` == 1])
)
################################################################################
## number of patients with graft loss during follow up period
tbl_iga_graft_loss_follow_up <- data.frame(
  iga_all = nrow(data_iga[`graft loss date` < `T-date` + follow_up]),
  iga_pos = nrow(data_iga_pos[`graft loss date` < `T-date` + follow_up]),
  iga_neg = nrow(data_iga_neg[`graft loss date` < `T-date` + follow_up])
)
################################################################################
## age distribution (in years)
## iga all
ggplot(data = data_iga) +
  geom_histogram(mapping = aes(x = interval(`Date of birth`,`T-date`) / years(1),
                               fill = `R-sex`),
                 color="#e9ecef",
                 alpha=0.6,
                 bins = 40,
                 position = "dodge") +
  two_scale_fill +
  ylab("Anzahl") +
  xlab("Alter in Jahren") +
  ggtitle("Patientenalter zum Zeitpunk der Operation",subtitle =  "(IGA all)") +
  labs(fill = "Geschlecht") +
  default_theme
save.plot("iga_histogram_age_all.jpg")

## iga positive
ggplot(data = data_iga_pos) +
  geom_histogram(mapping = aes(x = interval(`Date of birth`,`T-date`) / years(1),
                               fill = `R-sex`),
                 color="#e9ecef",
                 alpha=0.6,
                 bins = 40,
                 position = "dodge") +
  two_scale_fill +
  ylab("Anzahl") +
  xlab("Alter in Jahren") +
  ggtitle("Histogram Alter in Jahren am Transplantationsdatum", subtitle =  "IGA+") +
  labs(fill = "Geschlecht") +
  default_theme
save.plot("iga_histogram_age_positive.jpg")

## iga negative
ggplot(data = data_iga_neg) +
  geom_histogram(mapping = aes(x = interval(`Date of birth`,`T-date`) / years(1),
                               fill = `R-sex`),
                 color="#e9ecef",
                 alpha=0.6,
                 bins = 40,
                 position = "dodge") +
  two_scale_fill +
  ylab("Anzahl") +
  xlab("Alter in Jahren") +
  ggtitle("Histogram Alter in Jahren am Transplantationsdatum",
          subtitle =  "IGA-") +
  labs(fill = "Geschlecht") +
  default_theme
save.plot("iga_histogram_age_negative.jpg")

################################################################################
## IQR age distribution
tbl_iga_iqr_age <- data.frame(
  iga_all = IQR(data_iga$R_age_Tdate),
  iga_po = IQR(data_iga_pos$R_age_Tdate),
  iga_neg = IQR(data_iga_neg$R_age_Tdate)
)

p1 <- gg_boxplot(data_iga, "R_age_Tdate")
p2 <- gg_boxplot(data_iga_pos, "R_age_Tdate")
p3 <- gg_boxplot(data_iga_neg, "R_age_Tdate")


p1$labels$x <- p2$labels$x <- p3$labels$x <- ""
p1$labels$y<- p2$labels$y <- p3$labels$y <- ""

patch <- p1 | p2 | p3
patch <- patch + plot_annotation(
  title = "Altersverteilung zum Zeitpunkt der Operation"
)
patch & ylim(15, 75) & theme(axis.ticks.x = element_blank(),
                            axis.text.x = element_blank()) & ylab("Alter")
save.plot("iga_boxplot_age.jpg")
################################################################################
## sex
tbl_iga_sex_abs <- data.frame(
  iga_all = summary(data_iga$`R-sex`),
  iga_pos = summary(data_iga_pos$`R-sex`),
  iga_neg = summary(data_iga_neg$`R-sex`)
)
p1 <- data.frame(group = rownames(tbl_iga_sex_abs),
                 value = tbl_iga_sex_abs$iga_all) %>% 
  ggplot(aes(x = "", y = value, fill = group)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  guides(fill=guide_legend(title = "Geschlecht")) +
  theme_void() +
  two_scale_fill +
  ggtitle("iga all")

p2 <- data.frame(group = rownames(tbl_iga_sex_abs),
                 value = tbl_iga_sex_abs$iga_pos) %>% 
  ggplot(aes(x = "", y = value, fill = group)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  guides(fill=guide_legend(title = "Geschlecht")) +
  theme_void() +
  two_scale_fill +
  ggtitle("iga +")

p3 <- data.frame(group = rownames(tbl_iga_sex_abs),
                 value = tbl_iga_sex_abs$iga_neg) %>% 
  ggplot(aes(x = "", y = value, fill = group)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  guides(fill=guide_legend(title = "Geschlecht")) +
  theme_void() +
  two_scale_fill +
  ggtitle("iga -")

patch <- p1 | p2 | p3
patch + plot_annotation(title = "Kreisdiagramme Geschlecht (absolut)")
save.plot("iga_piechart_sex.jpg")
# sex percent
tbl_iga_sex_percent <- data.frame(
  iga_all = round(summary(data_iga$`R-sex`) / nrow(data_iga), 2),
  iga_pos = round(summary(data_iga_pos$`R-sex`) / nrow(data_iga_pos), 2),
  iga_neg = round(summary(data_iga_neg$`R-sex`) / nrow(data_iga_neg), 2)
)
################################################################################
tbl_iga_1 <- summary(data_iga[`graft loss (0=functial, 1=loss)` == 1]$`D-type`)
tbl_iga_1 <- round(tbl_iga_1 / sum(tbl_iga_1), 3)
tbl_iga_2 <- summary(data_iga[`graft loss (0=functial, 1=loss)` == 0]$`D-type`)
tbl_iga_2 <- round(tbl_iga_2 / sum(tbl_iga_2),3)
rbind("loss" = tbl_iga_1, "functional" = tbl_iga_2) %>% 
  as.data.frame()
################################################################################
# dead/alive
tbl_iga_1 <- summary(data_iga[`Pat death (0=alive, 1= dead)` == 1]$`D-type`)
tbl_iga_1 <- round(tbl_iga_1 / sum(tbl_iga_1), 3)
tbl_iga_2<- summary(data_iga[`Pat death (0=alive, 1= dead)` == 0]$`D-type`)
tbl_iga_2 <-  round(tbl_iga_2 / sum(tbl_iga_2),3)  # in %
rbind("dead" = tbl_iga_1, "alive" = tbl_iga_2) %>% 
  as.data.frame()
################################################################################
# BMI (mean.)
tbl_iga_bmi <- data.table(
  iga_all = mean(data_iga$`D-weight` * (data_iga$`D-height`)^2),
  iga_pos = mean(data_iga_pos$`D-weight` * (data_iga_pos$`D-height`)^2),
  iga_neg = mean(data_iga_neg$`D-weight` * (data_iga_neg$`D-height`)^2)
)
################################################################################
# HLA mismatch
p1 <- gg_boxplot(data_iga, "mismatch_sum", title = "IGA all")
p2 <- gg_boxplot(data_iga_neg, column = "mismatch_sum", title = "IGA-")
p3 <- gg_boxplot(data_iga_pos, column = "mismatch_sum", title = "IGA+")

patch <- p1 | p2 | p3
patch + plot_annotation(title = "HLA mismatch summe") & ylim(0,6)
save.plot("iga_boxplot_hla.jpg")
# HLA-mm (0-6)
## data.table of mm-A, mm-B and mm-DR
tbl_iga_1 <- as.data.table(lapply(data_iga[, c("mm-A", "mm-B", "mm-DR")],
                                  as.numeric))
## calculate mean for each column
tbl_iga_2 <- apply(X = tbl_iga_1,
                   MARGIN = 2,
                   FUN = mean, na.rm = TRUE)
round(tbl_iga_2, 3)
# mean of means
round(mean(tbl_iga_2),3)

## sum of mm-A, mm-B and mm-DR
tbl_iga_3 <- apply(X = tbl_iga_1,
                   MARGIN = 1,  
                   FUN = sum, na.rm = TRUE)
# mean value of col sum
mean(tbl_iga_3)
# median value of col sum
sd(tbl_iga_3)
################################################################################
# PRA
################################################################################
# PRA current (mean)
tbl_iga_pra_curr_mean <- data.frame(
  iga_all = mean(data_iga$`Current PRA%`, na.rm = TRUE),
  iga_pos = mean(data_iga_pos$`Current PRA%`, na.rm = TRUE),
  iga_neg = mean(data_iga_neg$`Current PRA%`, na.rm = TRUE)
)
# PRA highest (mean)
tbl_iga_pra_high <- data.frame(
  iga_all = mean(data_iga$`Highest PRA%`, na.rm = TRUE),
  iga_pos = mean(data_iga_pos$`Highest PRA%`, na.rm = TRUE),
  iga_neg = mean(data_iga_neg$`Highest PRA%`, na.rm = TRUE)
)
################################################################################
# cold-ischemia time (hours)
# mean
tbl_iga_cis_mean <- data.frame(
  iga_all = mean(data_iga$`Cold ischaemic period hours`, na.rm = TRUE),
  iga_pos = mean(data_iga_pos$`Cold ischaemic period hours`, na.rm = TRUE),
  iga_neg = mean(data_iga_neg$`Cold ischaemic period hours`, na.rm = TRUE)
)
# median
tbl_iga_cis_median <- data.frame(
  iga_all = median(data_iga$`Cold ischaemic period hours`, na.rm = TRUE),
  iga_pos = median(data_iga_pos$`Cold ischaemic period hours`, na.rm = TRUE),
  iga_neg = median(data_iga_neg$`Cold ischaemic period hours`, na.rm = TRUE)
)
# standard error
tbl_iga_cis_sd <- data.frame(
  iga_all = sd(data_iga$`Cold ischaemic period hours`, na.rm = TRUE),
  iga_pos = sd(data_iga_pos$`Cold ischaemic period hours`, na.rm = TRUE),
  iga_neg = sd(data_iga_neg$`Cold ischaemic period hours`, na.rm = TRUE)
)

p1 <- gg_boxplot(data_iga, "Cold ischaemic period hours", title = "IGA all")
p2 <- gg_boxplot(data_iga_pos, "Cold ischaemic period hours", title = "IGA+")
p3 <- gg_boxplot(data_iga_neg, "Cold ischaemic period hours", title = "IGA-")

p1$labels$x <- p2$labels$x <- p3$labels$x <- ""
p1$labels$y<- p2$labels$y <- p3$labels$y <- ""

patch <- p1 | p2 | p3
patch <- patch + plot_annotation(
  title = "Boxplot Cold Isch. Time h"
)
patch & ylim(0, 40) & theme(axis.ticks.y = element_blank(),
                            axis.text.y = element_blank()) & ylab("Cold Isch Time")
save.plot("iga_cold_ime.jpg")
################################################################################