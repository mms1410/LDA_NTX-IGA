############################
# descriptive statistics lda
############################

## create variable containing minimum of date last seen (T-dls) and end of follow-up period (T-date + follow_up)
data_iga$follow_up_truncated <-pmin(data_iga$`T-dls`, data_iga$`T-date` + follow_up)
## create variable containing row wise sum of mm-A, mm-B and mm-DR
data_iga$mismatch_sum <- as.numeric(as.character(data_iga$`mm-A`)) + as.numeric(as.character(data_iga$`mm-B`)) + as.numeric(as.character(data_iga$`mm-DR`))
## partition iga data into positive (with recurrence after biopsy) and negative (no recurrence after biopsy)
data_iga_pos <- data_iga[`biopsy proven recurrence (0=no, 1=yes)` == 1]
data_iga_neg <- data_iga[`biopsy proven recurrence (0=no, 1=yes)` == 0]
################################################################################
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

p1 <- ggplot(data = data_iga) +
  geom_boxplot(aes(x = R_age_Tdate)) +
  coord_flip() +
  ggtitle("IGA (all)") +
  default_theme 
p1 <- annotate_fivenumbers(data_iga, p1)

p2 <- ggplot(data = data_iga_pos) +
  geom_boxplot(aes(x = R_age_Tdate)) +
  coord_flip() +
  ggtitle("IGA r+") +
  default_theme
p2 <- annotate_fivenumbers(data_iga_pos, p2)

p3 <- ggplot(data = data_iga_neg) +
  geom_boxplot(aes(x = R_age_Tdate)) +
  coord_flip()+
  ggtitle("IGA r-") +
  default_theme
p3 <- annotate_fivenumbers(data_iga_neg, p3)


p1$labels$x <- p2$labels$x <- p3$labels$x <- ""
p1$labels$y<- p2$labels$y <- p3$labels$y <- ""

patch <- p1 | p2 | p3
patch <- patch + plot_annotation(
  title = "Altersverteilung zum Zeitpunkt der Operation"
)
patch & xlim(15, 80) & theme(axis.ticks.x = element_blank(),
                            axis.text.x = element_blank()) & xlab("Alter")
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
# donator
################################################################################
# death donator
tbl_death_d <- cbind(c(nrow(data_iga[`Pat death (0=alive, 1= dead)` == 0]),
                       nrow(data_iga[`Pat death (0=alive, 1= dead)` == 1])),
                     c(nrow(data_iga_pos[`Pat death (0=alive, 1= dead)` == 0]),
                       nrow(data_iga_pos[`Pat death (0=alive, 1= dead)` == 1])),
                     c(nrow(data_iga_neg[`Pat death (0=alive, 1= dead)` == 0]),
                       nrow(data_iga_neg[`Pat death (0=alive, 1= dead)` == 1])))
colnames(tbl_death_d) <- c("iga_all", "iga_pos", "iga_neg")
rownames(tbl_death_d) <- c("Lebendspende", "Todspende")

p1 <- data.frame(group = c("Lebendspende", "Todspende"),
                 value = c(nrow(data_iga[`Pat death (0=alive, 1= dead)` == 0]),
                           nrow(data_iga[`Pat death (0=alive, 1= dead)` == 1]))) %>% 
  ggplot(aes(x = "", y = value, fill = group)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  guides(fill=guide_legend(title = "Spender")) +
  theme_void() +
  two_scale_fill +
  ggtitle("iga all")

p2 <- data.frame(group = c("Lebendspende", "Todspende"),
                 value = c(nrow(data_iga_pos[`Pat death (0=alive, 1= dead)` == 0]),
                           nrow(data_iga_pos[`Pat death (0=alive, 1= dead)` == 1]))) %>% 
  ggplot(aes(x = "", y = value, fill = group)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  guides(fill=guide_legend(title = "Spender")) +
  theme_void() +
  two_scale_fill +
  ggtitle("iga +")

p3 <- data.frame(group = c("Lebendspende", "Todspende"),
                 value = c(nrow(data_iga_neg[`Pat death (0=alive, 1= dead)` == 0]),
                           nrow(data_iga_neg[`Pat death (0=alive, 1= dead)` == 1]))) %>% 
  ggplot(aes(x = "", y = value, fill = group)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  guides(fill=guide_legend(title = "Spender")) +
  theme_void() +
  two_scale_fill +
  ggtitle("iga -")


patch <- p1 | p2 | p3
patch + plot_annotation(title = "Kreisdiagramme Spender")
save.plot("iga_piechart_sex_donator.jpg")
################################################################################
# age donator
tbl_iga_age_donor <- data.frame(
  iga_all = mean(data_iga$`D-age`),
  iga_pos =  mean(data_iga_pos$`D-age`),
  iga_neg =  mean(data_iga_neg$`D-age`)
)
# living vs dead donator
tbl_iga_don_living_abs <- data.frame(
  iga_all = nrow(data_iga[`D-type` == "Living"]),
  iga_pos = nrow(data_iga_pos[`D-type` == "Living"]),
  iga_neg = nrow(data_iga_neg[`D-type` == "Living"])
)

tbl_iga_don_dead_abs <- data.frame(
  iga_all = nrow(data_iga[`D-type` == "Cadaver"]),
  iga_pos = nrow(data_iga_pos[`D-type` == "Cadaver"]),
  iga_neg = nrow(data_iga_neg[`D-type` == "Cadaver"])
)


tbl_iga_don_living_rel <- data.frame(
  iga_all = nrow(data_iga[`D-type` == "Living"]) / nrow(data_iga),
  iga_pos = nrow(data_iga_pos[`D-type` == "Living"]) / nrow(data_iga_pos),
  iga_neg = nrow(data_iga_neg[`D-type` == "Living"]) / nrow(data_iga_neg)
)

tbl_iga_don_dead_rel <- data.frame(
  iga_all = nrow(data_iga[`D-type` == "Cadaver"]) / nrow(data_iga),
  iga_pos = nrow(data_iga_pos[`D-type` == "Cadaver"]) / nrow(data_iga_pos),
  iga_neg = nrow(data_iga_neg[`D-type` == "Cadaver"]) / nrow(data_iga_neg)
)
################################################################################
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
p1 <- data_iga %>% 
  ggplot() +
  geom_boxplot(aes(x = "", y = mismatch_sum )) +
  default_theme +
  ggtitle("IGA all") +
  ylab("") +
  xlab("")
p2 <- data_iga_neg %>% 
  ggplot() +
  geom_boxplot(aes(x = "", y = mismatch_sum)) +
  default_theme +
  ggtitle("IGA-") +
  ylab("") +
  xlab("")
p3 <- data_iga_pos %>% 
  ggplot() +
  geom_boxplot(aes(x = "", y = mismatch_sum)) +
  default_theme +
  ggtitle("IGA+") + 
  ylab("") +
  xlab("")

patch <- p1 | p2 | p3
patch + plot_annotation(title = "HLA mismatch summe")
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

p1 <- data_iga %>% 
  ggplot() +
  geom_boxplot(aes(x = `Cold ischaemic period hours`)) +
  coord_flip() +
  ggtitle("IGA (all)") +
  default_theme 

p2 <- data_iga_pos %>% 
  ggplot() +
  geom_boxplot(aes(x = `Cold ischaemic period hours`)) +
  coord_flip() +
  ggtitle("IGA +") +
  default_theme 

p3 <- data_iga_neg %>% 
  ggplot() +
  geom_boxplot(aes(x = `Cold ischaemic period hours`)) +
  coord_flip() +
  ggtitle("IGA -") +
  default_theme 

p1$labels$x <- p2$labels$x <- p3$labels$x <- ""
p1$labels$y<- p2$labels$y <- p3$labels$y <- ""

patch <- p1 | p2 | p3
patch <- patch + plot_annotation(
  title = "Boxplot Cold Isch. Time h"
)
patch & xlim(0, 40) & theme(axis.ticks.x = element_blank(),
                            axis.text.x = element_blank()) & xlab("Cold Isch Time")
save.plot("iga_cold_ime.jpg")
