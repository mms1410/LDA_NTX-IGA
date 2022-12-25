################################################################################
# descriptive statistics plots
################################################################################
# Time Date Last seen Boxplot
## IGA
gg.boxplot(data = data_iga[, .(`biopsy proven recurrence (0=no, 1=yes)`,time_dls = interval(`T-date`, `T-dls`) / years(1))],
           y.column = "time_dls", x.column = "biopsy proven recurrence (0=no, 1=yes)",
           ylab = "Zeitspanne in Jahren", x.ticks = c("0" = "Ohne", "1" = "Mit"),
           title = "IGA: Zeitspanne zwischen Operation und Datum zuletzt gesehen")
save.plot("boxplot_tdls_iga.jpg")
## NTX
ggplot() +
  geom_boxplot(aes(x = 1,
                   y = interval(data_ntx$Datum, data_ntx$tdls) / years(1)),
               data = data_ntx) +
  ggtitle("NTX: Zeitspanne zwischen Operation und Datum zuletzt gesehen") +
  ylab("Jahre") +
  xlab("") + 
  default_theme +
  theme(axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank())
gg.boxplot(data = data_ntx[, .(time_dls = interval(data_ntx$Datum, data_ntx$tdls) / years(1))],
           y.column = "time_dls", title = "NTX: Zeitspanne zwischen Operation und Datum zuletzt gesehen",
           ylab = "Zeitspanne in Jahren")
save.plot("boxplot_tdls_ntx.jpg")
## all
p1 <- gg.boxplot(data = data_ntx[, .(time_dls =interval(Datum, tdls) / years(1))],
                 y.column = "time_dls", xlab = "NTX", ylab = "Zeitspanne in Jahren", ylims = c(0, 15))
p2 <- gg.boxplot(data = data_iga_pos[, .(time_dls = interval(`T-date`, `T-dls`) / years(1))],
                 y.column = "time_dls", xlab = "IGA+", ylims = c(0, 15))
p3 <- gg.boxplot(data = data_iga_neg[, .(time_dls = interval(`T-date`, `T-dls`) / years(1))],
                 y.column = "time_dls", xlab = "IGA-", ylims = c(0, 15))

patch <- p1 | p2 | p3
patch <- patch + plot_annotation(
  title = "Zeitspanne zwischen Operation und Datum zuletzt gesehen "
)
patch
save.plot("boxplot_tdls_all.jpg")
# Age Boxplot
## IGA
gg.boxplot(data = data_iga, y.column = "R_age_Tdate", x.column = "biopsy proven recurrence (0=no, 1=yes)",
           x.ticks = c("0" = "ohne", "1" = "mit"), ylab = "Alter", title = "IGA: Alter zum Zeitpunkt der OP")
save.plot("boxplot_age_iga.jpg")
## NTX
gg.boxplot(data = data_ntx, y.column ="R_age_Datum", ylab = "Alter", title = "NTX: Alter zum Zeitpunkt der OP")
save.plot("boxplot_age_ntx.jpg")
## all
p1 <- gg.boxplot(data = data_iga_pos, y.column = "R_age_Tdate",
                 xlab = "IGA+", ylims = c(0, 85), ylab = "Alter")
p2 <- gg.boxplot(data = data_iga_neg, y.column = "R_age_Tdate",
                 xlab = "IGA-", ylims = c(0, 85))
p3 <- gg.boxplot(data = data_ntx, y.column ="R_age_Datum", xlab = "NTX", ylims = c(0, 85))

patch <- p1 | p2 | p3
patch <- patch + plot_annotation("Alter zum Zeitpunk der OP")
patch
save.plot("boxplot_age_all.jpg")
# Age Density by sex
## IGA
gg.density(data = data_iga, density.column = "R_age_Tdls" , fill.column = "R-sex", ylab = "Dichte",
           legend.title = "Geschlecht", legend.names = c("F" = "Weiblich", "M" = "Männlich"),
           title = "IGA all: Dichteschätzung Alter nach Geschlecht", xlab = "Alter")
save.plot("density_age_sex_iga.jpg")
gg.density(data = data_iga_pos, density.column = "R_age_Tdls" , fill.column = "R-sex", ylab = "Dichte",
           xlab = "Alter", legend.title = "Geschlecht", legend.names = c("F" = "Weiblich", "M" = "Männlich"),
           title = "IGA pos: Dichteschätzung Alter")
save.plot("density_age_sex_iga_pos.jpg")
gg.density(data = data_iga_neg, density.column = "R_age_Tdls" , fill.column = "R-sex", ylab = "Dichte",
           xlab = "Alter", legend.title = "Geschlecht",
           legend.names = c("F" = "Weiblich", "M" = "Männlich"), title = "IGA neg: Dichteschätzung Alter nach Geschlecht")
save.plot("density_age_sex_iga_neg.jpg")
gg.density(data = data_ntx, density.column = "R_age_Datum", fill.column = "Geschlecht",
           ylab = "Dichte", xlab = "Alter",
           legend.title = "Geschlecht", legend.names = c("weiblich" = "Weiblich", "männlich" = "Männlich"),
           title = "NTX: Dichteschätzung Alter nach Geschlecht")
save.plot("density_age.ntx.jpg")
# Cold Ischaemic period
## IGA all
gg.boxplot(data = data_iga[cold_time_sum_min > 0], y.column = "cold_time_sum_min", x.column = "biopsy proven recurrence (0=no, 1=yes)",
           title = "IGA all: Kalte ischämiezeit", ylab = "Zeit in Minuten", x.ticks = c("0" = "Ohne", "1" = "Mit"))
save.plot("boxplot_cold_iga.jpg")
## IGA pos
gg.boxplot(data = data_iga_pos[cold_time_sum_min > 0], y.column = "cold_time_sum_min",
           title = "IGA pos: Kalte ischämiezeit", ylab = "Zeit in Minuten")
save.plot("boxplot_cold_iga_pos.jpg")
## IGA neg
gg.boxplot(data = data_iga_neg[cold_time_sum_min > 0], y.column = "cold_time_sum_min",
          title = "IGA neg: Kalte Ischämiezeit", ylab = "Zeit in Minuten")
save.plot("boxplot_cold_iga_neg.jpg")
# HLA mismatch
## IGA all
gg.boxplot(data_iga[!is.na(mismatch_sum)], y.column = "mismatch_sum", x.column = "biopsy proven recurrence (0=no, 1=yes)",
           title = "IGA all: HLA-mismatch (Summe)",
           ylab = "Summe zwischen 0 und 5", x.ticks = c("0" = "Ohne", "1" = "Mit"))
save.plot("boxplot_iga_hla.jpg")
## IGA neg sex
gg.boxplot(data_iga_neg[!is.na(mismatch_sum)], y.column = "mismatch_sum", x.column = "R-sex",
           x.ticks = c("M" = "Männlich", "F" = "Weiblich"), title = "IGA neg: HLA-mismatch (Summe)",
           ylab = "Summe zwischen 0 und 5")
save.plot("boxplot_iga_neg_hla_sex.jpg")
gg.boxplot(data_iga_pos[!is.na(mismatch_sum)], y.column = "mismatch_sum", x.column = "R-sex",
           x.ticks = c("M" = "Männlich", "F" = "Weiblich"), title = "IGA all: HLA-mismatch (Summe)",
           ylab = "Summe zwischen 0 und 5", ylims = c(0,5))
save.plot("boxplot_iga_pos_hla_sex.jpg")

## histogram all
gg.binhist(data = data_iga, bin.breaks = c(0, 2, 4, 6), colname = "mismatch_sum", 
           group.name = "biopsy proven recurrence (0=no, 1=yes)",
           levels.name = c("no rec.", "with rec.", "iga all"),
           legend.title = "Gruppe", include.all = TRUE, xlab = "", ylab = "Anzahl",
           title = "HLA mimsatch", lowest = TRUE, count.stat = TRUE)
save.plot("histogram_mismatch_sum_iga.jpg")

# time of biopsy
## IGA all
gg.boxplot(data = data_iga[], y.column = "time of biopsy (years after KTX)", x.column = "biopsy proven recurrence (0=no, 1=yes)",
           x.ticks = c("0" = "Ohne", "1" = "Mit"),
           ylab = "Jahre nach KTX", title = "IGA: Jahre nach KTX")
save.plot("boxplot_iga_yearsKTX.jpg")
# current PRA
#TODO: tick only 30
#      same widh
#      deutsch
gg.binhist(data = data_iga, bin.breaks = c(0, 30, 100), colname = "Current PRA%", 
           group.name = "biopsy proven recurrence (0=no, 1=yes)",
           levels.name = c("no rec.", "with rec.", "all iga"),
           legend.title = "Gruppe", include.all = TRUE, xlab = "", ylab = "Anzahl",
           title = " Aktueller PRA%-Wert zum Transplantationszeitpunk \n (<= 30 & > 30)", lowest = TRUE, count.stat = TRUE)
save.plot("histogram_current_pra_iga.jpg")

# highest PRA
gg.binhist(data = data_iga, bin.breaks = c(0, 30, 100), colname = "Highest PRA%", 
           group.name = "biopsy proven recurrence (0=no, 1=yes)",
           levels.name = c("no rec.", "with rec.", "all iga"),
           legend.title = "Gruppe", include.all = TRUE, xlab = "", ylab = "Anzahl",
           title = " Höchster PRA%-Wert\n (<= 30 & > 30)", lowest = TRUE, count.stat = TRUE)
save.plot("histogram_highest_pra_iga.jpg")
