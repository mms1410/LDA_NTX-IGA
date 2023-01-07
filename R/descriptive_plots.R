###=============================================================================
# descriptive statistics plots
###=============================================================================
# Time Date Last seen Boxplot
## IgA
data.iga[, .(`biopsy_proven_recurrence(0=no,1=yes)`,years_within_follow_up)] %>% 
  gg.boxplot(y.column="years_within_follow_up", x.column="biopsy_proven_recurrence(0=no,1=yes)",
             ylab="Zeitspanne in Jahren", x.ticks=c("0"="Ohne Rekurrenz", "1"="Mit Rekurrenz"),
             title="IGA: Zeitspanne zwischen Operation und Datum zuletzt gesehen")
save.plot("boxplot_tdls_iga.jpg")
## NTX
gg.boxplot(data = data.ntx, y.column = "years_within_follow_up", title = "NTX: Zeitspanne zwischen Operation und Datum zuletzt gesehen",
           ylab = "Zeitspanne in Jahren")
save.plot("boxplot_tdls_ntx.jpg")
## all
p1.1 <- gg.boxplot(data = data.ntx, y.column = "years_within_follow_up",
                 xlab = "NTX", ylab = "Zeitspanne in Jahren", ylims=c(0, 15))
p1.2 <- gg.boxplot(data = data.iga.pos,
                 y.column = "years_within_follow_up", xlab = "IgA+", ylims=c(0, 15))
p1.3 <- gg.boxplot(data = data.iga.neg,
                 y.column = "years_within_follow_up", xlab = "IgA-", ylims=c(0, 15))
p1.4 <- gg.boxplot(data = data.iga,
                 y.column="years_within_follow_up", xlab="IgA-all", ylims=c(0, 15))
patch1 <- p1.1 | p1.4 |p1.2 | p1.3
patch1 <- patch1 + plot_annotation(
  title = "Zeitspanne zwischen Operation und Datum zuletzt gesehen "
)
patch1
save.plot("boxplot_tdls_all.jpg")
# Time Date Last seen Boxplot (alive)
## IgA
data.iga[`Pat_death(0=alive,1=dead)` == 0, .(`biopsy_proven_recurrence(0=no,1=yes)`,years_within_follow_up)] %>% 
  gg.boxplot(y.column="years_within_follow_up", x.column="biopsy_proven_recurrence(0=no,1=yes)",
             ylab="Zeitspanne in Jahren", x.ticks=c("0"="Ohne Rekurrenz", "1"="Mit Rekurrenz"),
             title="IGA: Zeitspanne zwischen Operation und Datum zuletzt gesehen\n(Überlebende)")
save.plot("boxplot_tdls_alive_iga.jpg")
## NTX
gg.boxplot(data = data.ntx[`Patienten_Status[NTXPatientenInformation]` == "1_lebt"],
           y.column = "years_within_follow_up",
           title = "NTX: Zeitspanne zwischen Operation und Datum zuletzt gesehen\n(Überlebende)",
           ylab = "Zeitspanne in Jahren")
save.plot("boxplot_tdls_alive_ntx.jpg")
## all
p2.1 <- gg.boxplot(data = data.ntx[`Patienten_Status[NTXPatientenInformation]` == "1_lebt"],
                 y.column = "years_within_follow_up",
                 xlab = "NTX", ylab = "Zeitspanne in Jahren", ylims=c(0, 15))
p2.2 <- gg.boxplot(data = data.iga.pos[`Pat_death(0=alive,1=dead)` == 0],
                 y.column = "years_within_follow_up", xlab = "IgA+", ylims=c(0, 15))
p2.3 <- gg.boxplot(data = data.iga.neg[`Pat_death(0=alive,1=dead)` == 0],
                 y.column = "years_within_follow_up", xlab = "IgA-", ylims=c(0, 15))
p2.4 <- gg.boxplot(data = data.iga[`Pat_death(0=alive,1=dead)` == 0],
                 y.column="years_within_follow_up", xlab="IgA-all", ylims=c(0, 15))

patch2 <- p2.1 | p2.4 |p2.2 | p2.3
patch2 <- patch2 + plot_annotation(
  title = "Zeitspanne zwischen Operation und Datum zuletzt gesehen\n(Überlebende) "
)
patch2
save.plot("boxplot_tdls_alive_all.jpg")
# Time Date Last seen Boxplot (dead)
## IgA
data.iga[`Pat_death(0=alive,1=dead)` == 1, .(`biopsy_proven_recurrence(0=no,1=yes)`,years_within_follow_up)] %>% 
  gg.boxplot(y.column="years_within_follow_up", x.column="biopsy_proven_recurrence(0=no,1=yes)",
             ylab="Zeitspanne in Jahren", x.ticks=c("0"="Ohne Rekurrenz", "1"="Mit Rekurrenz"),
             title="IGA: Zeitspanne zwischen Operation und Datum zuletzt gesehen\n(Gestorbene)")
save.plot("boxplot_tdls_alive_iga.jpg")
## NTX
gg.boxplot(data = data.ntx[`Patienten_Status[NTXPatientenInformation]` == "2_verstorben"],
           y.column = "years_within_follow_up",
           title = "NTX: Zeitspanne zwischen Operation und Datum zuletzt gesehen\n(Verstorbene)",
           ylab = "Zeitspanne in Jahren")
save.plot("boxplot_tdls_alive_ntx.jpg")
## all
p3.1 <- gg.boxplot(data = data.ntx[`Patienten_Status[NTXPatientenInformation]` == "2_verstorben"],
                   y.column = "years_within_follow_up",
                   xlab = "NTX", ylab = "Zeitspanne in Jahren", ylims=c(0, 15))
p3.2 <- gg.boxplot(data = data.iga.pos[`Pat_death(0=alive,1=dead)` == 1],
                   y.column = "years_within_follow_up", xlab = "IgA+", ylims=c(0, 15))
p3.3 <- gg.boxplot(data = data.iga.neg[`Pat_death(0=alive,1=dead)` == 1],
                   y.column = "years_within_follow_up", xlab = "IgA-", ylims=c(0, 15))
p3.4 <- gg.boxplot(data = data.iga[`Pat_death(0=alive,1=dead)` == 1],
                   y.column="years_within_follow_up", xlab="IgA-all", ylims=c(0, 15))

patch3 <- p3.1 | p3.4 |p3.2 | p3.3
patch3 <- patch3 + plot_annotation(
  title = "Zeitspanne zwischen Operation und Datum zuletzt gesehen\n(Verstorbene) "
)
patch3
save.plot("boxplot_tdls_dead_all.jpg")
##
(p1.1 + ggtitle("Alle") | p1.4 | p1.2 | p1.3)/
  (p2.1 + ggtitle("Überlebene")| p2.4 | p2.2 | p2.3) /
  (p3.1 + ggtitle("Verstorbende")| p3.4 | p3.2 | p3.3) + plot_annotation("Zeitspanne zwischen Operation und Datum zuletzt gesehen")
save.plot("boxplot_tdls_all_groups.jpg")
###=============================================================================
# age_surgery Boxplot
## IGA
gg.boxplot(data = data.iga,
           y.column = "R_age_surgery",
           x.column = "biopsy_proven_recurrence(0=no,1=yes)",
           x.ticks = c("0" = "r+IgA", "1" = "r-IgA"),
           ylab = "Alter",
           title = "Alter zum Zeitpunkt der OP\n(IgA)")
save.plot("boxplot_age_iga.jpg")
## NTX
gg.boxplot(data = data.ntx,
           y.column ="R_age_surgery",
           ylab = "Alter",
           title = "Alter zum Zeitpunkt der OP\n(NTX)")
save.plot("boxplot_age_ntx.jpg")
## all
p1 <- gg.boxplot(data = data.iga.pos,
                 y.column = "R_age_surgery",
                 xlab = "r+IgA", ylims = c(0, 85), ylab = "Alter")
p2 <- gg.boxplot(data = data.iga.neg, y.column = "R_age_surgery",
                 xlab = "r-IgA", ylims = c(0, 85))
p3 <- gg.boxplot(data = data.ntx, y.column ="R_age_surgery", xlab = "NTX", ylims = c(0, 85))

patch <- p1 | p2 | p3
patch <- patch + plot_annotation("Alter zum Zeitpunk der OP")
patch
save.plot("boxplot_age_all.jpg")
# Age Density by sex
## IGA
gg.density(data = data.iga, density.column = "R_age_surgery",
           fill.column = "Geschlecht", ylab = "Dichte",
           legend.title = "Geschlecht", legend.names = c("W" = "W", "M" = "M"),
           title = "Kerndichteschätzung Alter zum Operationszeitpunkt\n(all-IgA)", xlab = "Alter")
save.plot("density_age_sex_iga.jpg")

gg.density(data = data.iga.pos, density.column = "R_age_surgery",
           fill.column = "Geschlecht", ylab = "Dichte",
           xlab = "Alter", legend.title = "Geschlecht", legend.names = c("W" = "W", "M" = "M"),
           title = "Kerndichteschätzung Alter zum Operationszeitpunkt\n(r+IgA)")
save.plot("density_age_sex_iga_pos.jpg")

gg.density(data = data.iga.neg, density.column = "R_age_surgery",
           fill.column = "Geschlecht",
           ylab = "Dichte", xlab = "Alter", legend.title = "Geschlecht",
           legend.names = c("W" = "W", "M" = "M"),
           title = "Kerndichteschätzung Alter zum Operationszeitpunkt\n(r-IgA)")
save.plot("density_age_sex_iga_neg.jpg")

gg.density(data = data.ntx, density.column = "R_age_surgery",
           fill.column = "Geschlecht",
           ylab = "Dichte", xlab = "Alter",
           legend.title = "Geschlecht", legend.names = c("W" = "W", "M" = "M"),
           title = "Kerndichteschätzung Alter zum Operationszeitpunkt\n(NTX)")
save.plot("density_age.ntx.jpg")
###=============================================================================
# Cold Ischaemic period
## IgA all
p1 <- gg.boxplot(data = data.iga[cold_time_minutes > 0],
                 y.column = "cold_time_minutes",
                 title = "Kalte Ischämiezeit\n(IgA-all)",
                 ylab = "Zeit in Minuten")
p1
save.plot("boxplot_cold_iga.jpg")
## IgA pos
p2 <- gg.boxplot(data = data.iga.pos[cold_time_minutes > 0], y.column = "cold_time_minutes",
                 title = "Kalte Ischämiezeit\n(IgA+)", ylab = "Zeit in Minuten")
p2
save.plot("boxplot_cold_iga_pos.jpg")
## IgA neg
p3 <- gg.boxplot(data = data.iga.neg[cold_time_minutes > 0],
                 y.column = "cold_time_minutes",
                 title = "Kalte Ischämiezeit\n(IgA-)", ylab = "Zeit in Minuten")
p3
save.plot("boxplot_cold_iga_neg.jpg")
## NTX
p4 <- gg.boxplot(data = data.ntx[cold_time_minutes > 0],
                 y.column = "cold_time_minutes",
                 title = "Kalte Ischämiezeit\n(NTX)",
                 ylab = "Zeit in Minuten")
p4
save.plot("boxplot_cold_ntx.jpg")

patch <- 
  p4 + ggtitle("") + labs(caption="NTX-all") + ylab("Zeit in Minuten") + ylim(c(0,1700)) |
  p1 + ggtitle("") + labs(caption="IgA-all") + ylab("") + ylim(c(0,1700)) |
  p2 + ggtitle("") + labs(caption="IgA+") + ylab("") + ylim(c(0,1700)) |
  p3 + ggtitle("") + labs(caption="IgA-") + ylab("") + ylim(c(0,1700))
patch + plot_annotation("Kalte Ischämiezeit")
save.plot("boxplot_cold_all.jpg")
###=============================================================================
# HLA mismatch
## IGA all
p1 <- gg.boxplot(data.iga[!is.na(mismatch_sum)],
                 y.column = "mismatch_sum",
                 title = "HLA-mismatch (Summe)\n(IgA-all)",
                 ylab = "Summe\n[0-5]")
p1
save.plot("boxplot_iga_hla.jpg")
## IGA neg
p2 <- gg.boxplot(data.iga.neg[!is.na(mismatch_sum)],
                 y.column = "mismatch_sum",
                 title = "HLA-mismatch (Summe)\n(IgA-)",
                 ylab = "Summe\n[0-5]")
p2
save.plot("boxplot_iga_neg_hla.jpg")
# IgA pos
p3 <- gg.boxplot(data.iga.pos[!is.na(mismatch_sum)],
                 y.column = "mismatch_sum",
                 title = "HLA-mismatch (Summe)\n(IgA+)",
                 ylab = "Summe\n[0-5]")
p3
save.plot("boxplot_iga_pos_hla.jpg")
p4 <- gg.boxplot(data.ntx,
                 y.column = "mismatch_sum",
                 title = "HLA-mismatch (Summe)\n(NTX)",
                 ylab = "Summe\n[0-6]")
p4
save.plot("boxplot_ntx_hla.jpg")


p4 + ylim(c(0,6)) + ylab("Summe\n[0-6]") + ggtitle("") + labs(caption="all-NTX") |
  p1 + ylim(c(0,6)) + ylab("") + ggtitle("") + labs(caption="all-IgA")|
  p2 + ylim(c(0,6)) + ylab("") + ggtitle("") + labs(caption="r-IgA") |
  p3 + ylim(c(0,6)) + ylab("") + ggtitle("") + labs(caption="r+IgA") +
  plot_annotation("HLA mismatch")
save.plot("boxplot_all_hla.jpg")

## histogram all
gg.binhist(data = data.iga, bin.breaks = c(0, 2, 4, 6), colname = "mismatch_sum", 
           group.name = "biopsy_proven_recurrence(0=no,1=yes)",
           levels.name = c("r-IgA", "r+IgA", "all-IgA"),
           legend.title = "Gruppe", include.all = TRUE, xlab = "", ylab = "Anzahl",
           title = "HLA missatch", lowest = TRUE, count.stat = TRUE)
save.plot("histogram_mismatch_sum_iga.jpg")
###=============================================================================
# time of biopsy
## IGA all
gg.boxplot(data = data.iga, y.column = "time_of_biopsy_(years_after_KTX)",
           x.column = "biopsy_proven_recurrence(0=no,1=yes)",
           x.ticks = c("0" = "Ohne Rekurrez", "1" = "Mit Rekurrez"),
           ylab = "Jahre",
           title = "Jahre nach KTX\n(all-IgA)")
save.plot("boxplot_iga_yearsKTX.jpg")
###=============================================================================
# current PRA
gg.binhist(data = data.iga, bin.breaks = c(0, 30, 100),
           colname = "current_PRA%", 
           group.name = "biopsy_proven_recurrence(0=no,1=yes)",
           levels.name =  c("r-IgA", "r+IgA", "all-IgA"),
           legend.title = "Gruppe",
           include.all = TRUE,
           xlab = "", ylab = "Anzahl",
           title = " Aktueller PRA%-Wert zum Transplantationszeitpunk\n(IgA)",
           lowest = TRUE, count.stat = TRUE)
save.plot("histogram_current_pra_iga.jpg")
# highest PRA
gg.binhist(data = data.iga, bin.breaks = c(0, 30, 100),
           colname = "Highest_PRA%", 
           group.name = "biopsy_proven_recurrence(0=no,1=yes)",
           levels.name = c("r-IgA", "r+IgA", "all-IgA"),
           legend.title = "Gruppe",
           include.all = TRUE,
           xlab = "", ylab = "Anzahl",
           title = " Höchster PRA%-Wert",
           lowest = TRUE, count.stat = TRUE)
save.plot("histogram_highest_pra_iga.jpg")
###=============================================================================
## donator type
rbindlist(list(
  data.iga[, .(group = "all-IgA", Spendertyp = D_type)],
  data.iga.pos[, .(group = "r+IgA", Spendertyp = D_type)],
  data.iga.neg[, .(group = "r-IgA", Spendertyp = D_type)]
)) %>%
  ggplot() +
  geom_histogram(aes(fill = Spendertyp, x = group), stat="count", position = "dodge") +
  ylab("Anzahl") +
  xlab("") +
  ggtitle("Organspendetyp") +
  scale_fill_discrete(labels=c("Totspende", "Lebendspende")) +
  default_theme
###=============================================================================
## KREA 1Y
p1 <- gg.boxplot(data.iga,
           y.column = "Krea_1Y",
           title = "Krea_1Y\n(all-IgA)",
           ylab = "Kreatininwerte") + ylim(c(0,13))
p1
save.plot("boxplot_krea_1_iga.jpg")

p2 <- gg.boxplot(data.iga.pos,
           y.column = "Krea_1Y",
           title = "Krea_1Y\n(r+IgA)",
           ylab = "Kreatininwerte") + ylim(c(0,13))
p2
save.plot("boxplot_krea_1_iga_pos.jpg")

p3 <-gg.boxplot(data.iga.neg,
           y.column = "Krea_1Y",
           title = "Krea_1Y\n(r-IgA)",
           ylab = "Kreatininwerte") + ylim(c(0,13))
p3
save.plot("boxplot_krea_1_iga_neg.jpg")

p4 <- gg.boxplot(data.ntx,
                 y.column = "Krea_1Y",
                 title = "Krea_1Y\n(all-NTX)",
                 ylab = "Kreatininwerte") + ylim(c(0,13))
p4
save.plot("boxplot_krea_1_ntx.jpg")

patch1 <- (p4 + ylab("Kreatininwerte") + ggtitle("") + labs(caption="all-NTX") |
  p1 + ylab("") + ggtitle("") + labs(caption="all-IgA") | 
  p2 + ylab("") + ggtitle("") + labs(caption="r+IgA")| 
  p3 + ylab("") + ggtitle("") + labs(caption="r-IgA")) +
  plot_annotation("Krea 1Y")
patch1
save.plot("boxplot_krea_1_all.jpg")

###=============================================================================
## KREA 5Y
p1 <- gg.boxplot(data.iga,
                 y.column = "Krea_5Y",
                 title = "Krea_5Y\n(all-IgA)",
                 ylab = "Kreatininwerte") + ylim(c(0,13))
p1
save.plot("boxplot_krea_5_iga.jpg")

p2 <- gg.boxplot(data.iga.pos,
                 y.column = "Krea_5Y",
                 title = "Krea_5Y\n(r+IgA)",
                 ylab = "Kreatininwerte") + ylim(c(0,13))
p2
save.plot("boxplot_krea_5_iga_pos.jpg")

p3 <-gg.boxplot(data.iga.neg,
                y.column = "Krea_5Y",
                title = "Krea_5Y\n(r-IgA)",
                ylab = "Kreatininwerte") + ylim(c(0,13))
p3
save.plot("boxplot_krea_5_iga_neg.jpg")

p4 <- gg.boxplot(data.ntx,
                 y.column = "Krea_5Y",
                 title = "Krea_5Y\n(all-NTX)",
                 ylab = "Kreatininwerte") + ylim(c(0,13))
p4
save.plot("boxplot_krea_5_ntx.jpg")

patch5 <- (p4 + ylab("Kreatininwerte") + ggtitle("") + labs(caption="all-NTX") |
    p1 + ylab("") + ggtitle("") + labs(caption="all-IgA") | 
    p2 + ylab("") + ggtitle("") + labs(caption="r+IgA")| 
    p3 + ylab("") + ggtitle("") + labs(caption="r-IgA")) +
  plot_annotation("Krea 5Y")
patch5
save.plot("boxplot_krea_5_all.jpg")
###=============================================================================
## KREA 10Y
p1 <- gg.boxplot(data.iga,
                 y.column = "Krea_10Y",
                 title = "Krea_10Y\n(all-IgA)",
                 ylab = "Kreatininwerte") + ylim(c(0,13))
p1
save.plot("boxplot_krea_10_iga_all.jpg")
p2 <- gg.boxplot(data.iga.pos,
                 y.column = "Krea_10Y",
                 title = "Krea_10Y\n(r+IgA)",
                 ylab = "Kreatininwerte") + ylim(c(0,13))
p2
save.plot("boxplot_krea_10_iga_pos.jpg")

p3 <-gg.boxplot(data.iga.neg,
                y.column = "Krea_10Y",
                title = "Krea_10Y\n(r-IgA)",
                ylab = "Kreatininwerte") + ylim(c(0,13))
p3
save.plot("boxplot_krea_10_iga_neg.jpg")

p4 <- gg.boxplot(data.ntx,
                 y.column = "Krea_10Y",
                 title = "Krea_10Y\n(all-NTX)",
                 ylab = "Kreatininwerte") + ylim(c(0,13))
p4
save.plot("boxplot_krea_10_ntx.jpg")

patch10 <- (p4 + ylab("Kreatininwerte") + ggtitle("") + labs(caption="all-NTX") |
             p1 + ylab("") + ggtitle("") + labs(caption="all-IgA") | 
             p2 + ylab("") + ggtitle("") + labs(caption="r+IgA")| 
             p3 + ylab("") + ggtitle("") + labs(caption="r-IgA")) +
  plot_annotation("Krea 10Y")
patch10
save.plot("boxplot_krea_10_all.jpg")
###=============================================================================
patch1 / patch5 / patch10
save.plot("boxplot_krea_all.jpg")
###=============================================================================
rm( list = ls()[grep(x = ls(), pattern = "^p")])