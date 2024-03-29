stderr <- function(x) sd(x, na.rm = TRUE) / sqrt(length(!is.na(x)))
c(mean = mean(data_iga[`D-type` == "Living", `D-age`]),
  stderr = stderr(data_iga[`D-type` == "Living", `D-age`]))


data_iga$`Cold ischaemic period minutes`
sum(is.na(data_iga$`Cold ischaemic period hours`))

data_iga_pos[!is.na(`Cold ischaemic period hours`),
         .(`Cold ischaemic period hours`,
           `Cold ischaemic period minutes`)][,
        minutes_total := `Cold ischaemic period hours` * 60][,
        lapply(.SD,function(x){ifelse(is.na(x),0,x)})][, minutes_total := minutes_total + `Cold ischaemic period minutes`][, minutes_total] %>% 
  mean()
data_iga_pos[!is.na(`Cold ischaemic period hours`),
         .(`Cold ischaemic period hours`,
           `Cold ischaemic period minutes`)][,
        minutes_total := `Cold ischaemic period hours` * 60][,
        lapply(.SD,function(x){ifelse(is.na(x),0,x)})][, minutes_total := minutes_total + `Cold ischaemic period minutes`][, minutes_total] %>% 
  stderr()

data_iga_neg[!is.na(`Cold ischaemic period hours`),
             .(`Cold ischaemic period hours`,
               `Cold ischaemic period minutes`)][,
                                                 minutes_total := `Cold ischaemic period hours` * 60][,
                                                                                                      lapply(.SD,function(x){ifelse(is.na(x),0,x)})][, minutes_total := minutes_total + `Cold ischaemic period minutes`][, minutes_total] %>% 
  mean()
data_iga_neg[!is.na(`Cold ischaemic period hours`),
             .(`Cold ischaemic period hours`,
               `Cold ischaemic period minutes`)][,
                                                 minutes_total := `Cold ischaemic period hours` * 60][,
                                                                                                      lapply(.SD,function(x){ifelse(is.na(x),0,x)})][, minutes_total := minutes_total + `Cold ischaemic period minutes`][, minutes_total] %>% 
  stderr()
  
data_iga[!is.na(`Cold ischaemic period hours`),
             .(`Cold ischaemic period hours`,
               `Cold ischaemic period minutes`)][,
                                                 minutes_total := `Cold ischaemic period hours` * 60][,
                                                                                                      lapply(.SD,function(x){ifelse(is.na(x),0,x)})][, minutes_total := minutes_total + `Cold ischaemic period minutes`][, minutes_total] %>% 
  quantile()
data_iga[!is.na(`Cold ischaemic period hours`),
             .(`Cold ischaemic period hours`,
               `Cold ischaemic period minutes`)][,
                                                 minutes_total := `Cold ischaemic period hours` * 60][,
                                                                                                      lapply(.SD,function(x){ifelse(is.na(x),0,x)})][, minutes_total := minutes_total + `Cold ischaemic period minutes`][, minutes_total] %>% 
  IQR()


p1 <- gg_boxplot(data = data_iga, column = "cold_time_sum_min",
                 xlab = "IGA all", ylab = "Cold isch time")
p2 <- gg_boxplot(data = data_iga_neg, column = "cold_time_sum_min", xlab = "IGA -")
p3 <- gg_boxplot(data = data_iga_pos, column = "cold_time_sum_min", xlab = "IGA +")
patch <- p1 | p2 | p3
patch <- patch + plot_annotation(
  title = "Boxplot Cold Isch. Time min"
)
patch & ylim(0, 1600) & theme(axis.ticks.x = element_blank(),
                              axis.text.x = element_blank())
mismatch_out(data_iga)
mismatch_out(data_iga_neg)
mismatch_out(data_iga_pos)
################################################################################

levs <- levels(unlist(data[, ..x.column]))
levs
names <- c("1" = "mit", "0" = "ohne")
names(names)
unname(names[order(names(names), levs)])

data <- data_iga[, .(R_age_Tdate, `R-sex`)]
x.column <- "R-sex"
y.column <- "R_age_Tdate"

data[, ..y.column]


ggplot() +
  geom_histogram(aes(x = unlist(data[, ..y.column]), fill = unlist(data[, ..x.column])),
                 color="#e9ecef",
                 alpha=0.9,
                 position = "dodge") +
  two_scale_fill +
  xlab("") +
  ylab("") +
  default_theme
###################
model_cox_iga_1 <- coxph(formula = Surv(time = as.numeric(status_date),
                                        event = status) ~ R_age_Tdate +
                           `D-age` + `R-sex` + `D-sex` + `D-type` + cold_time_sum_min + mismatch_sum , data = data_iga)

model_cox_iga_1 <- coxph(formula = Surv(time = as.numeric(status_date),
                                        event = status) ~ R_age_Tdate +
                           `D-age` + `R-sex` + `D-sex` + `D-type` + cold_time_sum_min , data = data_iga)

y <- summary(model_cox_iga_1)
model_cox_iga_1 <- coxph(formula = Surv(time = as.numeric(status_date),
                                        event = status) ~ R_age_Tdate +
                           `D-age` + `R-sex` + `D-sex` + `D-type` + cold_time_sum_min + mismatch_sum , data = data_iga)

x <- summary(model_cox_iga_1)


data_iga$mismatch_sum

steps <- step(model_cox_iga_1)
####################
test <- sapply(model_cox_iga_1, function(x) c(summary(x), type = class(x)))
path.test <- paste0(dir.project, .Platform$file.sep, "attic", .Platform$file.sep,
                    "test.csv")

y <- broom::glance(model_cox_iga_1)
####################
ggplot(data = data_iga, aes(x = `Current PRA%`, fill = `graft loss (0=functial, 1=loss)`)) +
  geom_histogram(data = data_iga,position = "identity") +
  default_theme 

ggplot() +
  geom_histogram(data = data_iga, aes(x = `Highest PRA%`, fill = `graft loss (0=functial, 1=loss)`)) +
  default_theme

############

create.summary.num.ntx(data_ntx,
                       "follow_up_age",
                       c("follow_up_age_ntx", "follow_up_age_ntx_0", "follow_up_age_ntx_1"))


data <- data_iga
bin.breaks <- c(0, 30, 100)
colname <- "Current PRA%"
group.name <- "biopsy proven recurrence (0=no, 1=yes)"
levels.name <- c("no rec.", "with rec.", "all iga")
legend.title = ""
include.all = TRUE
xlab = "xxxlab"
ylab = ""
title = ""
lowest = TRUE
count.stat = TRUE

tbl <- data[, group := "all"]
for (lev in levels(unlist(data[, ..group.name]))) {
  idx <- as.vector(data[, ..group.name] == lev)
  tbl.tmp <- data[idx, ]
  tbl.tmp[, group := lev]
  tbl <- rbindlist(list(tbl.tmp, tbl))
}

g <- geom_histogram(aes(y = after_stat(count), group = group, fill = group),
               breaks = bin.breaks, binwidth = bindwth,
               position = position_dodge2(preserve="single"))

g <- ggplot(tbl, aes(unlist(tbl[, group.values])))  +
  g
g

ggplot(tbl, aes(x = group.values)) +
  geom_histogram(aes(fill = group),stat = "count",
                 position = "dodge") + 
  default_theme
################################################################################
data_tx <- fread(tmp_data_tx, stringsAsFactors = TRUE, encoding = "UTF-8")
data_crea <- fread(tmp_data_crea, stringsAsFactors = TRUE, encoding = "UTF-8")


data_ntx <- fread(tmp_data_ntx_path,
                  select = tmp_ntx_select,
                  encoding = "UTF-8",
                  na.strings = "")


all(data_crea[, `Patient-ID`] %in% data_ntx$`Patient-ID`)
all(data_ntx$`Patient-ID` %in% data_crea[, `Patient-ID`])

colnames(data_ntx)[colnames(data_ntx) %in% colnames(data_tx)]
colnames(data_tx)
tmp_idx <- grepl(pattern = "^Cold", x = colnames(data_tx))
gsub(x = colnames(data_tx)[tmp_idx], pattern = "-", replacement = " ")
data_tmp <- data_crea[data_ntx, on = "Patient-ID"]
data_tmp <- merge(data_crea, data_ntx, by = "Patient-ID")

data_crea <- data_crea[R_age_Datum > 18]
data_crea <- data_crea[is.na(`Transplantatfunktionsende 3[NTX PatientenInformation]`) & is.na(`Transplantatfunktionsende 5[NTX PatientenInformation]`) & is.na(`Transplantatfunktionsende 6[NTX PatientenInformation]`)] 
data_crea[, tdls := fcase(!is.na(`Date last seen[NTX PatientenInformation]`), `Date last seen[NTX PatientenInformation]`,
                         !is.na(`Todesdatum[NTX PatientenInformation]`), `Todesdatum[NTX PatientenInformation]`)]
################################################################################
fifelse(!is.na(data$Date_last_seen), data$Date_last_seen, data$Todesdatum)
fifelse((data$Date_last_seen > (data$Datum_TX + years(follow_up))), (data$Datum_TX + years(follow_up)), as.Date(NA))
create.summary.num(data.iga, "cold_time_sum_min", c("cold_iga", "cold_iga_0", "cold_iga_1"))

as.data.table(create.summary.num(data.iga, "cold_time_minutes", c("cold_iga", "cold_iga_0", "cold_iga_1")))
create.summary.num(data = data.ntx, var.name = "hla_mismatch", subset.names = "hml_mismatch_ntx",
                   include.all = TRUE, colname.split = NULL)      


gg.binhist(data = data.ntx, bin.breaks = c(0, 30, 100),
           colname = "current_PRA", 
           group.name = NULL,
           levels.name =  NULL,
           legend.title = NULL,
           include.all = NULL,
           xlab = "", ylab = "Anzahl",
           title = " Aktueller PRA%-Wert zum Transplantationszeitpunk",
           lowest = TRUE, count.stat = TRUE)

#rbindlist(list(
#  data.iga[, .(group = "all-IgA", Spendertyp = D_type)],
#  data.iga.pos[, .(group = "r+IgA", Spendertyp = D_type)],
#  data.iga.neg[, .(group = "r-IgA", Spendertyp = D_type)]
#)) %>%
#  ggplot(aes(group, fill = Spendertyp)) +
#  geom_bar(position = "fill") +
#  geom_label(aes(label=after_stat(count)),
#             stat = "count",
#             position = "fill",
#             format_string = "{:.3f}%",
#             size = 3) +
#  default_theme

############

(!is.na(data.iga$Transplantatfunktionsende) & (data.iga$Transplantatfunktionsende <  (data.iga$Datum_TX + follow_up)))
