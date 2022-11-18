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
