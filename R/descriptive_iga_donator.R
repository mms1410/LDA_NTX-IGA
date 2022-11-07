################################################################################
# this script contains descriptive statistics on iga patients containing donator
# characteristics only
################################################################################
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