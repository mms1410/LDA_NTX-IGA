library(data.table)
library(lubridate)
library(forcats)
library(dplyr)
path_sven_windows <- file.path("C:", "Users", "svenm", "Documents", "06_data",
                               fsep = .Platform$file.sep)
path_alina_mac <- file.path("Users", "bitteaendern", "01_data")
path_mms14 <- file.path("~", "03_data", fsep = .Platform$file.sep)
if(Sys.info()["user"] == "svenm"){
  path1 <- file.path(path_sven_windows,"iga_sheet2.csv",
                     fsep = .Platform$file.sep)
  path2 <- file.path(path_sven_windows,"ntx_daten.csv",
                    fsep = .Platform$file.sep) 
  path3 <- file.path(path_sven_windows,"iga_sheet1.csv",
               fsep = .Platform$file.sep)
}else if(Sys.info()["user"] == "bitteaendern"){
  path1 <- file.path(path_alina_mac,"iga_sheet2.csv",
                     fsep = .Platform$file.sep)
  path2 <- file.path(path_alina_mac,"ntx_daten.csv",
                     fsep = .Platform$file.sep) 
  path3 <- file.path(path_alina_mac,"iga_sheet1.csv",
                     fsep = .Platform$file.sep)
} else if(Sys.info()["user"] == "mms14"){
  path1 <- file.path(path_mms14, "01_csv","iga_sheet2.csv",
                     fsep = .Platform$file.sep)
  path2 <- file.path(path_mms14, "01_csv","ntx_daten.csv",
                     fsep = .Platform$file.sep) 
  path3 <- file.path(path_mms14, "01_csv", "iga_sheet1.csv",
                     fsep = .Platform$file.sep)
}
################################################################################
tmp_data <- fread(path3, na.strings = "")
tmp_columns <- c("last-name", "first-name", "Date last seen")
tmp_data <- tmp_data[, ..tmp_columns]
tmp_names <- c("last-name", "first-name", "date_last_seen")
colnames(tmp_data) <- tmp_names
tmp_data$date_last_seen <- ymd(dmy(tmp_data$date_last_seen))
# Datenaufbereitung IGA Patienten
daten_iga <- fread(path1,
                   colClasses = c("graft loss (0=functial, 1=loss)" = "factor",
                                  "Pat death (0=alive, 1= dead)" =  "factor",
                                  "biopsy proven recurrence (0=no, 1=yes)" = "factor",
                                  "D-type" = "factor",
                                  "D-age" = "numeric",
                                  "D-abo" = "factor",
                                  "D-sex" = "factor",
                                  "R-abo" = "factor",
                                  "R Full Phenotype" = "factor",
                                  "D-pheno" = "factor",
                                  "Cold ischaemic period hours" = "numeric",
                                  "Cold ischaemic period minutes" = "numeric",
                                  "mm-B" = "factor",
                                  "mm-DR" = "factor",
                                  "Current PRA%" = "numeric",
                                  "Highest PRA%" = "numeric"
                   ), na.strings = "")
names_date_type <- c("Date of birth",
                     "T-date", "Todesdatum")
daten_iga[, (names_date_type) := lapply(.SD, function(x){ymd(dmy(x))}), .SDcols = names_date_type]
#### Teilnehmer unter 18 Jahren zur Zeit der Operation entfernen  
daten_iga <- daten_iga[(daten_iga$`T-date` - daten_iga$`Date of birth`) >= 18 * 365, ]
#### pseudo attribute die mit "V" anfangen loeschen
#### vmtl entstanden durch umwandlung von xmlx zu csv
tmp_colnames <- grepl("^V.*", colnames(daten_iga))
tmp_colnames <- colnames(daten_iga)[tmp_colnames]
daten_iga[,(tmp_colnames) := NULL]
##### check for non matching names
tmp_namen_tmp <- paste(tmp_data$`first-name`, tmp_data$`last-name`)
tmp_namen_iga <- paste(daten_iga$`first-name`, daten_iga$`last-name`)
tmp_mismatches <- tmp_namen_iga[!tmp_namen_iga %in% tmp_namen_tmp]
cat("==================WARNUNG!==================",
    "\n", "Die folgenden Namen aus Sheet2 IgA wurden nicht in Sheet1 IGA gefunden:",
    "\n",tmp_mismatches, "\n", "Entsprechende Attributwerte sind auf NA gesetzt!",
    "\n", "============================================")
# Daten aus Sheet 1 hinzufuegen
daten_iga <- merge.data.table(daten_iga,tmp_data, by = c("last-name", "first-name"), all.x = TRUE)
# Namen nicht relevant
daten_iga <- daten_iga[,-c(1,2)] 
names(daten_iga) <- c(
  "graft_loss", "graft_loss_date", "max_FUP_graft", "death", 
  "death_date", "max_FUP_surv", "biopsy_proven_recurrence",
  "date_of_birth", "T_date","D_type", "D_age", "D_abo", "D_sex",
  "R_abo", "R_full_phenotype", "D_pheno","cold_ischaemic_period_h", 
  "cold_ischaemic_period_m", "mm_B", "mm_DR", "current_PRA", 
  "highest_PRA", "date_last_seen")
daten_iga <- cbind(daten_iga,daten_iga$T_date + years(10))
names(daten_iga)[length(names(daten_iga))] <- "follow_up"
daten_iga$max_FUP_graft <- as.numeric(gsub(",",".",daten_iga$max_FUP_graft))
daten_iga$max_FUP_surv <- as.numeric(gsub(",",".",daten_iga$max_FUP_surv))
daten_iga_0 <- daten_iga[biopsy_proven_recurrence == 0]
daten_iga_1 <- daten_iga[biopsy_proven_recurrence == 1]
#################### Datenaufbereitung NTX Patienten ###########################
daten_ntx <- fread(path2, colClasses = c("Patienten Status[NTX PatientenInformation]" = "factor",
                                         "TX Status[NTX PatientenInformation]" = "factor",
                                         "Geschlecht" = "factor",
                                         "Bezeichnung" = "factor"),na.strings = "")
colnames_ntx <- c("Datum", "Bezeichnung", "Patienten_ID", "Familienname", "Vorname", "Geburtsdatum",
                  "Geschlecht", "NTXNR", "TX_FU_Ende_Liv1",
                  "T_Ende_1", "T_Ende_2", "T_Ende_3", "T_Ende_4", "T_Ende_5", "T_Ende_6",
                  "TX_Status", "P_Status", "Last_Seen", "Todesdatum")
colnames(daten_ntx) <- colnames_ntx

#### Factor levels 
daten_ntx$TX_Status<-  fct_collapse(daten_ntx$TX_Status,
                                    "mit" = c("1 - mit Transplantatfunktion"),
                                    "ohne" = c("2 - ohne Transplantatfunktion", "2- ohne Transplantatfunktion"))
daten_ntx$P_Status <- fct_collapse(daten_ntx$P_Status,
                                   "lebt" = c("1 - lebt"),
                                   "verstorben" = c("2 - verstorben"))
levels(daten_ntx$Geschlecht) <- c("m", "w")
#### Uhrzeiten aus Datum Variable entfernen
daten_ntx[, ("Datum") := lapply(.SD, function(x){substr(x, 1, 10)}), .SDcols = "Datum"]

#### Zeit/Datum Datentyp erstellen
namen_ntx_sheet1 <- c("Datum", "Geburtsdatum",
                      "T_Ende_1",
                      "T_Ende_2",
                      "T_Ende_3",
                      "T_Ende_4",
                      "T_Ende_5",
                      "T_Ende_6",
                      "Last_Seen", 
                      "Todesdatum")
daten_ntx[, (namen_ntx_sheet1) := lapply(.SD, function(x){ymd(dmy(x))}), .SDcols = namen_ntx_sheet1]
#### Teilnehmer unter 18 Jahren zur Zeit der Operation entfernen  
daten_ntx <- daten_ntx[interval(daten_ntx$Geburtsdatum, daten_ntx$Datum) / years(1) >= 18, ]

# follow_up time
daten_ntx <- cbind(daten_ntx, 
                   daten_ntx$Datum + years(10)
)
names(daten_ntx)[length(names(daten_ntx))] <- "follow_up"
# Irrelevante Spalten löschen
daten_ntx$Patienten_ID <- NULL
daten_ntx$Familienname <- NULL
daten_ntx$Vorname <- NULL
daten_ntx$Bezeichnung <- NULL
daten_ntx$TX_Status <- relevel(daten_ntx$TX_Status, ref = "ohne")
# T_Ende_x können in eine Variable subsumiert werden, weil jeder Patient
# innerhalb des follow-up, wenn er ein Transplantatverlust hatte, genau ein 
# Transplantat verlor.
daten_ntx <- daten_ntx %>% 
  mutate(Transplantatversagen = case_when(
    !is.na(daten_ntx$T_Ende_1) ~ daten_ntx$T_Ende_1,
    !is.na(daten_ntx$T_Ende_2) ~ daten_ntx$T_Ende_2,
    !is.na(daten_ntx$T_Ende_3) ~ daten_ntx$T_Ende_3,
    !is.na(daten_ntx$T_Ende_4) ~ daten_ntx$T_Ende_4,
    !is.na(daten_ntx$T_Ende_5) ~ daten_ntx$T_Ende_5,
    !is.na(daten_ntx$T_Ende_6) ~ daten_ntx$T_Ende_6
    
  ))
daten_ntx$T_Ende_1 <- NULL
daten_ntx$T_Ende_2 <- NULL
daten_ntx$T_Ende_3 <- NULL
daten_ntx$T_Ende_4 <- NULL
daten_ntx$T_Ende_5 <- NULL
daten_ntx$T_Ende_6 <- NULL
############################ speichern und bereinigen #########################
# save data as R
if(Sys.info()["user"] == "svenm"){
  save(daten_iga, file = file.path(path_sven_windows,"daten_IgA.Rdata",
                                   fsep = .Platform$file.sep))
  save(daten_ntx, file = file.path(path_sven_windows,"daten_NTX.Rdata",
                                   fsep = .Platform$file.sep))
}else if(Sys.info()["user"] == "bitteaendern"){
  save(daten_iga, file = file.path(path_alina_mac,"daten_IgA.Rdata",
                                   fsep = .Platform$file.sep))
  save(daten_ntx, file = file.path(path_alina_mac,"daten_NTX.Rdata",
                                   fsep = .Platform$file.sep))
} else if(Sys.info()["user"] == "mms14"){
  save(daten_iga, file = file.path(path_mms14,"06_Rdata", "daten_IgA.Rdata",
                                   fsep = .Platform$file.sep)) # cannot use too long path name here !!!
  save(daten_ntx, file = file.path(path_mms14,"06_Rdata", "daten_NTX.Rdata",
                                   fsep = .Platform$file.sep))
} 
# remove all tmp_* data and variables
rm(list = ls()[grep(pattern = "tmp_[\\w\\W]*", ls())])
rm(list = ls()[grep(pattern = "path[\\w\\W]*", ls())])
rm(list = ls()[grep(pattern = "names[\\w\\W]*", ls())])
rm(list = ls()[grep(pattern = "colnames[\\w\\W]*", ls())])
rm(list = ls()[grep(pattern = "namen[\\w\\W]*", ls())])