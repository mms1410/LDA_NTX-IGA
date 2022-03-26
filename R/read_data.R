# load packages 
library(readxl)
library(data.table)
library(forcats)
# load data
##
## check if whitespaces in filenames in data folder and replace by underscore
## ntx path
tmp_data_ntx_path <- dirname(rstudioapi::getSourceEditorContext()$path)  # <folder>/R
tmp_data_ntx_path <- dirname(tmp_data_ntx_path)  # <folder>
tmp_data_ntx_path <- paste0(tmp_data_ntx_path, .Platform$file.sep, "data")  # <folder>/data
tmp_data_ntx_path <- paste0(tmp_data_ntx_path,
                        .Platform$file.sep,
                        "ntx_daten.csv")
## iga path
tmp_data_iga_path <- dirname(rstudioapi::getSourceEditorContext()$path)  # <folder>/R
tmp_data_iga_path <- dirname(tmp_data_iga_path)  # <folder>
tmp_data_iga_path <- paste0(tmp_data_iga_path, .Platform$file.sep, "data")  # <folder>/data
tmp_data_iga1_path <- paste0(tmp_data_iga_path,
                        .Platform$file.sep,
                        "iga_sheet1.csv")
tmp_data_iga2_path <- paste0(tmp_data_iga_path,
                             .Platform$file.sep,
                             "iga_sheet2.csv")
##
tmp_iga1_select <- c("Datum" = "character",
                    "Geburtsdatum" = "character",
                    "Geschlecht" = "factor",
                    "TX Status" = "factor",
                    "Date last seen" = "character",
                    "Todesdatum" = "character",
                    "Transplantatfunktionsende 1" = "character",
                    "Transplantatfunktionsende 2" = "character",
                    "Transplantatfunktionsende 4" = "character",
                    "Transplantatfunktionsende 5" = "character",
                    "Transplantatfunktionsende 6" = "character",
                    "T-date" = "character",
                    "D-type" = "factor",
                    "D-age" = "factor",
                    "D-abo" = "factor",
                    "D-sex" = "factor",
                    "R-sex" = "factor",
                    "first-name" = "character",
                    "last-name" = "character")
tmp_iga1_dmy <- c("Datum",
                  "Geburtsdatum",
                  "Date last seen",
                  "Todesdatum",
                  "Transplantatfunktionsende 1",
                  "Transplantatfunktionsende 2",
                  "Transplantatfunktionsende 4",
                  "Transplantatfunktionsende 5",
                  "Transplantatfunktionsende 6",
                  "T-date")
tmp_ntx_select <- c("Geburtsdatum"  = "character" ,
                    "Geschlecht"  = "factor" ,
                    "Transplantatfunktionsende 1[NTX PatientenInformation]" = "character",
                    "Transplantatfunktionsende 2[NTX PatientenInformation]" = "character",
                    "Transplantatfunktionsende 3[NTX PatientenInformation]" = "character",
                    "Transplantatfunktionsende 5[NTX PatientenInformation]" = "character",
                    "Transplantatfunktionsende 6[NTX PatientenInformation]" = "character",
                    "TX Status[NTX PatientenInformation]" = "factor",
                    "Patienten Status[NTX PatientenInformation]" = "factor",
                    "Date last seen[NTX PatientenInformation]" = "character", 
                    "Todesdatum[NTX PatientenInformation]" = "character")
tmp_ntx_dmy <- c("Geburtsdatum",
                  "Transplantatfunktionsende 1[NTX PatientenInformation]",
                  "Transplantatfunktionsende 2[NTX PatientenInformation]",
                  "Transplantatfunktionsende 3[NTX PatientenInformation]",
                  "Transplantatfunktionsende 5[NTX PatientenInformation]",
                  "Transplantatfunktionsende 6[NTX PatientenInformation]",
                  "Date last seen[NTX PatientenInformation]",
                  "Todesdatum[NTX PatientenInformation]")
################################## ntx #########################################
data_ntx <- fread(tmp_data_ntx_path,
                  select = tmp_ntx_select,
                  encoding = "UTF-8",
                  na.strings = "")
## date format
data_ntx[, (tmp_ntx_dmy) := lapply(.SD, lubridate::dmy), .SDcols = tmp_ntx_dmy]
################################## iga #########################################
data_iga1 <- fread(tmp_data_iga1_path,
                  select = tmp_iga1_select,
                  encoding = "UTF-8",
                  na.strings = "")
data_iga1$Datum[!is.na(data_iga1$Datum)] <- regmatches(data_iga1$Datum,
                                                     regexpr("\\d\\d.\\d\\d.\\d\\d\\d\\d",
                                                             data_iga1$Datum))
## date format
data_iga1[, (tmp_iga1_dmy) := lapply(.SD, lubridate::dmy), .SDcols = tmp_iga1_dmy]

## merge `Transplantationsende \d`


## remove names
data_iga1 <- data_iga1[!is.na(Datum)]
data_iga1[, `first-name`:= NULL]
data_iga1[, `last-name` := NULL]
################################################################################
# remove tmp variables not used any more
rm( list = ls()[grep(x = ls(), pattern = "^tmp")])