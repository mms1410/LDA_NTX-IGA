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
tmp_data_iga_path <- paste0(tmp_data_iga_path,
                        .Platform$file.sep,
                        "IgAN_Sheet2.csv")

##
tmp_iga2_select <- c("T-date" = "character",
                     "T-dls" = "character",
                     "graft loss (0=functial, 1=loss)" = "factor",
                     "graft loss date" = "character",
                     "max FUP graft (years)" = "numeric",
                     "Pat death (0=alive, 1= dead)" = "factor",
                     "max FUP survivial (years)" = "numeric",
                     "biopsy after KTX  (0=no, 1=yes)" = "factor",
                     "biopsy proven recurrence (0=no, 1=yes)" = "factor",
                     "date of biopsy" = "character",
                     "time of biopsy (years after KTX)" = "numeric",
                     "Date of birth" = "character",
                     "D-type" = "factor",
                     "D-cod" = "factor",
                     "D-age" = "numeric",
                     "D-abo" = "factor",
                     "D-sex" = "factor",
                     "R-sex"  = "factor",
                     "R-abo" = "factor",
                     "R Full Phenotype" = "factor",
                     "D-weight" = "numeric",
                     "D-height" = "numeric",
                     "D-pheno" = "factor",
                     "T-fc" = "factor",
                     "R-dc" = "factor",
                     "R-weight" = "numeric",
                     "R-height" = "numeric",
                     "Cold ischaemic period hours" = "numeric",
                     "Cold ischaemic period minutes" = "numeric",
                     "Warm ischaemic period 2" = "numeric",
                     "mm-A" = "factor",
                     "mm-B" = "factor",
                     "mm-DR" = "factor",
                     "Current PRA%" = "numeric",
                     "Highest PRA%" = "numeric")


tmp_iga2_dmy <- c("T-date",
                  "T-dls",
                  "graft loss date",
                  "date of biopsy",
                  "Date of birth"
                  )

tmp_ntx_select <- c("Geburtsdatum"  = "character" ,
                    "Datum" = "character",
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
                 "Datum",
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
tmp_pattern = "^([0]?[1-9]|[1|2][0-9]|[3][0|1])[./-]([0]?[1-9]|[1][0-2])[./-]([0-9]{4}|[0-9]{2})"
data_ntx[, Datum := regmatches(Datum,
                               regexpr(pattern = tmp_pattern,
                                       text = Datum))]
data_ntx[, (tmp_ntx_dmy) := lapply(.SD, lubridate::dmy), .SDcols = tmp_ntx_dmy]
data_ntx[, R_age_Datum := interval(Geburtsdatum, Datum) / years(1)]
## drop certain patients
data_ntx <- data_ntx[R_age_Datum > 18]
data_ntx <- data_ntx[is.na(`Transplantatfunktionsende 3[NTX PatientenInformation]`) & is.na(`Transplantatfunktionsende 5[NTX PatientenInformation]`) & is.na(`Transplantatfunktionsende 6[NTX PatientenInformation]`)] 
data_ntx[, tdls := fcase(!is.na(`Date last seen[NTX PatientenInformation]`), `Date last seen[NTX PatientenInformation]`,
      !is.na(`Todesdatum[NTX PatientenInformation]`), `Todesdatum[NTX PatientenInformation]`)]
################################## iga #########################################
data_iga <- fread(tmp_data_iga_path, 
                   select = tmp_iga2_select,
                   encoding = "UTF-8",
                   na.strings = c("", "-"),
                   dec = ",")

data_iga <- data_iga[!is.na(`T-date`)]
data_iga[, (tmp_iga2_dmy) := lapply(.SD, lubridate::dmy), .SDcols = tmp_iga2_dmy]
data_iga[, R_age_Tdate := interval(`Date of birth`, `T-date`) / years(1)]
data_iga[, R_age_Tdls := interval(`Date of birth`, `T-dls`) / years(1)]
data_iga <- data_iga[R_age_Tdate > 18]

tmp_to_merge <- c("Transplantatfunktionsende 1[NTX PatientenInformation]",
                  "Transplantatfunktionsende 2[NTX PatientenInformation]",
                  "Transplantatfunktionsende 3[NTX PatientenInformation]",
                  "Transplantatfunktionsende 5[NTX PatientenInformation]",
                  "Transplantatfunktionsende 6[NTX PatientenInformation]")
data_ntx[, Transplantatfunktionsende := fcoalesce(
  data_ntx[ ,tmp_to_merge, with = FALSE])]  ## NOTE: only the first non NA value will be chosen in fcoalesce

                                                     
data_ntx$`TX Status[NTX PatientenInformation]` <-fct_collapse(data_ntx$`TX Status[NTX PatientenInformation]`,
                                                                    "1 - mit Transplantatfunktion" = c("1 - mit Transplantatfunktion"),
                                                              "2 - ohne Transplantatfunktion" = c("2 - ohne Transplantatfunktion", "2- ohne Transplantatfunktion"))
data_ntx[, R_age_Tdls := interval(Geburtsdatum, `Date last seen[NTX PatientenInformation]`) / years(1)]
################################################################################
# remove tmp variables not used any more
rm( list = ls()[grep(x = ls(), pattern = "^tmp")])