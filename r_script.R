path <- paste0(dirname(rstudioapi::getSourceEditorContext()$path),
               .Platform$file.sep,
               "make_file_NTX_IgA.R")
source(path)

daten_iga_0$graft_loss_date <- as.Date(daten_iga_0$graft_loss_date, format = "%d.%m.%Y")
str(daten_iga_0$follow_up)
sum(daten_iga_0$graft_loss_date < daten_iga_0$follow_up, na.rm = TRUE)

X <- subset(daten_iga_0, select = c(death,death_date, biopsy_proven_recurrence,graft_loss_date,graft_loss,
                                  T_date,follow_up, date_last_seen, date_of_birth))
library(survival)


# 2.1
# IGA0 T_verlust Graft Loss Date


sum(X$T_date > X$follow_up)
X$death_date < X$follow_up
as.numeric(X$death_date)
Y <- subset(X, select = c(graft_loss_date, date_last_seen, follow_up))
censoring <- apply(cbind(Y$date_last_seen, Y$follow_up), FUN = min, MARGIN = 1)
Y$graft_loss_date 
censoring # num
as.numeric(Y$graft_loss_date) < censoring
get_last_date_and_status <- function(dt, dates, event_name){
  min_col <- which.min(as.numeric(dates)) # na ignored
  ##cat(paste0("min col: ", min_col))
  if(colnames(dates)[[min_col]] == event_name){
    #status <- 1 # observed
    return(list(last_date = dates[[min_col]], status =  1))
  } else{
    #status <- 0 # censored
    return(list(last_date = dates[[min_col]],  status = 0))
  }
}
dates <- Y[2,]
################################################################################


