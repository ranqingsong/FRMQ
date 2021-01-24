#create lagged FRM candidates, which are used to construct recession models

#clean environment
rm(list = ls(all = TRUE))
setwd("")

#install and load packages
libraries = c("xtable", "dplyr","lubridate", "texreg")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

#settings 
date_start1 = 20070402
date_end1 = 20091231
date_start2 = 20190101
date_end2 = 20201231
channel = "Americas"
channel2 = "Europe"
##import FRM datasets and prepare data
create_lag_table <- function(date_start1, date_end1, channel){
  data <- read.csv(paste0("FRM_index_all",date_start1,"-",date_end1, channel,".csv"), 
                   colClasses = c("Date","numeric","numeric","numeric","numeric","numeric","numeric","numeric")) 
  data <- data %>% mutate(ym = paste0(year(ticker), "-",format(ticker, "%m")))
  ####import monthly recession indicator
  if (channel== "Americas") {
    recdata<- read.csv("USREC_monthly.csv")
    recdata$date <- as.Date(recdata$DATE, format = "%Y-%m-%d")
    recdata$ym <- paste0(year(recdata$date), "-",format(recdata$date, "%m"))
    recdata <- rename(recdata, rec=USREC)
    recdata <- select(recdata, c("ym", "rec","date"))
  } else {
    recdata<- read.csv("EUROREC.csv")
    recdata$date <- as.Date(recdata$DATE, format = "%Y-%m-%d")
    recdata$ym <- paste0(year(recdata$date), "-",format(recdata$date, "%m"))
    recdata <- rename(recdata, rec=EUROREC)
    recdata <- select(recdata, c("ym", "rec"))
  }
  
  #calculate monthly averaged FRM candidates
  data_m = aggregate(cbind(FRM_index,FRM_q50, FRM_q60, FRM_q70, FRM_q80, FRM_q90,FRM_iqr)~ ym, 
                     data = data, mean)
  
  #lag FRM candidates at 1 to 6 months
  for (lag_num in 1:6) {
    lags = sapply(2:8, function(i) dplyr::lag(data_m[,i], lag_num))
    collist = c("FRM_index","FRM_q50", "FRM_q60","FRM_q70","FRM_q80","FRM_q90","FRM_iqr")
    colnames(lags) = paste0("lag", lag_num,"_",collist)
    data_m = cbind(data_m, lags)
  }
  
  #combine FRM and recession data
  data_m<- merge(data_m, recdata, by = "ym", all.x = T)
  write.csv(data_m, paste0("lagged_daily_FRM_index_all",date_start1,"-",date_end1, channel,".csv"),
            row.names = FALSE, quote = FALSE)
  write.csv(data_m, paste0("lagged_FRM_index_all",date_start1,"-",date_end1, channel,".csv"),
            row.names = FALSE, quote = FALSE)
}
create_lag_table(date_start1, date_end1, channel)
create_lag_table(date_start2, date_end2, channel)
create_lag_table(date_start1, date_end1, channel2)
create_lag_table(date_start2, date_end2, channel2)

