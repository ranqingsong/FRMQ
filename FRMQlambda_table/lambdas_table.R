####Creating boxplots of historical lambdas for Americas and Europe####

#clean environment
rm(list = ls(all = TRUE))
#setwd("~/Dropbox/Masterarbeit/02_project/FRM_All/03_Quantlets")
setwd("C:/Users/srq04/Dropbox/Masterarbeit/02_project/FRM_All/03_Quantlets")
#libraries = c("magick", "texreg", "zoo","pscl", "plotly", "lubridate","tidyr", "dplyr","graphics",
#              "stringr", "timeDate", "igraph", "data.table", "ggplot2","kableExtra","webshot","stargazer")
#lapply(libraries, function(x) if (!(x %in% installed.packages())) {
#  install.packages(x)
#})

#lapply(libraries, library, quietly = TRUE, character.only = TRUE)
#import estimated lambda
date_start2 = 20190101
date_end2 = 20201231
date_start = 20070402
date_end = 20091231
channel = "Americas"
channel2 = "Europe"

#Transform the list of lambdas into a wide dataset
create_daily_lambas <- function(date_start,date_end,channel){
  FRM_history = readRDS(paste0("05_rdsdata/FRM_",date_start,"-",date_end, channel,".rds"))
  stock_names = vector()
  N_h = length(FRM_history)
  for (t in 1:N_h) stock_names = c(stock_names, attributes(FRM_history[[t]])$dimnames[[2]])
  stock_names = unique(stock_names)
  N_names = length(stock_names)
  lambdas_wide = matrix(0, N_h, N_names+1)
  lambdas_wide[, 1] = names(FRM_history)
  for (k in 1:N_names) 
    for (t in 1:N_h) 
      if (stock_names[k] %in% attributes(FRM_history[[t]])$dimnames[[2]]) 
        lambdas_wide[t, k+1] = FRM_history[[t]][, stock_names[k]]
  colnames(lambdas_wide) = c("date", stock_names)
  lambdas_wide <- data.frame(lambdas_wide)
  lambdas_wide[lambdas_wide==0] <- NA
  write.csv(lambdas_wide, paste0("01_csvData/lambdas_wide_",date_start,"-",date_end, channel,".csv"), 
            row.names = FALSE, quote = FALSE)
}
create_daily_lambas(date_start,date_end,channel)
create_daily_lambas(date_start2,date_end2,channel)
create_daily_lambas(date_start,date_end,channel2)
create_daily_lambas(date_start2,date_end2,channel2)
