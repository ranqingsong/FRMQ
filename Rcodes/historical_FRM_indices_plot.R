##Creating Plots of FRM candidates, which are calculated as average or quantiles of lambdas####

#clean environment
rm(list = ls(all = TRUE))

#set wd
setwd("")

#install and load packages
libraries = c("dplyr","RColorBrewer")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

#import estimated lambda
date_start = 20190101
date_end = 20201231
date_start2 = 20070402
date_end2 = 20091231
channel = "Americas"
channel2 = "Europe"

#save lambdas series and calculate FRM candidates
create_FRM <- function(date_start, date_end, channel){
  #import rds data
  FRM_history = readRDS(paste0("FRM_",date_start,"-",date_end, channel,".rds"))
  N_h = length(FRM_history)
  ##calculate FRM candidates
  #calculate mean of lambda 
  FRM_mean = sapply(1:N_h, function(i) mean(FRM_history[[i]]))
  #calculate quantiles of lambda 
  FRM_index_novel= sapply(1:N_h, function(i) quantile(FRM_history[[i]], c(0.25, 0.5, 0.6,  0.7, 0.75, 0.8,  0.9)))
  FRM_index_novel= t(FRM_index_novel)
  FRM_index_all = data.frame(date = names(FRM_history), FRM_index=FRM_mean, FRM=FRM_index_novel)
  FRM_index_all = FRM_index_all %>% mutate (ticker = as.Date(date, format = "%Y%m%d"))
  FRM_index_all$date <-NULL
  colnames(FRM_index_all)[2:8] = c("FRM_q25","FRM_q50", "FRM_q60","FRM_q70","FRM_q75","FRM_q80","FRM_q90")
  #calculate IQR of lambda 
  FRM_index_all$FRM_iqr = 0.5*(FRM_index_all$FRM_q75 - FRM_index_all$FRM_q25)
  FRM_index_all$FRM_q25 <- NULL
  FRM_index_all$FRM_q75 <- NULL
  col_order <- c("ticker","FRM_index","FRM_q50", "FRM_q60","FRM_q70","FRM_q80","FRM_q90","FRM_iqr")
  FRM_index_all <- FRM_index_all[, col_order]
  
  #plot calculated FRM candidates 
  cols <- brewer.pal(9, "Set1")
  plot(x=FRM_index_all$ticker, y = FRM_index_all$FRM_iqr, type = "l",col = cols[7], lwd = 2, ylim = c(0,max(FRM_index_all$FRM_q90)),
       xlim = c(min(FRM_index_all$ticker), max(FRM_index_all$ticker)), xaxt="none", xlab = "", ylab = "", cex.lab=1.5, cex.axis=1.5)
  lines(FRM_index_all$ticker, FRM_index_all$FRM_q50, col = "darkred", type = "l", lwd = 2)
  lines(FRM_index_all$ticker, FRM_index_all$FRM_q60, col = cols[2], lwd = 2)
  lines(FRM_index_all$ticker, FRM_index_all$FRM_q70, col = cols[3], lwd = 2)
  lines(FRM_index_all$ticker, FRM_index_all$FRM_q80, col = cols[4], lwd = 2)
  lines(FRM_index_all$ticker, FRM_index_all$FRM_q90, col = cols[5], lwd = 2)
  lines(FRM_index_all$ticker, FRM_index_all$FRM_index, col = "black", lwd=2)
  ll=seq(min(FRM_index_all$ticker), max(FRM_index_all$ticker), "quarter")
  axis.Date(1, at = ll, labels =ll , font=1, las=1, cex.axis= 1.5)
}
create_FRM(date_start, date_end, channel)
create_FRM(date_start2, date_end2, channel)
create_FRM(date_start, date_end, channel2)
create_FRM(date_start2, date_end2, channel2)


