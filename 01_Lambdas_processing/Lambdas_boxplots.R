####Creating boxplots of historical lambdas for Americas and Europe####

#clean environment
rm(list = ls(all = TRUE))
setwd("~/Dropbox/Masterarbeit/02_project/FRM_All/03_Quantlets")
setwd("C:/Users/srq04/Dropbox/Masterarbeit/02_project/FRM_All/03_Quantlets")

#lapply(libraries, library, quietly = TRUE, character.only = TRUE)
#import estimated lambda
date_start2 = 20190101
date_end2 = 20201026
date_start = 20070402
date_end = 20091231
channel = "Americas"
channel2 = "Europe"

#boxplot of lambda
### boxplots of ordinary FRM index and the maximum of FRM
create_boxplot <- function(date_start, date_end,date_start2, date_end2, channel){
  FRM_history = readRDS(paste0("05_rdsdata/FRM_",date_start,"-",date_end, channel,".rds"))
  N_upd= length(FRM_history)
  FRM_index = sapply(1:N_upd, function(i) mean(FRM_history[[i]]))
  FRM_max = sapply(1:N_upd, function(i) max(FRM_history[[i]]))
  FRM_history2 = readRDS(paste0("05_rdsdata/FRM_",date_start2,"-",date_end2, channel,".rds"))
  N_upd2= length(FRM_history2)
  FRM_index2 = sapply(1:N_upd2, function(i) mean(FRM_history2[[i]]))
  FRM_max2 = sapply(1:N_upd2, function(i) max(FRM_history2[[i]]))
  png(paste0("03_plots/Boxplot_",  
             channel, ".png"), width = 1500, height = 1000, bg = "transparent")
  par(mfrow =c(1,2))
  {boxplot(FRM_history, col = "white", xaxt = "n", outline = FALSE, ylim = c(0, max(FRM_max)), cex.axis=2)
  lines(tail(FRM_index, N_upd), col = "blue", lwd = 2)
  lines(tail(FRM_max, N_upd), col = "red", lwd = 2)
  if (date_start == "20070402"){
    boxplot_labels = c(20070402,20070702,20071001,20080101, 20080401,20080701,20081001, 20090101, 20090401, 20090701, 20091001)
  }else{
    boxplot_labels = c(20190102,20190401,20190701,20191001, 20200101,20200401, 20200701,20201001)
  }
  ll = which(names(FRM_history) %in% boxplot_labels)
  axis(1, at = ll, labels = boxplot_labels,cex.axis=2)}
  {boxplot(FRM_history2, col = "white", xaxt = "n", outline = FALSE, ylim = c(0, max(FRM_max)), cex.axis=2)
    lines(tail(FRM_index2, N_upd2), col = "blue", lwd = 2)
    lines(tail(FRM_max2, N_upd2), col = "red", lwd = 2)
    boxplot_labels2 = c(20190102,20190401,20190701,20191001, 20200101,20200401, 20200701,20201001)
    ll2 = which(names(FRM_history2) %in% boxplot_labels2)
    axis(1, at = ll2, labels = boxplot_labels2,cex.axis=2)}
  dev.off()
}
create_boxplot(date_start, date_end, date_start2, date_end2,channel)
create_boxplot(date_start, date_end, date_start2, date_end2,channel2)
create_boxplot(date_start2, date_end2, channel2)

