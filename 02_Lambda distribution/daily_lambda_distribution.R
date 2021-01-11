####Creating Plots of lambda distribution for each moving window####

#clean environment
rm(list = ls(all = TRUE))

#setwd()

#libraries = c("magick", "texreg", "zoo","pscl", "plotly", "lubridate","tidyr", "dplyr","graphics",
#              "stringr", "timeDate", "igraph", "data.table", "ggplot2","kableExtra","webshot","stargazer")
libraries = c("dplyr","RColorBrewer","goft")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

#import estimated lambda
date_start1 = 20070402
date_end1 = 20091231
date_start2 = 20190101
date_end2 = 20201026
channel = "Americas"
channel2 = "Europe"

#import csv files containing daily lambda
#plot daily lambda distribution from kernel density estimation
data_input <- function(date_start, date_end, channel){
  lambdas_wide <- read.csv(paste0("lambdas_wide_",date_start,"-",date_end, channel,".csv"))
  lambdas_wide_t <- as.data.frame(t(as.matrix(lambdas_wide)))
  lambdas_wide_t<- sapply(1:ncol(lambdas_wide_t),function(i) as.numeric(lambdas_wide_t[,i]))
  rownames(lambdas_wide_t) <- colnames(lambdas_wide)
  colnames(lambdas_wide_t) <- paste0("date",lambdas_wide[,1])
  lambdas_wide_t <- lambdas_wide_t[-c(1),]
  #lambdas_wide_t2 <- data.frame(lambdas_wide_t)
  return(lambdas_wide_t)
  }

AM_data1 <- data_input(date_start1, date_end1, channel)
AM_data2 <- data_input(date_start2, date_end2, channel)
EU_data1 <- data_input(date_start1, date_end1, channel2)
EU_data2<-  data_input(date_start2, date_end2, channel2)

#lambda distribution for one single moving windows , gaussian kernel density estimation 
AM_data1 <- data.frame(AM_data1)
AM_data2 <- data.frame(AM_data2)
EU_data1 <- data.frame(EU_data1)
EU_data2 <- data.frame(EU_data2)
weibull_test(AM_data1$date20070402)


#plot
cols <- brewer.pal(9, "Set1")

png(paste0("lambda_distribution_AM_EU.png"), width = 1500, height = 1000, bg = "transparent")
par(mfrow =c(1,2))
{plot(density(AM_data1$date20070801,na.rm = T, kernel = "gaussian"), type = "l", ylim = c(0,70), xlim=c(0,0.20),
      main = "", xlab = "estimated lambda", cex.lab=1.5, cex.axis=1.5, lwd=4)
  lines(density(AM_data2$date20190603,na.rm = T, kernel = "gaussian"), type = "l", col= cols[1], lwd=3) 
  lines(density(AM_data2$date20191203,na.rm = T, kernel = "gaussian"), type = "l", col= cols[2], lwd=3)
  lines(density(AM_data1$date20081003,na.rm = T, kernel = "gaussian"), type = "l", col= cols[3], lwd=4)
  lines(density(AM_data1$date20090202,na.rm = T, kernel = "gaussian"), type = "l", col= cols[4], lwd=4)
  lines(density(AM_data2$date20200602,na.rm = T, kernel = "gaussian"), type = "l", col= cols[5], lwd=4) 
  title(main = list(paste("KDE of daily lambda in", channel)), cex.main=1.5)
  #legend("topright", c("Americas on 20070801", "Americas on 20190603", "Americas on 20191203", "Americas on 20081003", "Americas on 20090202", "Americas on 20200602"), 
  #       fill = c("black", cols[1], cols[2], cols[3], cols[4], cols[5]), cex = 1.5)
}

{plot(density(EU_data1$date20070801,na.rm = T, kernel = "gaussian"), type = "l", ylim = c(0,70), xlim=c(0,0.20),
      main = "", xlab = "estimated lambda", cex.lab=1.5, cex.axis=1.5, lwd=3)
  lines(density(EU_data2$date20190603,na.rm = T, kernel = "gaussian"), type = "l", col= cols[1], lwd=3) 
  lines(density(EU_data2$date20191203,na.rm = T, kernel = "gaussian"), type = "l", col= cols[2], lwd=3)
  lines(density(EU_data1$date20081003,na.rm = T, kernel = "gaussian"), type = "l", col= cols[3], lwd=4)
  lines(density(EU_data1$date20090202,na.rm = T, kernel = "gaussian"), type = "l", col= cols[4], lwd=4)
  lines(density(EU_data2$date20200602,na.rm = T, kernel = "gaussian"), type = "l", col= cols[5], lwd=4) 
  title(main = list(paste("KDE of daily lambda", channel2)), cex.main=1.5)
  #legend("topright", c("Europe on 20070801", "Europe on 20190603", "Europe on 20191203", "Europe on 20081003", "Europe on 20090202", "Europe on 20200602"), 
  #       fill = c("black", cols[1], cols[2], cols[3], cols[4], cols[5]), cex = 1.5)
}

dev.off()



#tmp
#overlaying plot: kernel density estimation with gaussian kernel, column 197 to 587 are recession period
png(paste0("Overlaying_lambda_distribution_recession", date_start1, "_", date_end1, "_", 
           channel, ".png"), width = 900, height = 600, bg = "transparent")
par(mfrow =c(2,2))
{plot(density(AM_data1[,197],na.rm = T, kernel = "gaussian"), type = "h", ylim = c(0,100), xlim=c(0,0.20),
      main = "", xlab = "estimated lambda")
for (i in 198:587) {
  lines(density(AM_data1[,i],na.rm = T, kernel = "gaussian"), type = "h") 
}
  title(sub = list(paste("Overlaying Lambda Distribution during recession period in", channel, "from", date_start1, "to",date_end1)))
}

{plot(density(AM_data1[,1],na.rm = T, kernel = "gaussian"), type = "h", ylim = c(0,100), xlim=c(0,0.20),
      main = "", xlab = "estimated lambda")
  for (i in c(2:196, 588:dim(AM_data2)[2])) {
    lines(density(AM_data1[,i],na.rm = T, kernel = "gaussian"), type = "h") 
  }
  title(sub = list(paste("Overlaying Lambda Distribution during normal period in", channel, "from", date_start1, "to",date_end1)))
}

{plot(density(AM_data2[,1],na.rm = T, kernel = "gaussian"), type = "h", ylim = c(0,100), xlim=c(0,0.20), 
     main = "", xlab = "estimated lambda")
for (i in 2:dim(AM_data2)[2]) {
  lines(density(AM_data2[,i],na.rm = T, kernel = "gaussian"), type = "h")  
}
  title(main  = paste("Overlaying Lambda Distribution in", channel, "from", date_start2, "to",date_end2))}

{plot(density(EU_data1[,1],na.rm = T, kernel = "gaussian"), type = "h", ylim = c(0,100), xlim=c(0,0.20), 
      main = "", xlab = "estimated lambda")
for (i in 2:dim(EU_data1)[2]) {
  lines(density(EU_data1[,i],na.rm = T, kernel = "gaussian"), type = "h")  
}
title(main = paste("Overlaying Lambda Distribution in", channel2, "from", date_start1, "to",date_end1))}

{plot(density(EU_data2[,1],na.rm = T, kernel = "gaussian"), type = "h", ylim = c(0,100), xlim=c(0,0.20), 
      main = "", xlab = "estimated lambda")
for (i in 2:dim(EU_data2)[2]) {
 lines(density(EU_data2[,i],na.rm = T, kernel = "gaussian"), type = "h")  
}
title(main = paste("Overlaying Lambda Distribution in", channel2, "from", date_start2, "to",date_end2))}
dev.off()


#during recession period
#overlaying plot: kernel density estimation with gaussian kernel
png(paste0("03_plots/Overlaying_lambda_distribution", date_start1, "_", date_end1, "_", 
           channel, ".png"), width = 1500, height = 1000, bg = "transparent")
par(mfrow =c(2,2))
{plot(density(AM_data1[,1],na.rm = T, kernel = "gaussian"), type = "h", ylim = c(0,100), xlim=c(0,0.20),
      main = "", xlab = "estimated lambda")
  for (i in 2:dim(AM_data1)[2]) {
    lines(density(AM_data1[,i],na.rm = T, kernel = "gaussian"), type = "h") 
  }
  title(main = list(paste("Overlaying Lambda Distribution in", channel, "from", date_start1, "to",date_end1)))
}

{plot(density(AM_data2[,1],na.rm = T, kernel = "gaussian"), type = "h", ylim = c(0,100), xlim=c(0,0.20), 
      main = "", xlab = "estimated lambda")
  for (i in 2:dim(AM_data2)[2]) {
    lines(density(AM_data2[,i],na.rm = T, kernel = "gaussian"), type = "h")  
  }
  title(main  = paste("Overlaying Lambda Distribution in", channel, "from", date_start2, "to",date_end2))}






##tmp##
{plot(density(AM_data1$date20070601,na.rm = T, kernel = "gaussian"), type = "l", ylim = c(0,70), xlim=c(0,0.20),
      main = "", xlab = "estimated labmda")
  lines(density(AM_data1$date20070801,na.rm = T, kernel = "gaussian"), type = "l", col= cols[1]) 
  lines(density(AM_data1$date20071001,na.rm = T, kernel = "gaussian"), type = "l", col= cols[2])
  lines(density(AM_data1$date20081003,na.rm = T, kernel = "gaussian"), type = "l", col= cols[3], lwd=2)
  lines(density(AM_data1$date20090202,na.rm = T, kernel = "gaussian"), type = "l", col= cols[4], lwd=2)
  lines(density(AM_data1$date20090602,na.rm = T, kernel = "gaussian"), type = "l", col= cols[5], lwd=2) 
  
  title(main = list(paste("Lambda Distribution from several moving windows in", channel)))
  legend("topright", c("Americas on 20070601", "Americas on 20070801", "Americas on 20071001", "Americas on 20081003", "Americas on 20090202", "Americas on 20090601"), 
         fill = c("black", cols[1], cols[2], cols[3], cols[4], cols[5]), cex = 0.5)
  text("")
}


  ll=seq(min(FRM_index_all$ticker), max(FRM_index_all$ticker), "quarter")
  axis.Date(1, at = ll, labels = ll, font=1, las=1)
  legend("topright", c("FRM_mean", "FRM_q50", "FRM_q60", "FRM_q70", "FRM_q80", "FRM_q90"), 
         fill = c("black", cols[1], cols[2], cols[3], cols[4], cols[5]), cex = 1)
  dev.off()

  #similar distribution
  lines(density(AM_data2$date20190107,na.rm = T, kernel = "gaussian"), type = "l", col= cols[2])
  lines(density(AM_data2$date20190603,na.rm = T, kernel = "gaussian"), type = "l", col= cols[1]) 
  lines(density(AM_data1$date20070702,na.rm = T, kernel = "gaussian"), type = "l", col= cols[2])

