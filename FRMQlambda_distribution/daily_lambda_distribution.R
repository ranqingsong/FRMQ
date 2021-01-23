####Creating Plots of lambda distribution for each moving window####

#clean environment
rm(list = ls(all = TRUE))
setwd("~/Dropbox/Masterarbeit/02_project/FRM_All/03_Quantlets")
setwd("C:/Users/srq04/Dropbox/Masterarbeit/02_project/FRM_All/03_Quantlets")

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
date_end2 = 20201231
channel = "Americas"
channel2 = "Europe"

#import csv files containing daily lambda
#plot daily lambda distribution from kernel density estimation
data_input <- function(date_start, date_end, channel){
  lambdas_wide <- read.csv(paste0("01_csvData/lambdas_wide_",date_start,"-",date_end, channel,".csv"))
  lambdas_wide_t <- as.data.frame(t(as.matrix(lambdas_wide)))
  lambdas_wide_t<- sapply(1:ncol(lambdas_wide_t),function(i) as.numeric(lambdas_wide_t[,i]))
  rownames(lambdas_wide_t) <- colnames(lambdas_wide)
  colnames(lambdas_wide_t) <- paste0("date",lambdas_wide[,1])
  lambdas_wide_t <- lambdas_wide_t[-c(1),]
  #lambdas_wide_t2 <- data.frame(lambdas_wide_t)
  return(data.frame(lambdas_wide_t))
  }

AM_data1 <- data_input(date_start1, date_end1, channel)
AM_data2 <- data_input(date_start2, date_end2, channel)
EU_data1 <- data_input(date_start1, date_end1, channel2)
EU_data2<-  data_input(date_start2, date_end2, channel2)

#plot kernel density estimation of several lambda series
cols <- brewer.pal(9, "Set1")

png(paste0("03_plots/lambda_distribution_AM_EU.png"), width = 1500, height = 1000, bg = "transparent")
par(mfrow =c(1,2))
{plot(density(AM_data1$date20070801,na.rm = T, kernel = "gaussian"), type = "l", ylim = c(0,70), xlim=c(0,0.20),
      main = "", xlab = "estimated lambda", cex.lab=1.5, cex.axis=1.5, lwd=4)
  lines(density(AM_data2$date20190603,na.rm = T, kernel = "gaussian"), type = "l", col= cols[1], lwd=3) 
  lines(density(AM_data2$date20191203,na.rm = T, kernel = "gaussian"), type = "l", col= cols[2], lwd=3)
  lines(density(AM_data1$date20081003,na.rm = T, kernel = "gaussian"), type = "l", col= cols[3], lwd=4)
  lines(density(AM_data1$date20090202,na.rm = T, kernel = "gaussian"), type = "l", col= cols[4], lwd=4)
  lines(density(AM_data2$date20200602,na.rm = T, kernel = "gaussian"), type = "l", col= cols[5], lwd=4) 
  title(main = list(paste("KDE of daily lambda in", channel)), cex.main=1.5)
}

{plot(density(EU_data1$date20070801,na.rm = T, kernel = "gaussian"), type = "l", ylim = c(0,70), xlim=c(0,0.20),
      main = "", xlab = "estimated lambda", cex.lab=1.5, cex.axis=1.5, lwd=3)
  lines(density(EU_data2$date20190603,na.rm = T, kernel = "gaussian"), type = "l", col= cols[1], lwd=3) 
  lines(density(EU_data2$date20191203,na.rm = T, kernel = "gaussian"), type = "l", col= cols[2], lwd=3)
  lines(density(EU_data1$date20081003,na.rm = T, kernel = "gaussian"), type = "l", col= cols[3], lwd=4)
  lines(density(EU_data1$date20090202,na.rm = T, kernel = "gaussian"), type = "l", col= cols[4], lwd=4)
  lines(density(EU_data2$date20200602,na.rm = T, kernel = "gaussian"), type = "l", col= cols[5], lwd=4) 
  title(main = list(paste("KDE of daily lambda", channel2)), cex.main=1.5)
}

dev.off()
