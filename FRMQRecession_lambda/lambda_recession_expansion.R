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

####overlaying plot to compare lambdas during recession and expansion: kernel density estimation with gaussian kernel 
png(paste0("03_plots/lambda_distribution_recession_expansion", 
           channel, ".png"), width = 1500, height = 1000, bg = "transparent")
par(mfrow =c(2,2))
#comparing the overlaying plot during recession and expansion for Americas - financial crisis of 2008
#during expansion
{plot(density(AM_data1[,1],na.rm = T, kernel = "gaussian"), type = "h", ylim = c(0,100), xlim=c(0,0.20),
      main = "", xlab = "estimated lambda")
  for (i in c(2:196, 588:dim(AM_data1)[2])) {
    lines(density(AM_data1[,i],na.rm = T, kernel = "gaussian"), type = "h") 
  }}
#during recession
{plot(density(AM_data1[,197],na.rm = T, kernel = "gaussian"), type = "h", ylim = c(0,100), xlim=c(0,0.20),
      main = "", xlab = "estimated lambda", col="gray")
  for (i in 198:587) {
    lines(density(AM_data1[,i],na.rm = T, kernel = "gaussian"), type = "h", col="gray") 
  }}

#comparing the overlaying plot during recession and expansion for Americas - COVID-19 pandemic
#during expansion
{plot(density(AM_data2[,1],na.rm = T, kernel = "gaussian"), type = "h", ylim = c(0,100), xlim=c(0,0.20), 
      main = "", xlab = "estimated lambda")
  for (i in 2:305) {
    lines(density(AM_data2[,i],na.rm = T, kernel = "gaussian"), type = "h")  
  }}
#during recession
{plot(density(AM_data2[,306],na.rm = T, kernel = "gaussian"), type = "h", ylim = c(0,100), xlim=c(0,0.20), 
      main = "", xlab = "estimated lambda", col="gray")
  for (i in 307:dim(AM_data2)[2]) {
    lines(density(AM_data2[,i],na.rm = T, kernel = "gaussian"), type = "h", col="gray")  
  }}
dev.off()

png(paste0("03_plots/lambda_distribution_recession_expansion", 
           channel2, ".png"), width = 1500, height = 1000, bg = "transparent")
par(mfrow =c(2,2))
#comparing the overlaying plot during recession and expansion for Europe - financial crisis of 2008
#during expansion
{plot(density(EU_data1[,1],na.rm = T, kernel = "gaussian"), type = "h", ylim = c(0,100), xlim=c(0,0.20), 
      main = "", xlab = "estimated lambda")
  for (i in c(2:261,589:dim(EU_data1)[2])) {
    lines(density(EU_data1[,i],na.rm = T, kernel = "gaussian"), type = "h")  
  }}
#during recession
{plot(density(EU_data2[,262],na.rm = T, kernel = "gaussian"), type = "h", ylim = c(0,100), xlim=c(0,0.20), 
      main = "", xlab = "estimated lambda", col="gray")
  for (i in 263:588) {
    lines(density(EU_data1[,i],na.rm = T, kernel = "gaussian"), type = "h", col="gray")  
  }
  }

#during expansion
{plot(density(EU_data2[,1],na.rm = T, kernel = "gaussian"), type = "h", ylim = c(0,100), xlim=c(0,0.20), 
      main = "", xlab = "estimated lambda")
  for (i in 2:261) {
    lines(density(EU_data1[,i],na.rm = T, kernel = "gaussian"), type = "h")  
  }}
#during recession
{plot(density(EU_data2[,262],na.rm = T, kernel = "gaussian"), type = "h", ylim = c(0,100), xlim=c(0,0.20), 
      main = "", xlab = "estimated lambda", col="gray")
  for (i in 263:dim(EU_data2)[2]) {
    lines(density(EU_data2[,i],na.rm = T, kernel = "gaussian"), type = "h", col="gray") }}
dev.off()
