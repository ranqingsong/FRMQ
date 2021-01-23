##Creating density distribution of standard FRM index through kernel density estimation## 
#clean environment
rm(list = ls(all = TRUE))
setwd("~/Dropbox/Masterarbeit/02_project/FRM_All/03_Quantlets")

#libraries = c("magick", "texreg", "zoo","pscl", "plotly", "lubridate","tidyr", "dplyr","graphics",
#              "stringr", "timeDate", "igraph", "data.table", "ggplot2","kableExtra","webshot","stargazer")
libraries = c("RColorBrewer")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

#settings 
date_start1 = 20070402
date_end1 = 20091231
date_start2 = 20190101
date_end2 = 20201026
channel = "Americas"
channel2 = "Europe"

#input datasets
inputcsv <- function(date_start, date_end, channel){
  read.csv(paste0("01_csvData/FRM_index_all",date_start,"-",date_end, channel,".csv"), 
           colClasses = c( "numeric","numeric","numeric","numeric","numeric","numeric","numeric",
                          "numeric","numeric","numeric","Date")) 
} 
AM_data1<- inputcsv(date_start1, date_end1, channel)
AM_data2<- inputcsv(date_start2, date_end2, channel)
EU_data1<- inputcsv(date_start1, date_end1, channel2)
EU_data2<- inputcsv(date_start2, date_end2, channel2)

#create plots
png(paste0("03_plots/FRM_index_distribution_AM_EU.png"), width = 1500, height = 1000, bg = "transparent")
par(mfrow =c(1,2))
{plot(density(EU_data1$FRM_index, kernel = "gaussian"),main="",sub = paste("KDE of FRM_mean from", date_start1, "to",date_end1), 
      xlab = "", cex.axis=1.5, cex.lab=1.5, lwd=2,cex.sub=1.5)
lines(density(AM_data1$FRM_index, kernel = "gaussian"), col="blue", lwd=2)
legend("topright", c("FRM_mean@Americas", "FRM_mean@Europe"), fill = c("black", "blue"),cex=1.5)}
{plot(density(EU_data2$FRM_index, kernel = "gaussian"), main="",sub = paste("KDE of FRM_mean from", date_start2, "to",date_end2),
      xlab = "", cex.axis=1.5, cex.lab=1.5, lwd=2,cex.sub=1.5)
lines(density(AM_data2$FRM_index, kernel = "gaussian"), col="blue", lwd=2)}
dev.off()

#create plots for FRM_q90
png(paste0("03_plots/FRM_q90_distribution_AM_EU.png"), width = 1500, height = 1000, bg = "transparent")
par(mfrow =c(1,2))
{plot(density(EU_data1$FRM_q90, kernel = "gaussian"), main="",sub = paste("KDE of FRM_q90 from", date_start1, "to",date_end1), 
      xlab = "", cex.axis=1.5, cex.lab=1.5, lwd=2,cex.sub=1.5)
  lines(density(AM_data1$FRM_q90, kernel = "gaussian"), col="blue", lwd=2)
  legend("topright", c("FRM_q90@Americas", "FRM_q90@Europe"), fill = c("black", "blue"),cex=1.5)}
{plot(density(EU_data2$FRM_q90, kernel = "biweight"), main="",sub = paste("KDE of standard FRM_q90 from", date_start2, "to",date_end2),
      xlab = "", cex.axis=1.5, cex.lab=1.5, lwd=2,cex.sub=1.5)
  lines(density(AM_data2$FRM_q90, kernel = "gaussian"), col="blue", lwd=2)}
dev.off()

#create plots for FRM_mean during recession and normal periods
png(paste0("03_plots/FRM_mean_distribution_rec_exp_AM_EU",date_start1,".png"),width = 900, height = 600, bg = "transparent")
par(mfrow =c(2,2))
{plot(density(EU_data1[262:587,]$FRM_index, kernel = "gaussian"), main="",sub = paste("KDE of FRM_mean during recession from", date_start1, "to",date_end1), 
      xlab = "FRM_mean")
  lines(density(AM_data1[197:587,]$FRM_index, kernel = "gaussian"), col="blue")}
{plot(density(EU_data1[c(1:261,588:719),]$FRM_index, kernel = "gaussian"), ylim=c(0,90), main="",sub = paste("KDE of FRM_mean from during expansion", date_start1, "to",date_end1),
      xlab = "FRM_mean")
  lines(density(AM_data1[c(1:296,588:719),]$FRM_index, kernel = "gaussian"), col="blue")
  legend("topright", c("FRM_mean@Americas", "FRM_mean@Europe"), fill = c("black", "blue"))}
{plot(density(EU_data2[262:475,]$FRM_index, kernel = "gaussian"), main="",sub = paste("KDE of FRM_mean during recession from", date_start2, "to",date_end2), 
      xlab = "FRM_mean")
  lines(density(AM_data2[305:475,]$FRM_index, kernel = "gaussian"), col="blue")}
{plot(density(EU_data2[c(1:261),]$FRM_index, kernel = "gaussian"), ylim=c(0,200), main="",sub = paste("KDE of FRM_mean from during expansion", date_start2, "to",date_end2),
      xlab = "FRM_mean")
  lines(density(AM_data2[c(1:304),]$FRM_index, kernel = "gaussian"), col="blue")}
dev.off()


