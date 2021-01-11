#Correlation comparison

#clean environment
rm(list = ls(all = TRUE))
#setwd("~/Dropbox/Masterarbeit/02_project/FRM_All/03_Quantlets")
setwd("C:/Users/srq04/Dropbox/Masterarbeit/02_project/FRM_All/03_Quantlets")
#libraries = c("magick", "texreg", "zoo","pscl", "plotly", "lubridate","tidyr", "dplyr","graphics",
#              "stringr", "timeDate", "igraph", "data.table", "ggplot2","kableExtra","webshot","stargazer")
libraries = c("gtrendsR","xtable","RColorBrewer")
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

#scale data
scale_function = function(x){
  (x-min(x))/(max(x)-min(x))
}

##import FRM datasets
AM_data1<- read.csv(paste0("01_csvData/FRM_index_all",date_start1,"-",date_end1, channel,".csv"), 
                    colClasses = c("Date","numeric","numeric","numeric","numeric","numeric","numeric","numeric")) 

AM_data2<- read.csv(paste0("01_csvData/FRM_index_all",date_start2,"-",date_end2, channel,".csv"), 
                    colClasses = c("Date","numeric","numeric","numeric","numeric","numeric","numeric","numeric")) 

FRM_data <- rbind(AM_data1, AM_data2)
####import google trend data and vix
VIX_1 <- read.csv(paste0("01_csvData/",channel,"_Macro_",date_end1,".csv")) %>% dplyr::select(ticker, VIX.INDEX)
VIX_2 <- read.csv(paste0("01_csvData/",channel,"_Macro_",date_end2,".csv")) %>% dplyr::select(ticker, VIX.INDEX)
VIX = rbind(VIX_1, VIX_2)
VIX$ticker <- as.Date(VIX$ticker, "%Y-%m-%d")

#using cubic interpolation to transform weekly data into daily
GT_input_1 <- gtrendsR::gtrends(keyword = "financial crisis", geo = c("US","CA"),time = ("2007-03-20 2010-01-05"), onlyInterest = T)$interest_over_time
GT_input_2 <- gtrendsR::gtrends(keyword = "financial crisis", geo = c("US","CA"),time = ("2018-01-01 2020-11-01"), onlyInterest = T)$interest_over_time
create_dailyGT <- function(GT_input_1){
  GT <- aggregate(hits ~ date, GT_input_1, sum)
  GT$date <- as.Date(as.POSIXct(GT$date))
  daily <- seq(GT$date[1], tail(GT$date,1), by="day")
  GT_d=spline(GT, method = "fmm", xout = daily)$y
  GT_daily <- data.frame(date=daily, GT_d=scale_function(GT_d))
  return(GT_daily)
}
GT_d1<- create_dailyGT(GT_input_1)
GT_d2<- create_dailyGT(GT_input_2)
GT_daily <- rbind(GT_d1, GT_d2)
comparison_data <- merge(VIX, GT_daily, by.x = "ticker", by.y = "date")

#monthly SRISK data with selected countries: Canada and United States; data source:https://vlab.stern.nyu.edu/welcome/srisk
SRISK_1 <- read.csv(paste0("01_csvData/SRISK.csv"))
SRISK_2 <- read.csv(paste0("01_csvData/SRISK_2.csv"))
colnames(SRISK_2) <- c("Month", "SRISK")
create_dailySRISK <- function(SRISK_1){
  SRISK <- SRISK_1
  SRISK$Month <- as.Date(paste0(SRISK$Month,"-01"), format = "%Y-%m-%d")
  daily_srisk <- seq(SRISK$Month[1], tail(SRISK$Month,1), by="day")
  SRISK_daily <- data.frame(date=daily_srisk, SRISK_d=spline(SRISK, method = "fmm", xout = daily_srisk)$y)
}
SRISK_d1 <- create_dailySRISK(SRISK_1)
SRISK_d2 <- create_dailySRISK(SRISK_2)
SRISK_daily <- rbind(SRISK_d1, SRISK_d2)
comparison_data <- merge(comparison_data, SRISK_daily, by.x = "ticker", by.y="date")

#merge comparison indices and FRM indices 
com_data_all <-merge(FRM_data, comparison_data, by = "ticker")
scaled_data <-data.frame(apply(subset(com_data_all, select=-c(ticker, GT_d)), 2,scale_function), GT_d = com_data_all$GT_d, date= com_data_all$ticker)
colnames(scaled_data)[8:10] <- c("VIX","SRISK", "GT")
write.csv(scaled_data, file = "01_csvData/systemic_risk_comparison_AM.csv", 
          row.names = FALSE, quote = FALSE)


##kolmogorov smirnov test of FRMs and common systemic risk indicators
apply(scaled_data[,1:7], 2, function(x){
  ks.test(x, scaled_data$VIX,alternative = "two.sided")})
apply(scaled_data[,1:7], 2, function(x){
  ks.test(x, scaled_data$SRISK,alternative = "two.sided")})
apply(scaled_data[,1:7], 2, function(x){
  ks.test(x, scaled_data$GT,alternative = "two.sided")})

##correlation --> VIX, GT and SRISK are correlated with all FRM indices
cortable <- cor(scaled_data[,c(1:7)], scaled_data[,8:10], method = "pearson")
print(xtable(cortable, "Correlation of FRM and systemic risk indicators in Americas", type="latex"), file = "04_tables/Correlation_AM.tex")
#significant
cor.test(scaled_data$FRM_index, scaled_data$VIX, method = "pearson")
cor.test(scaled_data$FRM_index, scaled_data$GT, method = "pearson")
cor.test(scaled_data$FRM_index, scaled_data$SRISK, method = "pearson")

cor.test(scaled_data$FRM_q50, scaled_data$VIX, method = "pearson")
cor.test(scaled_data$FRM_q50, scaled_data$GT, method = "pearson")
cor.test(scaled_data$FRM_q50, scaled_data$SRISK, method = "pearson")

cor.test(scaled_data$FRM_q60, scaled_data$VIX, method = "pearson")
cor.test(scaled_data$FRM_q60, scaled_data$GT, method = "pearson")
cor.test(scaled_data$FRM_q60, scaled_data$SRISK, method = "pearson")

cor.test(scaled_data$FRM_q70, scaled_data$VIX, method = "pearson")
cor.test(scaled_data$FRM_q70, scaled_data$GT, method = "pearson")
cor.test(scaled_data$FRM_q70, scaled_data$SRISK, method = "pearson")

cor.test(scaled_data$FRM_q80, scaled_data$VIX, method = "pearson")
cor.test(scaled_data$FRM_q80, scaled_data$GT, method = "pearson")
cor.test(scaled_data$FRM_q80, scaled_data$SRISK, method = "pearson")

cor.test(scaled_data$FRM_q90, scaled_data$VIX, method = "pearson")
cor.test(scaled_data$FRM_q90, scaled_data$GT, method = "pearson")
cor.test(scaled_data$FRM_q90, scaled_data$SRISK, method = "pearson")

cor.test(scaled_data$FRM_iqr, scaled_data$VIX, method = "pearson")
cor.test(scaled_data$FRM_iqr, scaled_data$GT, method = "pearson")
cor.test(scaled_data$FRM_iqr, scaled_data$SRISK, method = "pearson")

#plot FRM, VIX, GT 
cols <- brewer.pal(9, "Set1")
datecut <- which(scaled_data$date==as.Date("2009-12-31","%Y-%m-%d"))
png(paste0("03_plots/systemic_risk_indicators_",channel,".png"), width = 1500, height = 1000, bg = "transparent")
par(mfrow =c(1,2))
{plot(scaled_data$date[1:datecut],scaled_data$GT[1:datecut], type = "l", xlab = "",col="blue", 
      ylab = "nomalized systemic risk indictors",ylim = c(0,1), lwd =3, cex.axis=2, cex.lab=2)
lines(scaled_data$date[1:datecut],scaled_data$SRISK[1:datecut], type = "l", col="darkgreen", lwd =2)
lines(scaled_data$date[1:datecut],scaled_data$VIX[1:datecut], type = "l", col="red", lwd =2)
lines(scaled_data$date[1:datecut],scaled_data$FRM_index[1:datecut], type = "l", col="black", lwd =2)
#legend("topleft", c("FRM_mean", "VIX", "SRISK", "GT"), fill=c("black", "red","darkgreen", "blue"),cex=2)
}

{plot(scaled_data$date[1:datecut],scaled_data$FRM_q50[1:datecut], type = "l", lty=1,xlab = "", ylab = "nomalized systemic risk indictors",
      ylim = c(0,1), cex.axis=2, cex.lab=2, col = cols[1],lwd=2)
  lines(scaled_data$date[1:datecut],scaled_data$FRM_iqr[1:datecut], type = "l",lty=1, col=cols[7],lwd=2)
  lines(scaled_data$date[1:datecut],scaled_data$FRM_q80[1:datecut], type = "l",lty=1, col=cols[4],lwd=2)
  lines(scaled_data$date[1:datecut],scaled_data$FRM_q90[1:datecut], type = "l",lty=1, col=cols[5],lwd=2)
  lines(scaled_data$date[1:datecut],scaled_data$FRM_q60[1:datecut], type = "l",lty=1, col=cols[2],lwd=2)
  lines(scaled_data$date[1:datecut],scaled_data$FRM_q70[1:datecut], type = "l",lty=1, col=cols[3],lwd=2)
  lines(scaled_data$date[1:datecut],scaled_data$FRM_index[1:datecut], type = "l",lty=1, col="black",lwd=3)
  #legend("topleft", c("FRM_mean", "FRM_q50", "FRM_q60", "FRM_q70", "FRM_q80", "FRM_q90","FRM_IQR"), 
  #       fill = c("black", cols[1], cols[2], cols[3], cols[4], cols[5],cols[7]), cex = 2)
}
dev.off()


png(paste0("03_plots/systemic_risk_indicators2_",channel,".png"), width = 1500, height = 1000, bg = "transparent")
par(mfrow =c(1,2))

{plot(scaled_data$date[(datecut+1):length(scaled_data$date)],scaled_data$GT[(datecut+1):length(scaled_data$date)], lwd =3, type = "l",
      ylim = c(0,1), xlab = "", ylab = "", cex.axis=2, cex.lab=2, col="blue")
  lines(scaled_data$date[(datecut+1):length(scaled_data$date)],scaled_data$SRISK[(datecut+1):length(scaled_data$date)], type = "l", col="darkgreen", lwd =3)
  lines(scaled_data$date[(datecut+1):length(scaled_data$date)],scaled_data$VIX[(datecut+1):length(scaled_data$date)], type = "l", col="red", lwd =3)
  lines(scaled_data$date[(datecut+1):length(scaled_data$date)],scaled_data$FRM_index[(datecut+1):length(scaled_data$date)], type = "l", col="black", lwd =3)
  #legend("topleft", c("FRM_mean", "VIX", "SRISK", "GT"), fill=c("black", "red","darkgreen", "blue"),cex=2)
}
{plot(scaled_data$date[(datecut+1):length(scaled_data$date)],scaled_data$FRM_q50[(datecut+1):length(scaled_data$date)], type = "l", lty=1,xlab = "", 
      ylab = "nomalized systemic risk indictors",ylim = c(0,1), cex.axis=2, cex.lab=2, col = cols[1],lwd=2)
  lines(scaled_data$date[(datecut+1):length(scaled_data$date)],scaled_data$FRM_iqr[(datecut+1):length(scaled_data$date)], type = "l",lty=1, col=cols[7],lwd=2)
  lines(scaled_data$date[(datecut+1):length(scaled_data$date)],scaled_data$FRM_q80[(datecut+1):length(scaled_data$date)], type = "l",lty=1, col=cols[4],lwd=2)
  lines(scaled_data$date[(datecut+1):length(scaled_data$date)],scaled_data$FRM_q90[(datecut+1):length(scaled_data$date)], type = "l",lty=1, col=cols[5],lwd=2)
  lines(scaled_data$date[(datecut+1):length(scaled_data$date)],scaled_data$FRM_q60[(datecut+1):length(scaled_data$date)], type = "l",lty=1, col=cols[2],lwd=2)
  lines(scaled_data$date[(datecut+1):length(scaled_data$date)],scaled_data$FRM_q70[(datecut+1):length(scaled_data$date)], type = "l",lty=1, col=cols[3],lwd=2)
  lines(scaled_data$date[(datecut+1):length(scaled_data$date)],scaled_data$FRM_index[(datecut+1):length(scaled_data$date)], type = "l",lty=1, col="black",lwd=3)
  #legend("topleft", c("FRM_mean", "FRM_q50", "FRM_q60", "FRM_q70", "FRM_q80", "FRM_q90","FRM_IQR"), 
  #       fill = c("black", cols[1], cols[2], cols[3], cols[4], cols[5],cols[7]), cex = 2)
  }
dev.off()

