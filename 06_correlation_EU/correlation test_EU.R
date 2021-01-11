#Correlation comparison

#clean environment
rm(list = ls(all = TRUE))
setwd("~/Dropbox/Masterarbeit/02_project/FRM_All/03_Quantlets")
setwd("C:/Users/srq04/Dropbox/Masterarbeit/02_project/FRM_All/03_Quantlets")

#libraries = c("magick", "texreg", "zoo","pscl", "plotly", "lubridate","tidyr", "dplyr","graphics",
#              "stringr", "timeDate", "igraph", "data.table", "ggplot2","kableExtra","webshot","stargazer")
libraries = c("xtable","RColorBrewer")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

#settings 
date_start1 = 20070402
date_end1 = 20091231
date_start2 = 20190101
date_end2 = 20201026
channel2 = "Europe"

#scale data
scale_function = function(x){
  (x-min(x))/(max(x)-min(x))
}

##import FRM datasets
EU_data1<- read.csv(paste0("01_csvData/FRM_index_all",date_start1,"-",date_end1, channel2,".csv"), 
                    colClasses = c("Date","numeric","numeric","numeric","numeric","numeric","numeric","numeric")) 
EU_data2<- read.csv(paste0("01_csvData/FRM_index_all",date_start2,"-",date_end2, channel2,".csv"), 
                    colClasses = c("Date","numeric","numeric","numeric","numeric","numeric","numeric","numeric")) 
FRM_data <- rbind(EU_data1, EU_data2)
####import vix and CISS data
V2X_1 <- read.csv(paste0("01_csvData/",channel2,"_Macro_",date_end1,".csv")) %>% dplyr::select(ticker, V2X.INDEX)
V2X_2 <- read.csv(paste0("01_csvData/",channel2,"_Macro_",date_end2,".csv")) %>% dplyr::select(ticker, V2X.INDEX)
V2X = rbind(V2X_1, V2X_2)
V2X$ticker <- as.Date(V2X$ticker, "%Y-%m-%d")

#weekly CISS 
CISS <- read.csv(paste0("01_csvData/CISS.csv"))
CISS$Date <- as.Date(CISS$Date, format = "%d/%m/%Y")
CISS <- subset(CISS, Date > as.Date("20070301", "%Y%m%d"))
CISS <- CISS[order(CISS$Date),]
daily_ciss <- seq(CISS$Date[1], tail(CISS$Date,1), by="day")
CISS_daily <- data.frame(date=daily_ciss, CISS_d=spline(CISS, method = "fmm", xout = daily_ciss )$y)

comparison_data <- merge(V2X, CISS_daily, by.x = "ticker", by.y = "date")
#merge comparison indices and FRM indices 
com_data_all <-merge(FRM_data, comparison_data, by = "ticker")
scaled_data <-data.frame(apply(subset(com_data_all, select=-c(ticker)), 2,scale_function),  date= com_data_all$ticker)
colnames(scaled_data)[c(8, 9)] <- c("VSTOXX","CISS")
write.csv(scaled_data, file = "01_csvData/systemic_risk_comparison_EU.csv", 
          row.names = FALSE, quote = FALSE)
##correlation --> VIX, GT and SRISK are correlated with all FRM indices
cortable <- cor(scaled_data[,1:7], scaled_data[,8:9], method = "pearson")
print(xtable(cortable, "Correlation of FRM and systemic risk indicators in Europe", type="latex"), file = "04_tables/Correlation_EU.tex")

apply(scaled_data[,1:7], 2, function(x){
  ks.test(x, scaled_data$VSTOXX,alternative = "two.sided")})
apply(scaled_data[,1:7], 2, function(x){
  ks.test(x, scaled_data$CISS,alternative = "two.sided")})

#significant
cor.test(scaled_data$FRM_q50, scaled_data$VSTOXX, method = "pearson")
cor.test(scaled_data$FRM_q50, scaled_data$CISS, method = "pearson")
cor.test(scaled_data$FRM_q60, scaled_data$VSTOXX, method = "pearson")
cor.test(scaled_data$FRM_q60, scaled_data$CISS, method = "pearson")
cor.test(scaled_data$FRM_q70, scaled_data$VSTOXX, method = "pearson")
cor.test(scaled_data$FRM_q70, scaled_data$CISS, method = "pearson")
cor.test(scaled_data$FRM_q80, scaled_data$VSTOXX, method = "pearson")
cor.test(scaled_data$FRM_q80, scaled_data$CISS, method = "pearson")

cor.test(scaled_data$FRM_q90, scaled_data$VSTOXX, method = "pearson")
cor.test(scaled_data$FRM_q90, scaled_data$CISS, method = "pearson")
cor.test(scaled_data$FRM_iqr, scaled_data$VSTOXX, method = "pearson")
cor.test(scaled_data$FRM_iqr, scaled_data$CISS, method = "pearson")
cor.test(scaled_data$FRM_index, scaled_data$VSTOXX, method = "pearson")
cor.test(scaled_data$FRM_index, scaled_data$CISS, method = "pearson")


#plot FRM, VIX, GT 
cols <- brewer.pal(9, "Set1")
datecut <- which(scaled_data$date==as.Date("2009-12-31","%Y-%m-%d"))
png(paste0("03_plots/systemic_risk_indicators_",channel2,".png"), width = 1500, height = 1000, bg = "transparent")
par(mfrow =c(1,2))
{plot(scaled_data$date[1:datecut],scaled_data$FRM_index[1:datecut], type = "l", xlab = "", 
      ylab = "nomalized systemic risk indictors",ylim = c(0,1), lwd =3, cex.axis=2, cex.lab=2)
lines(scaled_data$date[1:datecut],scaled_data$VSTOXX[1:datecut], type = "l", col="red", lwd =2)
lines(scaled_data$date[1:datecut],scaled_data$CISS[1:datecut], type = "l", col="blue", lwd =2)
#legend("topleft", c("FRM_mean", "VSTOXX", "CISS"), fill=c("black", "red", "blue"), cex=2)
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


png(paste0("03_plots/systemic_risk_indicators2_",channel2,".png"), width = 1500, height = 1000, bg = "transparent")
par(mfrow =c(1,2))
{plot(scaled_data$date[(datecut+1):length(scaled_data$date)],scaled_data$FRM_index[(datecut+1):length(scaled_data$date)], type = "l",ylim = c(0,1), 
      xlab = "", ylab = "", lwd =3, cex.axis=2, cex.lab=2)
  lines(scaled_data$date[(datecut+1):length(scaled_data$date)],scaled_data$VSTOXX[(datecut+1):length(scaled_data$date)], type = "l", col="red", lwd =2)
  lines(scaled_data$date[(datecut+1):length(scaled_data$date)],scaled_data$CISS[(datecut+1):length(scaled_data$date)], type = "l", col="blue", lwd =2)
  #legend("topleft", c("FRM_mean", "VSTOXX", "CISS"), fill=c("black", "red", "blue"), cex=2)
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

