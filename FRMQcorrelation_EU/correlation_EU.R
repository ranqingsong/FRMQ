#process and plot other systemic risk measure

#clean environment
rm(list = ls(all = TRUE))
graphics.off()

setwd("~/studium/Masterarbeit_local/novel-FRM/FRMQcorrelation_EU")

# Install and load packages
libraries = c("xtable","RColorBrewer","dplyr")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

#settings 
date_start1 = 20070402
date_end1 = 20091231
date_start2 = 20190101
date_end2 = 20201231
channel2 = "Europe"

#scale data
scale_function = function(x){
  (x-min(x))/(max(x)-min(x))
}

##import FRM datasets
frm.data <- read.csv("FRM_all_EU.csv", 
                     colClasses = c("Date","numeric","numeric","numeric","numeric","numeric","numeric","numeric")) 

####import vstoxx data
v2x <- read.csv(paste0("V2X.csv")) %>% dplyr::select(ticker, V2X.INDEX)
v2x$ticker <- as.Date(v2x$ticker, "%Y-%m-%d")

#import weekly CISS 
ciss<- read.csv(paste0("CISS.csv"))
ciss$Date <- as.Date(ciss$Date, format = "%Y-%m-%d")
daily_ciss <- seq(ciss$Date[1], tail(ciss$Date,1), by="day")
ciss_daily <- data.frame(date=daily_ciss, ciss_d=spline(ciss, method = "fmm", xout = daily_ciss )$y)

#merge VSTOXX and CISS
comparison_data <- merge(v2x, ciss_daily, by.x = "ticker", by.y = "date")

# Compute financial turbulence
create_ft <- function(date_start1,date_end1, channel2){
  company_list <- colnames(read.csv(paste0("lambdas_wide_",date_start1,"-", date_end1,channel2,".csv")))[-1]
  
  frm.data.tmp = read.csv(paste0(channel2,"_Price_",date_end1,".csv")) 
  frm.data <- frm.data.tmp[, company_list]
  frm.data <- frm.data[, colSums(is.na(frm.data))==0]
  y            = data.matrix(frm.data)
  mu           = as.vector(apply(y, 2, mean))             # Mean for every company
  cov          = as.matrix(var(y))                        # Covariance matrix 
  tur          = numeric(0)
  for (i in 1:dim(y)[1]){
    tur[i] = as.vector(y[i, ] - mu) %*% solve(cov) %*% as.vector(t(y[i, ] - mu))
  }
  tur <- data.frame(tur)
  tur <- cbind(ticker =frm.data.tmp$ticker, tur)
  tur$ticker = as.Date(tur$ticker, "%Y-%m-%d") 
  return(tur)
}
ft_1<- create_ft(date_start1, date_end1,channel2)
ft_2<- create_ft(date_start2,date_end2,channel2)
ft_all <- rbind(ft_1, ft_2)

#merge all comparable systemic measures
comparison_data <- merge(comparison_data, ft_all, by.x = "ticker", by.y="ticker")

#merge comparable measures and FRM indices 
com_data_all <-merge(frm.data, comparison_data, by = "ticker")

#normalize all systemic risk measures
scaled_data <-data.frame(apply(subset(com_data_all, select=-c(ticker)), 2,scale_function),  date= com_data_all$ticker)
colnames(scaled_data)[c(8:10)] <- c("VSTOXX","CISS","FT")
write.csv(scaled_data, file = "systemic_risk_comparison_EU.csv", 
          row.names = FALSE, quote = FALSE)

#plot FRM, VIX, GT 
cols <- brewer.pal(9, "Set1")
datecut <- which(scaled_data$date==as.Date("2009-12-31","%Y-%m-%d"))
png(paste0("systemic_risk_indicators_",channel2,".png"), width = 1500, height = 1000, bg = "transparent")
par(mfrow =c(1,2))
{plot(scaled_data$date[1:datecut],scaled_data$FRM_index[1:datecut], type = "l", xlab = "", 
      ylab = "nomalized systemic risk indictors",ylim = c(0,1), lwd =3, xaxt="none",cex.axis=2, cex.lab=2)
  lines(scaled_data$date[1:datecut],scaled_data$FT[1:datecut], type = "l", col="violet", lwd =2)
  lines(scaled_data$date[1:datecut],scaled_data$VSTOXX[1:datecut], type = "l", col="red", lwd =2)
  lines(scaled_data$date[1:datecut],scaled_data$CISS[1:datecut], type = "l", col="darkblue", lwd =2)
  ll=seq(min(scaled_data$date), max(scaled_data$date), "quarter")
  axis.Date(1, at = ll, labels =ll , font=1, las=1, cex.axis= 1.5)
}

{plot(scaled_data$date[1:datecut],scaled_data$FRM_q50[1:datecut], type = "l", lty=1,xlab = "", ylab = "",
      ylim = c(0,1), cex.axis=2, cex.lab=2, xaxt="none",col = "darkred",lwd=2)
  lines(scaled_data$date[1:datecut],scaled_data$FRM_iqr[1:datecut], type = "l",lty=1, col=cols[7],lwd=2)
  lines(scaled_data$date[1:datecut],scaled_data$FRM_q80[1:datecut], type = "l",lty=1, col=cols[4],lwd=2)
  lines(scaled_data$date[1:datecut],scaled_data$FRM_q90[1:datecut], type = "l",lty=1, col=cols[5],lwd=2)
  lines(scaled_data$date[1:datecut],scaled_data$FRM_q60[1:datecut], type = "l",lty=1, col=cols[2],lwd=2)
  lines(scaled_data$date[1:datecut],scaled_data$FRM_q70[1:datecut], type = "l",lty=1, col=cols[3],lwd=2)
  lines(scaled_data$date[1:datecut],scaled_data$FRM_index[1:datecut], type = "l",lty=1, col="black",lwd=3)
  ll=seq(min(scaled_data$date), max(scaled_data$date), "quarter")
  axis.Date(1, at = ll, labels =ll , font=1, las=1, cex.axis= 1.5)
}

dev.off()


png(paste0("systemic_risk_indicators2_",channel2,".png"), width = 1500, height = 1000, bg = "transparent")
par(mfrow =c(1,2))
{plot(scaled_data$date[(datecut+1):length(scaled_data$date)],scaled_data$FRM_index[(datecut+1):length(scaled_data$date)], type = "l",ylim = c(0,1), 
      xlab = "", ylab = "nomalized systemic risk indictors", lwd =3, cex.axis=2,xaxt="none", cex.lab=2)
  lines(scaled_data$date[(datecut+1):length(scaled_data$date)],scaled_data$FT[(datecut+1):length(scaled_data$date)], type = "l", col="violet", lwd =2)
  lines(scaled_data$date[(datecut+1):length(scaled_data$date)],scaled_data$VSTOXX[(datecut+1):length(scaled_data$date)], type = "l", col="red", lwd =2)
  lines(scaled_data$date[(datecut+1):length(scaled_data$date)],scaled_data$CISS[(datecut+1):length(scaled_data$date)], type = "l", col="darkblue", lwd =2)
  ll=seq(min(scaled_data$date), max(scaled_data$date), "quarter")
  axis.Date(1, at = ll, labels =ll , font=1, las=1, cex.axis= 1.5)
}

{plot(scaled_data$date[(datecut+1):length(scaled_data$date)],scaled_data$FRM_q50[(datecut+1):length(scaled_data$date)], type = "l", lty=1,xlab = "", 
      ylab = "",ylim = c(0,1), xaxt="none",cex.axis=2, cex.lab=2, col ="darkred",lwd=2)
  lines(scaled_data$date[(datecut+1):length(scaled_data$date)],scaled_data$FRM_iqr[(datecut+1):length(scaled_data$date)], type = "l",lty=1, col=cols[7],lwd=2)
  lines(scaled_data$date[(datecut+1):length(scaled_data$date)],scaled_data$FRM_q80[(datecut+1):length(scaled_data$date)], type = "l",lty=1, col=cols[4],lwd=2)
  lines(scaled_data$date[(datecut+1):length(scaled_data$date)],scaled_data$FRM_q90[(datecut+1):length(scaled_data$date)], type = "l",lty=1, col=cols[5],lwd=2)
  lines(scaled_data$date[(datecut+1):length(scaled_data$date)],scaled_data$FRM_q60[(datecut+1):length(scaled_data$date)], type = "l",lty=1, col=cols[2],lwd=2)
  lines(scaled_data$date[(datecut+1):length(scaled_data$date)],scaled_data$FRM_q70[(datecut+1):length(scaled_data$date)], type = "l",lty=1, col=cols[3],lwd=2)
  lines(scaled_data$date[(datecut+1):length(scaled_data$date)],scaled_data$FRM_index[(datecut+1):length(scaled_data$date)], type = "l",lty=1, col="black",lwd=3)
  ll=seq(min(scaled_data$date), max(scaled_data$date), "quarter")
  axis.Date(1, at = ll, labels =ll , font=1, las=1, cex.axis= 1.5)
}
dev.off()

##correlation table--> VIX, GT and SRISK are correlated with all FRM indices
cortable <- cor(scaled_data[,1:7], scaled_data[,8:10], method = "pearson")

#pearson correlation test 
cor.test(scaled_data$FRM_q50, scaled_data$VSTOXX, method = "pearson")
cor.test(scaled_data$FRM_q50, scaled_data$CISS, method = "pearson")
cor.test(scaled_data$FRM_q50, scaled_data$FT, method = "pearson")

cor.test(scaled_data$FRM_q60, scaled_data$VSTOXX, method = "pearson")
cor.test(scaled_data$FRM_q60, scaled_data$CISS, method = "pearson")
cor.test(scaled_data$FRM_q60, scaled_data$FT, method = "pearson")

cor.test(scaled_data$FRM_q70, scaled_data$VSTOXX, method = "pearson")
cor.test(scaled_data$FRM_q70, scaled_data$CISS, method = "pearson")
cor.test(scaled_data$FRM_q70, scaled_data$FT, method = "pearson")

cor.test(scaled_data$FRM_q80, scaled_data$VSTOXX, method = "pearson")
cor.test(scaled_data$FRM_q80, scaled_data$CISS, method = "pearson")
cor.test(scaled_data$FRM_q80, scaled_data$FT, method = "pearson")

cor.test(scaled_data$FRM_q90, scaled_data$VSTOXX, method = "pearson")
cor.test(scaled_data$FRM_q90, scaled_data$CISS, method = "pearson")
cor.test(scaled_data$FRM_q90, scaled_data$FT, method = "pearson")

cor.test(scaled_data$FRM_iqr, scaled_data$VSTOXX, method = "pearson")
cor.test(scaled_data$FRM_iqr, scaled_data$CISS, method = "pearson")
cor.test(scaled_data$FRM_iqr, scaled_data$FT, method = "pearson")

cor.test(scaled_data$FRM_index, scaled_data$VSTOXX, method = "pearson")
cor.test(scaled_data$FRM_index, scaled_data$CISS, method = "pearson")
cor.test(scaled_data$FRM_index, scaled_data$FT, method = "pearson")

#KS test 
apply(scaled_data[,1:7], 2, function(x){
  ks.test(x, scaled_data$VSTOXX,alternative = "two.sided")})
apply(scaled_data[,1:7], 2, function(x){
  ks.test(x, scaled_data$CISS,alternative = "two.sided")})
apply(scaled_data[,1:7], 2, function(x){
  ks.test(x, scaled_data$FT,alternative = "two.sided")})

