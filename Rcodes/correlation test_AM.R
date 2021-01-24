#Correlation comparison

#clean environment
rm(list = ls(all = TRUE))

# Set working directory
# setwd("")

# Install and load packages
libraries = c("gtrendsR","xtable","RColorBrewer","dplyr")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

#settings 
date_start1 = 20070402
date_end1 = 20091231
date_start2 = 20190101
date_end2 = 20201231
channel = "Americas"

#scale data
scale_function = function(x){
  (x-min(x))/(max(x)-min(x))
}

##import FRM datasets
frm_data = read.csv("FRM_all_AM.csv", 
                     colClasses = c("Date","numeric","numeric","numeric","numeric","numeric","numeric","numeric")) 

####import vix data
vix = read.csv("VIX.csv")
vix$ticker = as.Date(vix$ticker, "%Y-%m-%d")

#using cubic interpolation to transform weekly GT data into daily
gt_input1 = gtrendsR::gtrends(keyword = "financial crisis", geo = c("US","CA"),time = ("2007-03-20 2010-01-05"), onlyInterest = T)$interest_over_time
gt_input2 = gtrendsR::gtrends(keyword = "financial crisis", geo = c("US","CA"),time = ("2018-01-01 2021-01-05"), onlyInterest = T)$interest_over_time
create_dailyGT = function(gt_input1){
  gt = aggregate(hits ~ date, gt_input1, sum)
  gt$date = as.Date(as.POSIXct(gt$date))
  daily = seq(gt$date[1], tail(gt$date,1), by="day")
  gt_d=spline(gt, method = "fmm", xout = daily)$y
  gt_daily = data.frame(date=daily, gt_d=scale_function(gt_d))
  return(gt_daily)
}
gt_d1= create_dailyGT(gt_input1)
gt_d2= create_dailyGT(gt_input2)
gt_daily = rbind(gt_d1, gt_d2)
comparison_data = merge(vix, gt_daily, by.x = "ticker", by.y = "date")

#monthly SRISK data with selected countries: Canada and United States
#data source:https://vlab.stern.nyu.edu/welcome/srisk
srisk1 = read.csv(paste0("SRISK.csv"))
srisk2 = read.csv(paste0("SRISK_2.csv"))
colnames(srisk2) = c("Month", "SRISK")
create_dailySRISK = function(srisk1){
  SRISK = srisk1
  SRISK$Month = as.Date(paste0(SRISK$Month,"-01"), format = "%Y-%m-%d")
  daily_srisk = seq(SRISK$Month[1], tail(SRISK$Month,1), by="day")
  SRISK_daily = data.frame(date=daily_srisk, SRISK=spline(SRISK, method = "fmm", xout = daily_srisk)$y)
}
srisk_d1 = create_dailySRISK(srisk1)
srisk_d2 = create_dailySRISK(srisk2)
srisk = rbind(srisk_d1, srisk_d2)
comparison_data = merge(comparison_data, srisk, by.x = "ticker", by.y="date")

# Compute financial turbulence
create_ft = function(date_start1,date_end1, channel){
  company_list = colnames(read.csv(paste0("lambdas_wide_",date_start1,"-", date_end1,channel,".csv")))[-1]
  frm.data.tmp = read.csv(paste0(channel,"_Price_",date_end1,".csv")) 
  frm.data = frm.data.tmp[, company_list]
  frm.data = frm.data[, colSums(is.na(frm.data))==0]
  w.daily=63                                       # Lenth of the moving window
  y=data.matrix(frm.data)
  mu=as.vector(apply(y, 2, mean))             # Mean for every company
  cov=as.matrix(var(y))                        # Covariance matrix 
  tur=numeric(0)
  for (i in 1:dim(y)[1]){
    tur[i] = as.vector(y[i, ] - mu) %*% solve(cov) %*% as.vector(t(y[i, ] - mu))
  }
  tur=data.frame(tur)
  tur = cbind(ticker =frm.data.tmp$ticker, tur)
  tur$ticker = as.Date(tur$ticker, "%Y-%m-%d") 
  return(tur)
}
ft_1= create_ft(date_start1,date_end1,channel)
ft_2= create_ft(date_start2,date_end2,channel)
ft_all = rbind(ft_1, ft_2)

comparison_data =merge(comparison_data, ft_all, by.x = "ticker", by.y="ticker")

#merge comparison indices and FRM indices 
com_data_all =merge(frm_data, comparison_data, by = "ticker")
scaled_data =data.frame(apply(subset(com_data_all, select=-c(ticker, gt_d)), 2,scale_function),GT= com_data_all$gt_d,date= com_data_all$ticker)
colnames(scaled_data)[8:11] = c("VIX","SRISK", "FT","GT")
write.csv(scaled_data, file = "systemic_risk_comparison_AM.csv", 
          row.names = FALSE, quote = FALSE)

##correlation --> VIX, GT, FT and SRISK are correlated with all FRM indices
cor(scaled_data[,c(1:7)], scaled_data[,8:11], method = "pearson")

#correlation test
cor.test(scaled_data$FRM_index, scaled_data$VIX, method = "pearson")
cor.test(scaled_data$FRM_index, scaled_data$GT, method = "pearson")
cor.test(scaled_data$FRM_index, scaled_data$SRISK, method = "pearson")
cor.test(scaled_data$FRM_index, scaled_data$FT, method = "pearson")

cor.test(scaled_data$FRM_q50, scaled_data$VIX, method = "pearson")
cor.test(scaled_data$FRM_q50, scaled_data$GT, method = "pearson")
cor.test(scaled_data$FRM_q50, scaled_data$SRISK, method = "pearson")
cor.test(scaled_data$FRM_q50, scaled_data$FT, method = "pearson")

cor.test(scaled_data$FRM_q60, scaled_data$VIX, method = "pearson")
cor.test(scaled_data$FRM_q60, scaled_data$GT, method = "pearson")
cor.test(scaled_data$FRM_q60, scaled_data$SRISK, method = "pearson")
cor.test(scaled_data$FRM_q60, scaled_data$FT, method = "pearson")

cor.test(scaled_data$FRM_q70, scaled_data$VIX, method = "pearson")
cor.test(scaled_data$FRM_q70, scaled_data$GT, method = "pearson")
cor.test(scaled_data$FRM_q70, scaled_data$SRISK, method = "pearson")
cor.test(scaled_data$FRM_q70, scaled_data$FT, method = "pearson")

cor.test(scaled_data$FRM_q80, scaled_data$VIX, method = "pearson")
cor.test(scaled_data$FRM_q80, scaled_data$GT, method = "pearson")
cor.test(scaled_data$FRM_q80, scaled_data$SRISK, method = "pearson")
cor.test(scaled_data$FRM_q80, scaled_data$FT, method = "pearson")

cor.test(scaled_data$FRM_q90, scaled_data$VIX, method = "pearson")
cor.test(scaled_data$FRM_q90, scaled_data$GT, method = "pearson")
cor.test(scaled_data$FRM_q90, scaled_data$SRISK, method = "pearson")
cor.test(scaled_data$FRM_q90, scaled_data$FT, method = "pearson")

cor.test(scaled_data$FRM_iqr, scaled_data$VIX, method = "pearson")
cor.test(scaled_data$FRM_iqr, scaled_data$GT, method = "pearson")
cor.test(scaled_data$FRM_iqr, scaled_data$SRISK, method = "pearson")
cor.test(scaled_data$FRM_iqr, scaled_data$FT, method = "pearson")

##kolmogorov smirnov test of FRMs and common systemic risk indicators
apply(scaled_data[,1:7], 2, function(x){
  ks.test(x, scaled_data$VIX,alternative = "two.sided")})
apply(scaled_data[,1:7], 2, function(x){
  ks.test(x, scaled_data$SRISK,alternative = "two.sided")})
apply(scaled_data[,1:7], 2, function(x){
  ks.test(x, scaled_data$GT,alternative = "two.sided")})
apply(scaled_data[,1:7], 2, function(x){
  ks.test(x, scaled_data$FT,alternative = "two.sided")})

#plot FRM@Americas, VIX, GT, FT during financial crisis
cols = brewer.pal(9, "Set1")
datecut = which(scaled_data$date==as.Date("2009-12-31","%Y-%m-%d"))
par(mfrow =c(1,2))
plot(scaled_data$date[1:datecut],scaled_data$GT[1:datecut],xaxt="none", type = "l", xlab = "",col="darkblue", 
      ylab = "",ylim = c(0,1), lwd =3, cex.axis=2, cex.lab=2)
lines(scaled_data$date[1:datecut],scaled_data$FT[1:datecut], type = "l", col="violet", lwd =2)
lines(scaled_data$date[1:datecut],scaled_data$SRISK[1:datecut], type = "l", col="darkgreen", lwd =2)
lines(scaled_data$date[1:datecut],scaled_data$VIX[1:datecut], type = "l", col="red", lwd =2)
lines(scaled_data$date[1:datecut],scaled_data$FRM_index[1:datecut], type = "l", col="black", lwd =2)
ll=seq(min(scaled_data$date), max(scaled_data$date), "quarter")
axis.Date(1, at = ll, labels =ll , font=1, las=1, cex.axis= 1.5)

plot(scaled_data$date[1:datecut],scaled_data$FRM_q50[1:datecut], xaxt="none", type = "l", lty=1,xlab = "", ylab = "",
      ylim = c(0,1), cex.axis=2, cex.lab=2, col = "darkred",lwd=2)
  lines(scaled_data$date[1:datecut],scaled_data$FRM_iqr[1:datecut], type = "l",lty=1, col=cols[7],lwd=2)
  lines(scaled_data$date[1:datecut],scaled_data$FRM_q80[1:datecut], type = "l",lty=1, col=cols[4],lwd=2)
  lines(scaled_data$date[1:datecut],scaled_data$FRM_q90[1:datecut], type = "l",lty=1, col=cols[5],lwd=2)
  lines(scaled_data$date[1:datecut],scaled_data$FRM_q60[1:datecut], type = "l",lty=1, col=cols[2],lwd=2)
  lines(scaled_data$date[1:datecut],scaled_data$FRM_q70[1:datecut], type = "l",lty=1, col=cols[3],lwd=2)
  lines(scaled_data$date[1:datecut],scaled_data$FRM_index[1:datecut], type = "l",lty=1, col="black",lwd=3)
  ll=seq(min(scaled_data$date), max(scaled_data$date), "quarter")
  axis.Date(1, at = ll, labels =ll , font=1, las=1, cex.axis= 1.5)

  #plot FRM@Americas, VIX, GT, FT during Covid-19
par(mfrow =c(1,2))
plot(scaled_data$date[(datecut+1):length(scaled_data$date)],scaled_data$GT[(datecut+1):length(scaled_data$date)], lwd =3, type = "l",
      ylim = c(0,1), xlab = "", ylab = "", cex.axis=2, cex.lab=2, col="darkblue", xaxt="none")
  lines(scaled_data$date[(datecut+1):length(scaled_data$date)],scaled_data$FT[(datecut+1):length(scaled_data$date)], type = "l", col="violet", lwd =2)
  lines(scaled_data$date[(datecut+1):length(scaled_data$date)],scaled_data$SRISK[(datecut+1):length(scaled_data$date)], type = "l", col="darkgreen", lwd =3)
  lines(scaled_data$date[(datecut+1):length(scaled_data$date)],scaled_data$VIX[(datecut+1):length(scaled_data$date)], type = "l", col="red", lwd =3)
  lines(scaled_data$date[(datecut+1):length(scaled_data$date)],scaled_data$FRM_index[(datecut+1):length(scaled_data$date)], type = "l", col="black", lwd =3)
  ll=seq(min(scaled_data$date), max(scaled_data$date), "quarter")
  axis.Date(1, at = ll, labels =ll , font=1, las=1, cex.axis= 1.5)

plot(scaled_data$date[(datecut+1):length(scaled_data$date)],scaled_data$FRM_q50[(datecut+1):length(scaled_data$date)], type = "l", lty=1,xlab = "", 
      ylab = "",ylim = c(0,1), cex.axis=2, xaxt="none", cex.lab=2, col = "darkred",lwd=2)
  lines(scaled_data$date[(datecut+1):length(scaled_data$date)],scaled_data$FRM_iqr[(datecut+1):length(scaled_data$date)], type = "l",lty=1, col=cols[7],lwd=2)
  lines(scaled_data$date[(datecut+1):length(scaled_data$date)],scaled_data$FRM_q80[(datecut+1):length(scaled_data$date)], type = "l",lty=1, col=cols[4],lwd=2)
  lines(scaled_data$date[(datecut+1):length(scaled_data$date)],scaled_data$FRM_q90[(datecut+1):length(scaled_data$date)], type = "l",lty=1, col=cols[5],lwd=2)
  lines(scaled_data$date[(datecut+1):length(scaled_data$date)],scaled_data$FRM_q60[(datecut+1):length(scaled_data$date)], type = "l",lty=1, col=cols[2],lwd=2)
  lines(scaled_data$date[(datecut+1):length(scaled_data$date)],scaled_data$FRM_q70[(datecut+1):length(scaled_data$date)], type = "l",lty=1, col=cols[3],lwd=2)
  lines(scaled_data$date[(datecut+1):length(scaled_data$date)],scaled_data$FRM_index[(datecut+1):length(scaled_data$date)], type = "l",lty=1, col="black",lwd=3)
  ll=seq(min(scaled_data$date), max(scaled_data$date), "quarter")
  axis.Date(1, at = ll, labels =ll , font=1, las=1, cex.axis= 1.5)


