#multivariate Recession models

#clean environment
rm(list = ls(all = TRUE))
setwd("~/Dropbox/Masterarbeit/02_project/FRM_All/03_Quantlets")
setwd("C:/Users/srq04/Dropbox/Masterarbeit/02_project/FRM_All/03_Quantlets")

#libraries = c("magick", "texreg", "zoo","pscl", "plotly", "lubridate","tidyr", "dplyr","graphics",
#              "stringr", "timeDate", "igraph", "data.table", "ggplot2","kableExtra","webshot","stargazer")
libraries = c( "dplyr","lubridate", "texreg","RColorBrewer","zoo","car")
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

#Estrella Pseudo R-square
data_m <- read.csv(paste0("01_csvData/lagged_FRM_index_all",date_start1,"-",date_end1, channel,".csv"))
data_m2<- read.csv(paste0("01_csvData/lagged_FRM_index_all",date_start2,"-",date_end2, channel,".csv"))
data_m <- rbind(data_m, data_m2)

fitness <- function(AM_lag1_output, channel){
  data_m <- read.csv(paste0("01_csvData/lagged_FRM_index_all",date_start1,"-",date_end1, channel,".csv"))
  data_m2<- read.csv(paste0("01_csvData/lagged_FRM_index_all",date_start2,"-",date_end2, channel,".csv"))
  data_m <- rbind(data_m, data_m2)
  estrella <- matrix(NA,1,7)
  lhnull<- logLik(glm(rec~1, family = binomial(link = "probit"), data=data_m)) #the log-likelihood from the intercept-only model
  for (i in 1:7) {
    lh <- logLik(AM_lag1_output[[i]])#the log-likelihood from the fitted model
    n <- dim(AM_lag1_output[[i]]$data)[1]
    estrella[1,i] <- 1-(lh/lhnull)^(-2/n*lhnull)
  }
  colnames(estrella) <-names(AM_lag1_output)
  return(estrella)}

#autocorrelation test###
#dwt(month_output_stepwise_all$mean, max.lag = 6)

###stepwise select based on combined dataset
rec_model_stepwise_all <- function(channel){
  ##import lagged FRM datasets
  data_m <- read.csv(paste0("01_csvData/lagged_FRM_index_all",date_start1,"-",date_end1, channel,".csv"))
  data_m2<- read.csv(paste0("01_csvData/lagged_FRM_index_all",date_start2,"-",date_end2, channel,".csv"))
  data_m <- rbind(data_m, data_m2)
  data_m$ym <- as.yearmon(data_m$ym)
  data_m <- na.omit(data_m)
  #multiple lags --> k= 1,2,4,6 months
  pm_full_mean <- glm(rec ~ lag1_FRM_index+lag2_FRM_index+lag3_FRM_index+lag4_FRM_index+lag5_FRM_index+lag6_FRM_index, family = binomial(link = "probit"), data = data_m)
  pm_full_q50 <- glm(rec ~ lag1_FRM_q50+lag2_FRM_q50+lag3_FRM_q50+lag4_FRM_q50+lag5_FRM_q50+lag6_FRM_q50, family = binomial(link = "probit"), data = data_m)
  pm_full_q60 <- glm(rec ~ lag1_FRM_q60+lag2_FRM_q60+lag3_FRM_q60+lag4_FRM_q60+lag5_FRM_q60+lag6_FRM_q60, family = binomial(link = "probit"), data = data_m)
  pm_full_q70 <- glm(rec ~ lag1_FRM_q70+lag2_FRM_q70+lag3_FRM_q70+lag4_FRM_q70+lag5_FRM_q70+lag6_FRM_q70, family = binomial(link = "probit"), data = data_m)
  pm_full_q80 <- glm(rec ~ lag1_FRM_q80+lag2_FRM_q80+lag3_FRM_q80+lag4_FRM_q80+lag5_FRM_q80+lag6_FRM_q80, family = binomial(link = "probit"), data = data_m)
  pm_full_q90 <- glm(rec ~ lag1_FRM_q90+lag2_FRM_q90+lag3_FRM_q90+lag4_FRM_q90+lag5_FRM_q90+lag6_FRM_q90, family = binomial(link = "probit"), data = data_m)
  pm_full_iqr <- glm(rec ~ lag1_FRM_iqr+lag2_FRM_iqr+lag3_FRM_iqr+  +lag5_FRM_iqr+lag6_FRM_iqr, family = binomial(link = "probit"), data = data_m)
  
  pstepm_mean <- step(pm_full_mean, trace = F)
  pstepm_q50 <- step(pm_full_q50, trace = F)
  pstepm_q60 <- step(pm_full_q60, trace = F)
  pstepm_q70 <- step(pm_full_q70, trace = F)
  pstepm_q80 <- step(pm_full_q80, trace = F)
  pstepm_q90 <- step(pm_full_q90, trace = F)
  pstepm_iqr <- step(pm_full_iqr, trace = F)
  
  output <- list(mean=pstepm_mean,q50=pstepm_q50,q60=pstepm_q60,q70=pstepm_q70, q80=pstepm_q80,q90=pstepm_q90,iqr=pstepm_iqr)
  return(output)
}
month_output_stepwise_all <- rec_model_stepwise_all(channel)
screenreg(month_output_stepwise_all)
plot(residuals(month_output_stepwise_all$mean))
plot(residuals(month_output_stepwise_all$q60))
EU_output_stepwise_all <- rec_model_stepwise_all( channel2)
screenreg(EU_output_stepwise_all)

fitness(month_output_stepwise_all,channel)
fitness(EU_output_stepwise_all,channel2)


#keep FRM candidates lag at 1 and 2 month based on combined dataset
rec_model_1_2 <- function(channel){
  ##import lagged FRM datasets
  ##import lagged FRM datasets
  data_m <- read.csv(paste0("01_csvData/lagged_FRM_index_all",date_start1,"-",date_end1, channel,".csv"))
  data_m2<- read.csv(paste0("01_csvData/lagged_FRM_index_all",date_start2,"-",date_end2, channel,".csv"))
  data_m <- data.frame(rbind(data_m, data_m2))
  data_m$ym <- as.yearmon(data_m$ym)
  data_m <- cbind(data_m[,substr(colnames(data_m), 1,4) %in% c("lag1","lag2")], rec=data_m$rec, ym=data_m$ym)
  data_m <- na.omit(data_m) 
  
  m_mean <- glm(rec ~ lag1_FRM_index+lag2_FRM_index, family = binomial(link = "probit"), data = data_m)
  m_q50 <- glm(rec ~ lag1_FRM_q50+lag2_FRM_q50, family = binomial(link = "probit"), data = data_m)
  m_q60 <- glm(rec ~ lag1_FRM_q60+lag2_FRM_q60, family = binomial(link = "probit"), data = data_m)
  m_q70 <- glm(rec ~ lag1_FRM_q70+lag2_FRM_q70, family = binomial(link = "probit"), data = data_m)
  m_q80 <- glm(rec ~ lag1_FRM_q80+lag2_FRM_q80, family = binomial(link = "probit"), data = data_m)
  m_q90 <- glm(rec ~ lag1_FRM_q90+lag2_FRM_q90, family = binomial(link = "probit"), data = data_m) 
  return(list(mean= m_mean, q50=m_q50, q60=m_q60, q70=m_q70, q80=m_q80,q90=m_q90))
}
output_1and2<- rec_model_1_2(channel)
screenreg(output_1and2)
EU_output_1and2<- rec_model_1_2(channel2)
screenreg(EU_output_1and2)

#keep FRM candidates lag at 1,2,3 month based on combined dataset
rec_model_123 <- function(channel){
  ##import lagged FRM datasets
  ##import lagged FRM datasets
  data_m <- read.csv(paste0("01_csvData/lagged_FRM_index_all",date_start1,"-",date_end1, channel,".csv"))
  data_m2<- read.csv(paste0("01_csvData/lagged_FRM_index_all",date_start2,"-",date_end2, channel,".csv"))
  data_m <- rbind(data_m, data_m2)
  data_m$ym <- as.yearmon(data_m$ym)
  data_m <- cbind(data_m[,substr(colnames(data_m), 1,4) %in% c("lag1","lag2","lag3")], rec=data_m$rec, ym=data_m$ym)
  data_m <- na.omit(data_m) 
  
  m_mean <- glm(rec ~ lag1_FRM_index+lag2_FRM_index+lag3_FRM_index, family = binomial(link = "probit"), data = data_m)
  m_q50 <- glm(rec ~ lag1_FRM_q50+lag2_FRM_q50+lag3_FRM_q50, family = binomial(link = "probit"), data = data_m)
  m_q60 <- glm(rec ~ lag1_FRM_q60+lag2_FRM_q60+lag3_FRM_q60, family = binomial(link = "probit"), data = data_m)
  m_q70 <- glm(rec ~ lag1_FRM_q70+lag2_FRM_q70+lag3_FRM_q70, family = binomial(link = "probit"), data = data_m)
  m_q80 <- glm(rec ~ lag1_FRM_q80+lag2_FRM_q80+lag3_FRM_q80, family = binomial(link = "probit"), data = data_m)
  m_q90 <- glm(rec ~ lag1_FRM_q90+lag2_FRM_q90+lag3_FRM_q90, family = binomial(link = "probit"), data = data_m) 
  return(list(mean= m_mean, q50=m_q50, q60=m_q60, 
              q70=m_q70, q80=m_q80, q90=m_q90))
}
output_123 <- rec_model_123(channel)
screenreg(output_123)
lapply(output_123, pR2)
EU_output_123 <- rec_model_123(channel2)
screenreg(EU_output_123)
###done

#implied recession probability
est_rec_prob <- function(month_output_list, channel){
  pred_k1 = sapply(1:length(month_output_list), function(num_reg) assign(paste0("pred_", names(month_output_list)[num_reg]), month_output_list[[num_reg]]$fitted.values))
  colnames(pred_k1) = names(month_output_list)
  #join date 
  pred_k1 <- pred_k1 %>% as.data.frame()%>% mutate(n=rownames(pred_k1))
  pred_k1 <- cbind(ym=month_output_list[[1]]$data$ym, rec= month_output_list[[1]]$data$rec,pred_k1)
  pred_k1 <- arrange(pred_k1, ym)
}

###create plot of implied probability based on combined data
create_plot_two <- function(month_output_stepwise, channel,title){
  pred <- est_rec_prob(month_output_stepwise, channel)
  cols <- brewer.pal(9, "Set1")
  png(paste0("03_plots/implied_rec_",channel,title,".png"), width = 1500, height = 1000, bg = "transparent")
  par(mfrow =c(1,2))
  index<- min(which(substr(pred$ym, 5,8)=="2019"))
  pred_1 <- pred[1:index-1,]
  pred_2 <- pred[index:length(pred$ym),]
  {plot(x=pred_1$ym, y=pred_1$rec, type="n", xaxt = "none",ylab = "Implied recession probability", xlab="", cex.lab=1.5, cex.axis=2)
    rect(xleft =min(pred_1$ym[which(pred_1$rec==1)]), ybottom = 0, xright = max(pred_1$ym[which(pred_1$rec==1)]), ytop = 1,col="gray", border = "#F4F6F6")
    lines(x=pred_1$ym, y = pred_1$iqr, col = cols[7], lwd = 2)
    lines(x=pred_1$ym, y = pred_1$q90, col = cols[5], lwd = 2)
    lines(x=pred_1$ym, y = pred_1$q50, col = cols[1], type = "l", lwd = 2)
    lines(x=pred_1$ym, y = pred_1$q60, col = cols[2], lwd = 2)
    lines(x=pred_1$ym, y = pred_1$q70, col = cols[3], lwd = 2)
    lines(x=pred_1$ym, y = pred_1$q80,col = cols[4], lwd = 2)
    lines(x=pred_1$ym, y = pred_1$mean, col = "black", type = "l", lwd = 3)
    axis(1, at = pred_1$ym, labels = pred_1$ym, font=1, las=1, cex.axis=2)  
}
  {plot(x=pred_2$ym, y=pred_2$rec, type="n", xaxt = "none",ylab = "Implied recession probability", xlab="", cex.lab=1.5, cex.axis=2)
    rect(xleft =min(pred_2$ym[which(pred_2$rec==1)]), ybottom = 0, xright = max(pred_2$ym[which(pred_2$rec==1)]), ytop = 1,col="gray", border = "#F4F6F6")
    lines(x=pred_2$ym, y = pred_2$iqr, col = cols[7], lwd = 2)
    lines(x=pred_2$ym, y = pred_2$q50, col = cols[1], type = "l", lwd = 2)
    lines(x=pred_2$ym, y = pred_2$q60, col = cols[2], lwd = 2)
    lines(x=pred_2$ym, y = pred_2$q70, col = cols[3], lwd = 2)
    lines(x=pred_2$ym, y = pred_2$q80,col = cols[4], lwd = 2)
    lines(x=pred_2$ym, y = pred_2$q90, col = cols[5], lwd = 2)
    lines(x=pred_2$ym, y = pred_2$mean, col = "black", type = "l", lwd = 3)
    axis(1, at = pred_2$ym, labels = pred_2$ym, font=1, las=1, cex.axis=2)
    #legend("topleft", c(paste0(c("FRM_mean", "FRM_q50", "FRM_q60", "FRM_q70", "FRM_q80", "FRM_q90"),"@",channel),"Recession"), 
    #       fill = c("black", cols[1], cols[2], cols[3], cols[4], cols[5], "gray"), cex = 2)
    }  
  dev.off()
}
create_plot_two(month_output_stepwise_all,channel,"stepwise")
create_plot_two(output_1and2,channel,"k=1,2")
create_plot_two(output_123,channel,"k=1,2,3")

create_plot_two(EU_output_123,channel2, "k=1,2,3")
create_plot_two(EU_output_1and2,channel2, "k=1,2")
create_plot_two(EU_output_stepwise_all,channel2, "stepwise")

#####out-of-sample test######
###import data during stable period
out_data <- read.csv("https://raw.githubusercontent.com/QuantLet/FRM/master/FRM_GT/FRM_VIX_SRISK_GT.csv")
out_data$Date <- as.Date(out_data[, 1], format = "%d/%m/%Y")
out_data<- out_data[565:2386,1:2] #from 2010 to 2016
out_data <- out_data %>% mutate(ym = paste0(year(Date), "-",format(Date, "%m")))
out_datam = aggregate(cbind(FRM)~ ym, 
                   data = out_data, mean)

#lag FRM candidates at 1 to 6 months
for (lag_num in 1:6) {
  lags = sapply(2, function(i) dplyr::lag(out_datam[,i], lag_num))
  collist = c("FRM_index")
  colnames(lags) = paste0("lag", lag_num,"_",collist)
  out_datam = cbind(out_datam, lags)
}

####import daily recession indicator
recdata<- read.csv("01_csvData/USREC_monthly.csv")
recdata$date <- as.Date(recdata$DATE, format = "%Y-%m-%d")
recdata$ym <- paste0(year(recdata$date), "-",format(recdata$date, "%m"))
recdata <- rename(recdata, rec=USREC)
recdata <- select(recdata, c("ym", "rec","date"))

#merge recession indicators with FRM 
out_datam<- merge(out_datam, recdata, by = "ym", all.x = T)
data_m$pred <-predict.glm(month_output_stepwise_all$mean, data.frame(data_m),type = "response")
out_datam$pred <-predict.glm(month_output_stepwise_all$mean, data.frame(out_datam),type = "response")

png(paste0("03_plots/implied_rec_outofsample_",channel,".png"), width = 1500, height = 500, bg = "transparent")
{plot(x=out_datam$date, y=out_datam$pred,type="l",ylim = c(0,1),ylab = "Implied recession probability", xlab="", cex.lab=1.5, cex.axis=2)
lines(x=out_datam$date, y=out_datam$pred,type="l",lwd=3)}
dev.off()

##compare two datasets
a<- read.csv(paste0("01_csvData/FRM_index_all",date_start1,"-",date_end1, channel,".csv"), 
             colClasses = c("Date","numeric","numeric","numeric","numeric","numeric","numeric","numeric")) 
a<-a[,1:2]
b<-out_data[,c("Date","FRM")]
a <- merge(a,b, all.y = T, by.x = "ticker", by.y = "Date")
plot(a$ticker, a$FRM, type="l", ylim=c(0,0.15))
lines(a$ticker, a$FRM_index, col="blue")
