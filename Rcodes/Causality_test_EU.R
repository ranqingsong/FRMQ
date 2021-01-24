# clear all variables
rm(list = ls())
graphics.off()

# set wd
setwd("~/Documents/GitHub/FRMQ/FRMQcausality_EU")

#install and load packages
libraries = c("vars", "stats", "tseries", "quantmod", "tsDyn", "dygraphs", "urca", 
              "xtable")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

################input scaled data#################
data<- read.csv("systemic_risk_comparison_EU.csv", 
                colClasses = c("numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","Date")) 

adf.test(data$FRM_index) #non-stationary
adf.test(data$FRM_q50) #non-stationary
adf.test(data$FRM_q60) #non-stationary
adf.test(data$FRM_q70) #non-stationary
adf.test(data$FRM_q80) #non-stationary
adf.test(data$FRM_q90) #non-stationary
adf.test(data$FRM_iqr) #non-stationary

adf.test(data$VSTOXX) #non-stationary
adf.test(data$CISS) #non-stationary
adf.test(data$FT) #stationary
####### Causality FRM_index and VSTOXX ###############
### from FRM_index to VSTOXX ###
lm_SF = lm(data$VSTOXX ~ data$FRM_index)
summary(lm_SF)
res_lm_SF = resid(lm_SF)
res.ADF <- ur.df(res_lm_SF, type = "none", selectlags = "AIC")
summary(res.ADF)

### from VSTOXX to FRM_index ###
lm_FS = lm(data$FRM_index ~ data$VSTOXX)
summary(lm_FS)
res_lm_FS = resid(lm_FS)
res.ADF <- ur.df(res_lm_FS, type = "none", selectlags = "AIC")
summary(res.ADF)

####### Causality FRM_q50 and VSTOXX ###############
### from FRM_q50 to VSTOXX ###
lm_SF = lm(data$VSTOXX ~ data$FRM_q50)
summary(lm_SF)
res_lm_SF = resid(lm_SF)
res.ADF <- ur.df(res_lm_SF, type = "none", selectlags = "AIC")
summary(res.ADF)

### from VSTOXX to FRM_q50 ###
lm_FS = lm(data$FRM_q50 ~ data$VSTOXX)
summary(lm_FS)
res_lm_FS = resid(lm_FS)
res.ADF <- ur.df(res_lm_FS, type = "none", selectlags = "AIC")
summary(res.ADF)

####### Causality FRM_q60 and VSTOXX ###############
### from FRM_q60 to VSTOXX ###
lm_SF = lm(data$VSTOXX ~ data$FRM_q60)
summary(lm_SF)
res_lm_SF = resid(lm_SF)
res.ADF <- ur.df(res_lm_SF, type = "none", selectlags = "AIC")
summary(res.ADF)

### from VSTOXX to FRM_q60 ###
lm_FS = lm(data$FRM_q60 ~ data$VSTOXX)
summary(lm_FS)
res_lm_FS = resid(lm_FS)
res.ADF <- ur.df(res_lm_FS, type = "none", selectlags = "AIC")
summary(res.ADF)



####### Causality FRM_q70 and VSTOXX ###############
### from FRM_q70 to VSTOXX ###
lm_SF = lm(data$VSTOXX ~ data$FRM_q70)
summary(lm_SF)
res_lm_SF = resid(lm_SF)
res.ADF <- ur.df(res_lm_SF, type = "none", selectlags = "AIC")
summary(res.ADF)

### from VSTOXX to FRM_q70 ###
lm_FS = lm(data$FRM_q70 ~ data$VSTOXX)
summary(lm_FS)
res_lm_FS = resid(lm_FS)
res.ADF <- ur.df(res_lm_FS, type = "none", selectlags = "AIC")
summary(res.ADF)


####### Causality FRM_q80 and VSTOXX ###############
### from FRM_q80 to VSTOXX ###
lm_SF = lm(data$VSTOXX ~ data$FRM_q80)
summary(lm_SF)
res_lm_SF = resid(lm_SF)
res.ADF <- ur.df(res_lm_SF, type = "none", selectlags = "AIC")
summary(res.ADF)

### from VSTOXX to FRM_q80 ###
lm_FS = lm(data$FRM_q80 ~ data$VSTOXX)
summary(lm_FS)
res_lm_FS = resid(lm_FS)
res.ADF <- ur.df(res_lm_FS, type = "none", selectlags = "AIC")
summary(res.ADF)


####### Causality FRM_q90 and VSTOXX ###############
### from FRM_q90 to VSTOXX ###
lm_SF = lm(data$VSTOXX ~ data$FRM_q90)
summary(lm_SF)
res_lm_SF = resid(lm_SF)
res.ADF <- ur.df(res_lm_SF, type = "none", selectlags = "AIC")
summary(res.ADF)

### from VSTOXX to FRM_q90 ###
lm_FS = lm(data$FRM_q90 ~ data$VSTOXX)
summary(lm_FS)
res_lm_FS = resid(lm_FS)
res.ADF <- ur.df(res_lm_FS, type = "none", selectlags = "AIC")
summary(res.ADF)


####### Causality FRM_iqr and VSTOXX ###############
### from FRM_iqr to VSTOXX ###
lm_SF = lm(data$CISS ~ data$FRM_iqr)
summary(lm_SF)
res_lm_SF = resid(lm_SF)
res.ADF <- ur.df(res_lm_SF, type = "none", selectlags = "AIC")
summary(res.ADF)

### from VSTOXX to FRM_iqr ###
lm_FS = lm(data$FRM_iqr ~ data$VSTOXX)
summary(lm_FS)
res_lm_FS = resid(lm_FS)
res.ADF <- ur.df(res_lm_FS, type = "none", selectlags = "AIC")
summary(res.ADF)


####### Causality FRM_index and CISS ###############
### from FRM_index to CISS ###
lm_SF = lm(data$CISS ~ data$FRM_index)
summary(lm_SF)
res_lm_SF = resid(lm_SF)
res.ADF <- ur.df(res_lm_SF, type = "none", selectlags = "AIC")
summary(res.ADF)

### from CISS to FRM_index ###
lm_FS = lm(data$FRM_index ~ data$CISS)
summary(lm_FS)
res_lm_FS = resid(lm_FS)
res.ADF <- ur.df(res_lm_FS, type = "none", selectlags = "AIC")
summary(res.ADF)

####### Causality FRM_q50 and CISS ###############
### from FRM_q50 to CISS ###
lm_SF = lm(data$CISS ~ data$FRM_q50)
summary(lm_SF)
res_lm_SF = resid(lm_SF)
res.ADF <- ur.df(res_lm_SF, type = "none", selectlags = "AIC")
summary(res.ADF)

### from CISS to FRM_q50 ###
lm_FS = lm(data$FRM_q50 ~ data$CISS)
summary(lm_FS)
res_lm_FS = resid(lm_FS)
res.ADF <- ur.df(res_lm_FS, type = "none", selectlags = "AIC")
summary(res.ADF)

####### Causality FRM_q60 and CISS ###############
### from FRM_q60 to CISS ###
lm_SF = lm(data$CISS ~ data$FRM_q60)
summary(lm_SF)
res_lm_SF = resid(lm_SF)
res.ADF <- ur.df(res_lm_SF, type = "none", selectlags = "AIC")
summary(res.ADF)

### from CISS to FRM_q60 ###
lm_FS = lm(data$FRM_q60 ~ data$CISS)
summary(lm_FS)
res_lm_FS = resid(lm_FS)
res.ADF <- ur.df(res_lm_FS, type = "none", selectlags = "AIC")
summary(res.ADF)



####### Causality FRM_q70 and CISS ###############
### from FRM_q70 to CISS ###
lm_SF = lm(data$CISS ~ data$FRM_q70)
summary(lm_SF)
res_lm_SF = resid(lm_SF)
res.ADF <- ur.df(res_lm_SF, type = "none", selectlags = "AIC")
summary(res.ADF)

### from CISS to FRM_q70 ###
lm_FS = lm(data$FRM_q70 ~ data$CISS)
summary(lm_FS)
res_lm_FS = resid(lm_FS)
res.ADF <- ur.df(res_lm_FS, type = "none", selectlags = "AIC")
summary(res.ADF)


####### Causality FRM_q80 and CISS ###############
### from FRM_q80 to CISS ###
lm_SF = lm(data$CISS ~ data$FRM_q80)
summary(lm_SF)
res_lm_SF = resid(lm_SF)
res.ADF <- ur.df(res_lm_SF, type = "none", selectlags = "AIC")
summary(res.ADF)

### from CISS to FRM_q80 ###
lm_FS = lm(data$FRM_q80 ~ data$CISS)
summary(lm_FS)
res_lm_FS = resid(lm_FS)
res.ADF <- ur.df(res_lm_FS, type = "none", selectlags = "AIC")
summary(res.ADF)


####### Causality FRM_q90 and CISS ###############
### from FRM_q90 to CISS ###
lm_SF = lm(data$CISS ~ data$FRM_q90)
summary(lm_SF)
res_lm_SF = resid(lm_SF)
res.ADF <- ur.df(res_lm_SF, type = "none", selectlags = "AIC")
summary(res.ADF)

### from CISS to FRM_q90 ###
lm_FS = lm(data$FRM_q90 ~ data$CISS)
summary(lm_FS)
res_lm_FS = resid(lm_FS)
res.ADF <- ur.df(res_lm_FS, type = "none", selectlags = "AIC")
summary(res.ADF)


####### Causality FRM_iqr and CISS ###############
### from FRM_iqr to CISS ###
lm_SF = lm(data$CISS ~ data$FRM_iqr)
summary(lm_SF)
res_lm_SF = resid(lm_SF)
res.ADF <- ur.df(res_lm_SF, type = "none", selectlags = "AIC")
summary(res.ADF)

### from CISS to FRM_iqr ###
lm_FS = lm(data$FRM_iqr ~ data$CISS)
summary(lm_FS)
res_lm_FS = resid(lm_FS)
res.ADF <- ur.df(res_lm_FS, type = "none", selectlags = "AIC")
summary(res.ADF)

#############################################################
####### Causality FRM_index and FT ###############
VAR = data[,c("FRM_index","FT")]

VARselect(VAR, lag.max = 20, type = "both")
varest1 = VAR(VAR, p = 5, type = "both")
resid = residuals(varest1)
#check first order autocorrelation among residuals.
serial.test(varest1, lags.pt = 20, type = "PT.asymptotic")
serial.test(varest1, lags.pt = 20, type = "PT.adjusted")
serial.test(varest1, lags.pt = 20, type = "BG") 
serial.test(varest1, lags.pt = 20, type = "ES") 

causality(varest1, cause = "FRM_index")$Granger 
causality(varest1, cause = "FT")$Granger  
acf(resid, ylab = "", cex.axis = 2, lwd = 4, xlab = "", cex.main = 1.6)[1]

####### Causality FRM_q50 and FT ###############
VAR = data[,c("FRM_q50","FT")]

VARselect(VAR, lag.max = 20, type = "both")
varest1 = VAR(VAR, p =12, type = "both")
resid = residuals(varest1)
#ckeck first order autocorrelation among residuals.
serial.test(varest1, lags.pt = 20, type = "PT.asymptotic")
serial.test(varest1, lags.pt = 20, type = "PT.adjusted")
serial.test(varest1, lags.pt = 20, type = "BG") 
serial.test(varest1, lags.pt = 20, type = "ES") 

causality(varest1, cause = "FRM_q50")$Granger 
causality(varest1, cause = "FT")$Granger  
acf(resid, ylab = "", cex.axis = 2, lwd = 4, xlab = "", cex.main = 1.6)[1]

####### Causality FRM_q60 and GT ###############
VAR = data[,c("FRM_q60","FT")]

VARselect(VAR, lag.max = 20, type = "both")
varest1 = VAR(VAR, p = 8, type = "both")
resid = residuals(varest1)
#ckeck first order autocorrelation among residuals.
serial.test(varest1, lags.pt = 20, type = "PT.asymptotic")
serial.test(varest1, lags.pt = 20, type = "PT.adjusted")
serial.test(varest1, lags.pt = 20, type = "BG") 
serial.test(varest1, lags.pt = 20, type = "ES") 

causality(varest1, cause = "FRM_q60")$Granger 
causality(varest1, cause = "FT")$Granger  
acf(resid, ylab = "", cex.axis = 2, lwd = 4, xlab = "", cex.main = 1.6)[1]

####### Causality FRM_q70 and FT ###############
VAR = data[,c("FRM_q70","FT")]

VARselect(VAR, lag.max = 20, type = "both")
varest1 = VAR(VAR, p =11, type = "both")
resid = residuals(varest1)
#ckeck first order autocorrelation among residuals.
serial.test(varest1, lags.pt = 20, type = "PT.asymptotic")
serial.test(varest1, lags.pt = 20, type = "PT.adjusted")
serial.test(varest1, lags.pt = 20, type = "BG") 
serial.test(varest1, lags.pt = 20, type = "ES") 

causality(varest1, cause = "FRM_q70")$Granger 
causality(varest1, cause = "FT")$Granger  
acf(resid, ylab = "", cex.axis = 2, lwd = 4, xlab = "", cex.main = 1.6)[1]

####### Causality FRM_q80 and FT ###############
VAR = data[,c("FRM_q80","FT")]

VARselect(VAR, lag.max = 20, type = "both")
varest1 = VAR(VAR, p =13, type = "both")
resid = residuals(varest1)
#ckeck first order autocorrelation among residuals.
serial.test(varest1, lags.pt = 20, type = "PT.asymptotic")
serial.test(varest1, lags.pt = 20, type = "PT.adjusted")
serial.test(varest1, lags.pt = 20, type = "BG") 
serial.test(varest1, lags.pt = 20, type = "ES") 

causality(varest1, cause = "FRM_q80")$Granger 
causality(varest1, cause = "FT")$Granger  
acf(resid, ylab = "", cex.axis = 2, lwd = 4, xlab = "", cex.main = 1.6)[1]

####### Causality FRM_q90 and FT ###############
VAR = data[,c("FRM_q90","FT")]

VARselect(VAR, lag.max = 20, type = "both")
varest1 = VAR(VAR, p = 9, type = "both")
resid = residuals(varest1)
#ckeck first order autocorrelation among residuals.
serial.test(varest1, lags.pt = 20, type = "PT.asymptotic")
serial.test(varest1, lags.pt = 20, type = "PT.adjusted")
serial.test(varest1, lags.pt = 20, type = "BG") 
serial.test(varest1, lags.pt = 20, type = "ES") 

causality(varest1, cause = "FRM_q90")$Granger 
causality(varest1, cause = "FT")$Granger  
acf(resid, ylab = "", cex.axis = 2, lwd = 4, xlab = "", cex.main = 1.6)[1]

####### Causality FRM_iqr and FT ###############
VAR = data[,c("FRM_iqr","FT")]

VARselect(VAR, lag.max = 20, type = "both")
varest1 = VAR(VAR, p =13, type = "both")
resid = residuals(varest1)
#ckeck first order autocorrelation among residuals.
serial.test(varest1, lags.pt = 20, type = "PT.asymptotic")
serial.test(varest1, lags.pt = 20, type = "PT.adjusted")
serial.test(varest1, lags.pt = 20, type = "BG") 
serial.test(varest1, lags.pt = 20, type = "ES") 

causality(varest1, cause = "FRM_iqr")$Granger 
causality(varest1, cause = "FT")$Granger  
acf(resid, ylab = "", cex.axis = 2, lwd = 4, xlab = "", cex.main = 1.6)[1]


