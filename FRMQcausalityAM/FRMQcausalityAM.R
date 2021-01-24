#clear environment
rm(list = ls())
graphics.off()

# Set working directory setwd('')

# Install and load packages
libraries = c("vars", "stats", "tseries", "quantmod", "tsDyn", "dygraphs", "urca", "xtable")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
    install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

################ input scaled data#################
data = read.csv("systemic_risk_comparison_AM.csv", colClasses = c("numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
    "numeric", "numeric", "numeric", "numeric", "Date"))

adf.test(data$FRM_index)  #non-stationary
adf.test(data$FRM_q50)  #non-stationary
adf.test(data$FRM_q60)  #non-stationary
adf.test(data$FRM_q70)  #non-stationary
adf.test(data$FRM_q80)  #non-stationary
adf.test(data$FRM_q90)  #non-stationary
adf.test(data$FRM_iqr)  #non-stationary

adf.test(data$VIX)  #non-stationary
adf.test(data$SRISK)  #non-stationary
adf.test(data$GT)  #stationary
adf.test(data$FT)  #stationary

####### Causality FRM_index and VIX ############### from FRM_index to VIX ###
lm_SF = lm(data$VIX ~ data$FRM_index)
summary(lm_SF)
res_lm_SF = resid(lm_SF)
res.ADF = ur.df(res_lm_SF, type = "none", selectlags = "AIC")
summary(res.ADF)

### from VIX to FRM_index ###
lm_FS = lm(data$FRM_index ~ data$VIX)
summary(lm_FS)
res_lm_FS = resid(lm_FS)
res.ADF = ur.df(res_lm_FS, type = "none", selectlags = "AIC")
summary(res.ADF)

####### Causality FRM_q50 and VIX ############### from FRM_q50 to VIX ###
lm_SF = lm(data$VIX ~ data$FRM_q50)
summary(lm_SF)
res_lm_SF = resid(lm_SF)
res.ADF = ur.df(res_lm_SF, type = "none", selectlags = "AIC")
summary(res.ADF)

### from VIX to FRM_q50 ###
lm_FS = lm(data$FRM_q50 ~ data$VIX)
summary(lm_FS)
res_lm_FS = resid(lm_FS)
res.ADF = ur.df(res_lm_FS, type = "none", selectlags = "AIC")
summary(res.ADF)

####### Causality FRM_q60 and VIX ############### from FRM_q60 to VIX ###
lm_SF = lm(data$VIX ~ data$FRM_q60)
summary(lm_SF)
res_lm_SF = resid(lm_SF)
res.ADF = ur.df(res_lm_SF, type = "none", selectlags = "AIC")
summary(res.ADF)

### from VIX to FRM_q60 ###
lm_FS = lm(data$FRM_q60 ~ data$VIX)
summary(lm_FS)
res_lm_FS = resid(lm_FS)
res.ADF = ur.df(res_lm_FS, type = "none", selectlags = "AIC")
summary(res.ADF)



####### Causality FRM_q70 and VIX ############### from FRM_q70 to VIX ###
lm_SF = lm(data$VIX ~ data$FRM_q70)
summary(lm_SF)
res_lm_SF = resid(lm_SF)
res.ADF = ur.df(res_lm_SF, type = "none", selectlags = "AIC")
summary(res.ADF)

### from VIX to FRM_q70 ###
lm_FS = lm(data$FRM_q70 ~ data$VIX)
summary(lm_FS)
res_lm_FS = resid(lm_FS)
res.ADF = ur.df(res_lm_FS, type = "none", selectlags = "AIC")
summary(res.ADF)


####### Causality FRM_q80 and VIX ############### from FRM_q80 to VIX ###
lm_SF = lm(data$VIX ~ data$FRM_q80)
summary(lm_SF)
res_lm_SF = resid(lm_SF)
res.ADF = ur.df(res_lm_SF, type = "none", selectlags = "AIC")
summary(res.ADF)

### from VIX to FRM_q80 ###
lm_FS = lm(data$FRM_q80 ~ data$VIX)
summary(lm_FS)
res_lm_FS = resid(lm_FS)
res.ADF = ur.df(res_lm_FS, type = "none", selectlags = "AIC")
summary(res.ADF)


####### Causality FRM_q90 and VIX ############### from FRM_q90 to VIX ###
lm_SF = lm(data$VIX ~ data$FRM_q90)
summary(lm_SF)
res_lm_SF = resid(lm_SF)
res.ADF = ur.df(res_lm_SF, type = "none", selectlags = "AIC")
summary(res.ADF)

### from VIX to FRM_q90 ###
lm_FS = lm(data$FRM_q90 ~ data$VIX)
summary(lm_FS)
res_lm_FS = resid(lm_FS)
res.ADF = ur.df(res_lm_FS, type = "none", selectlags = "AIC")
summary(res.ADF)


####### Causality FRM_iqr and VIX ############### from FRM_iqr to VIX ###
lm_SF = lm(data$VIX ~ data$FRM_iqr)
summary(lm_SF)
res_lm_SF = resid(lm_SF)
res.ADF = ur.df(res_lm_SF, type = "none", selectlags = "AIC")
summary(res.ADF)

### from VIX to FRM_iqr ###
lm_FS = lm(data$FRM_iqr ~ data$VIX)
summary(lm_FS)
res_lm_FS = resid(lm_FS)
res.ADF = ur.df(res_lm_FS, type = "none", selectlags = "AIC")
summary(res.ADF)

####### Causality FRM_index and SRISK ############### from FRM_index to SRISK ###
lm_SF = lm(data$SRISK ~ data$FRM_index)
summary(lm_SF)
res_lm_SF = resid(lm_SF)
res.ADF = ur.df(res_lm_SF, type = "none", selectlags = "AIC")
summary(res.ADF)

### from SRISK to FRM_index ###
lm_FS = lm(data$FRM_index ~ data$SRISK)
summary(lm_FS)
res_lm_FS = resid(lm_FS)
res.ADF = ur.df(res_lm_FS, type = "none", selectlags = "AIC")
summary(res.ADF)

####### Causality FRM_q50 and SRISK ############### from FRM_q50 to SRISK ###
lm_SF = lm(data$SRISK ~ data$FRM_q50)
summary(lm_SF)
res_lm_SF = resid(lm_SF)
res.ADF = ur.df(res_lm_SF, type = "none", selectlags = "AIC")
summary(res.ADF)

### from SRISK to FRM_q50 ###
lm_FS = lm(data$FRM_q50 ~ data$SRISK)
summary(lm_FS)
res_lm_FS = resid(lm_FS)
res.ADF = ur.df(res_lm_FS, type = "none", selectlags = "AIC")
summary(res.ADF)

####### Causality FRM_q60 and SRISK ############### from FRM_q60 to SRISK ###
lm_SF = lm(data$SRISK ~ data$FRM_q60)
summary(lm_SF)
res_lm_SF = resid(lm_SF)
res.ADF = ur.df(res_lm_SF, type = "none", selectlags = "AIC")
summary(res.ADF)

### from VIX to FRM_q60 ###
lm_FS = lm(data$FRM_q60 ~ data$SRISK)
summary(lm_FS)
res_lm_FS = resid(lm_FS)
res.ADF = ur.df(res_lm_FS, type = "none", selectlags = "AIC")
summary(res.ADF)

####### Causality FRM_q70 and SRISK ############### from FRM_q70 to SRISK ###
lm_SF = lm(data$SRISK ~ data$FRM_q70)
summary(lm_SF)
res_lm_SF = resid(lm_SF)
res.ADF = ur.df(res_lm_SF, type = "none", selectlags = "AIC")
summary(res.ADF)

### from VIX to FRM_q70 ###
lm_FS = lm(data$FRM_q70 ~ data$SRISK)
summary(lm_FS)
res_lm_FS = resid(lm_FS)
res.ADF = ur.df(res_lm_FS, type = "none", selectlags = "AIC")
summary(res.ADF)


####### Causality FRM_q80 and SRISK ############### from FRM_q80 to SRISK ###
lm_SF = lm(data$SRISK ~ data$FRM_q80)
summary(lm_SF)
res_lm_SF = resid(lm_SF)
res.ADF = ur.df(res_lm_SF, type = "none", selectlags = "AIC")
summary(res.ADF)

### from VIX to FRM_q80 ###
lm_FS = lm(data$FRM_q80 ~ data$SRISK)
summary(lm_FS)
res_lm_FS = resid(lm_FS)
res.ADF = ur.df(res_lm_FS, type = "none", selectlags = "AIC")
summary(res.ADF)

####### Causality FRM_q90 and SRISK ############### from FRM_q90 to SRISK ###
lm_SF = lm(data$SRISK ~ data$FRM_q90)
summary(lm_SF)
res_lm_SF = resid(lm_SF)
res.ADF = ur.df(res_lm_SF, type = "none", selectlags = "AIC")
summary(res.ADF)

### from SRISK to FRM_q90 ###
lm_FS = lm(data$FRM_q90 ~ data$SRISK)
summary(lm_FS)
res_lm_FS = resid(lm_FS)
res.ADF = ur.df(res_lm_FS, type = "none", selectlags = "AIC")
summary(res.ADF)


####### Causality FRM_iqr and SRISK ############### from FRM_iqr to SRISK ###
lm_SF = lm(data$SRISK ~ data$FRM_iqr)
summary(lm_SF)
res_lm_SF = resid(lm_SF)
res.ADF = ur.df(res_lm_SF, type = "none", selectlags = "AIC")
summary(res.ADF)

### from SRISK to FRM_iqr ###
lm_FS = lm(data$FRM_iqr ~ data$SRISK)
summary(lm_FS)
res_lm_FS = resid(lm_FS)
res.ADF = ur.df(res_lm_FS, type = "none", selectlags = "AIC")
summary(res.ADF)

####### Causality DFRM and DSRISK, for not co-integrated series ###############
adf.test(diff((data$SRISK)))
adf.test(diff((data$FRM_index)))
adf.test(diff((data$FRM_q50)))


##### Causality DFRM and DSRISK #############
VAR = cbind(diff((data$FRM_index)), diff((data$SRISK)))
colnames(VAR) = c("DFRM", "DSRISK")
VARselect(VAR, lag.max = 20, type = "both")
varest1 = VAR(VAR, p = 13, type = "both")
resid = residuals(varest1)
serial.test(varest1, lags.pt = 20, type = "PT.asymptotic")
serial.test(varest1, lags.pt = 20, type = "PT.adjusted")
serial.test(varest1, lags.pt = 20, type = "BG")
serial.test(varest1, lags.pt = 20, type = "ES")
causality(varest1, cause = "DFRM")$Granger
causality(varest1, cause = "DSRISK")$Granger
acf(resid, ylab = "", cex.axis = 2, lwd = 4, xlab = "", cex.main = 1.6)[1]

##### Causality DFRM_q50 and DSRISK #############
VAR = cbind(diff((data$FRM_q50)), diff((data$SRISK)))
colnames(VAR) = c("DFRM", "DSRISK")
VARselect(VAR, lag.max = 20, type = "both")
varest1 = VAR(VAR, p = 11, type = "both")
resid = residuals(varest1)
serial.test(varest1, lags.pt = 20, type = "PT.asymptotic")
serial.test(varest1, lags.pt = 20, type = "PT.adjusted")
serial.test(varest1, lags.pt = 20, type = "BG")
serial.test(varest1, lags.pt = 20, type = "ES")
causality(varest1, cause = "DFRM")$Granger
causality(varest1, cause = "DSRISK")$Granger
acf(resid, ylab = "", cex.axis = 2, lwd = 4, xlab = "", cex.main = 1.6)[1]


############################################################# Causality FRM_index and GT ###############
VAR = data[, c("FRM_index", "GT")]

VARselect(VAR, lag.max = 20, type = "both")
varest1 = VAR(VAR, p = 14, type = "both")
resid = residuals(varest1)
# check first order autocorrelation among residuals.
serial.test(varest1, lags.pt = 20, type = "PT.asymptotic")
serial.test(varest1, lags.pt = 20, type = "PT.adjusted")
serial.test(varest1, lags.pt = 20, type = "BG")
serial.test(varest1, lags.pt = 20, type = "ES")

causality(varest1, cause = "FRM_index")$Granger
causality(varest1, cause = "GT")$Granger
acf(resid, ylab = "", cex.axis = 2, lwd = 4, xlab = "", cex.main = 1.6)[1]

####### Causality FRM_q50 and GT ###############
VAR = data[, c("FRM_q50", "GT")]

VARselect(VAR, lag.max = 20, type = "both")
varest1 = VAR(VAR, p = 13, type = "both")
resid = residuals(varest1)
# ckeck first order autocorrelation among residuals.
serial.test(varest1, lags.pt = 20, type = "PT.asymptotic")
serial.test(varest1, lags.pt = 20, type = "PT.adjusted")
serial.test(varest1, lags.pt = 20, type = "BG")
serial.test(varest1, lags.pt = 20, type = "ES")

causality(varest1, cause = "FRM_q50")$Granger
causality(varest1, cause = "GT")$Granger
acf(resid, ylab = "", cex.axis = 2, lwd = 4, xlab = "", cex.main = 1.6)[1]

####### Causality FRM_q60 and GT ###############
VAR = data[, c("FRM_q60", "GT")]

VARselect(VAR, lag.max = 20, type = "both")
varest1 = VAR(VAR, p = 14, type = "both")
resid = residuals(varest1)
# ckeck first order autocorrelation among residuals.
serial.test(varest1, lags.pt = 20, type = "PT.asymptotic")
serial.test(varest1, lags.pt = 20, type = "PT.adjusted")
serial.test(varest1, lags.pt = 20, type = "BG")
serial.test(varest1, lags.pt = 20, type = "ES")

causality(varest1, cause = "FRM_q60")$Granger
causality(varest1, cause = "GT")$Granger
acf(resid, ylab = "", cex.axis = 2, lwd = 4, xlab = "", cex.main = 1.6)[1]

####### Causality FRM_q70 and GT ###############
VAR = data[, c("FRM_q70", "GT")]

VARselect(VAR, lag.max = 20, type = "both")
varest1 = VAR(VAR, p = 13, type = "both")
resid = residuals(varest1)
# ckeck first order autocorrelation among residuals.
serial.test(varest1, lags.pt = 20, type = "PT.asymptotic")
serial.test(varest1, lags.pt = 20, type = "PT.adjusted")
serial.test(varest1, lags.pt = 20, type = "BG")
serial.test(varest1, lags.pt = 20, type = "ES")

causality(varest1, cause = "FRM_q70")$Granger
causality(varest1, cause = "GT")$Granger
acf(resid, ylab = "", cex.axis = 2, lwd = 4, xlab = "", cex.main = 1.6)[1]

####### Causality FRM_q80 and GT ###############
VAR = data[, c("FRM_q80", "GT")]
VARselect(VAR, lag.max = 20, type = "both")
varest1 = VAR(VAR, p = 15, type = "both")
resid = residuals(varest1)
# ckeck first order autocorrelation among residuals.
serial.test(varest1, lags.pt = 20, type = "PT.asymptotic")
serial.test(varest1, lags.pt = 20, type = "PT.adjusted")
serial.test(varest1, lags.pt = 20, type = "BG")
serial.test(varest1, lags.pt = 20, type = "ES")

causality(varest1, cause = "FRM_q80")$Granger
causality(varest1, cause = "GT")$Granger
acf(resid, ylab = "", cex.axis = 2, lwd = 4, xlab = "", cex.main = 1.6)[1]

####### Causality FRM_q90 and GT ###############
VAR = data[, c("FRM_q90", "GT")]

VARselect(VAR, lag.max = 20, type = "both")
varest1 = VAR(VAR, p = 14, type = "both")
resid = residuals(varest1)
# ckeck first order autocorrelation among residuals.
serial.test(varest1, lags.pt = 20, type = "PT.asymptotic")
serial.test(varest1, lags.pt = 20, type = "PT.adjusted")
serial.test(varest1, lags.pt = 20, type = "BG")
serial.test(varest1, lags.pt = 20, type = "ES")

causality(varest1, cause = "FRM_q90")$Granger
causality(varest1, cause = "GT")$Granger
acf(resid, ylab = "", cex.axis = 2, lwd = 4, xlab = "", cex.main = 1.6)[1]

####### Causality FRM_iqr and GT ###############
VAR = data[, c("FRM_iqr", "GT")]

VARselect(VAR, lag.max = 20, type = "both")
varest1 = VAR(VAR, p = 14, type = "both")
resid = residuals(varest1)
# ckeck first order autocorrelation among residuals.
serial.test(varest1, lags.pt = 20, type = "PT.asymptotic")
serial.test(varest1, lags.pt = 20, type = "PT.adjusted")
serial.test(varest1, lags.pt = 20, type = "BG")
serial.test(varest1, lags.pt = 20, type = "ES")

causality(varest1, cause = "FRM_iqr")$Granger
causality(varest1, cause = "GT")$Granger
acf(resid, ylab = "", cex.axis = 2, lwd = 4, xlab = "", cex.main = 1.6)[1]


############################################################# Causality FRM_index and FT ###############
VAR = data[, c("FRM_index", "FT")]
VARselect(VAR, lag.max = 20, type = "both")
varest1 = VAR(VAR, p = 11, type = "both")
resid = residuals(varest1)
# check first order autocorrelation among residuals.
serial.test(varest1, lags.pt = 20, type = "PT.asymptotic")
serial.test(varest1, lags.pt = 20, type = "PT.adjusted")
serial.test(varest1, lags.pt = 20, type = "BG")
serial.test(varest1, lags.pt = 20, type = "ES")

causality(varest1, cause = "FRM_index")$Granger
causality(varest1, cause = "FT")$Granger
acf(resid, ylab = "", cex.axis = 2, lwd = 4, xlab = "", cex.main = 1.6)[1]

####### Causality FRM_q50 and FT ###############
VAR = data[, c("FRM_q50", "FT")]
VARselect(VAR, lag.max = 20, type = "both")
varest1 = VAR(VAR, p = 12, type = "both")
resid = residuals(varest1)
# ckeck first order autocorrelation among residuals.
serial.test(varest1, lags.pt = 20, type = "PT.asymptotic")
serial.test(varest1, lags.pt = 20, type = "PT.adjusted")
serial.test(varest1, lags.pt = 20, type = "BG")
serial.test(varest1, lags.pt = 20, type = "ES")

causality(varest1, cause = "FRM_q50")$Granger
causality(varest1, cause = "FT")$Granger
acf(resid, ylab = "", cex.axis = 2, lwd = 4, xlab = "", cex.main = 1.6)[1]

####### Causality FRM_q60 and GT ###############
VAR = data[, c("FRM_q60", "FT")]
VARselect(VAR, lag.max = 20, type = "both")
varest1 = VAR(VAR, p = 11, type = "both")
resid = residuals(varest1)
# ckeck first order autocorrelation among residuals.
serial.test(varest1, lags.pt = 20, type = "PT.asymptotic")
serial.test(varest1, lags.pt = 20, type = "PT.adjusted")
serial.test(varest1, lags.pt = 20, type = "BG")
serial.test(varest1, lags.pt = 20, type = "ES")

causality(varest1, cause = "FRM_q60")$Granger
causality(varest1, cause = "FT")$Granger
acf(resid, ylab = "", cex.axis = 2, lwd = 4, xlab = "", cex.main = 1.6)[1]

####### Causality FRM_q70 and FT ###############
VAR = data[, c("FRM_q70", "FT")]
VARselect(VAR, lag.max = 20, type = "both")
varest1 = VAR(VAR, p = 11, type = "both")
resid = residuals(varest1)
# ckeck first order autocorrelation among residuals.
serial.test(varest1, lags.pt = 20, type = "PT.asymptotic")
serial.test(varest1, lags.pt = 20, type = "PT.adjusted")
serial.test(varest1, lags.pt = 20, type = "BG")
serial.test(varest1, lags.pt = 20, type = "ES")

causality(varest1, cause = "FRM_q70")$Granger
causality(varest1, cause = "FT")$Granger
acf(resid, ylab = "", cex.axis = 2, lwd = 4, xlab = "", cex.main = 1.6)[1]

####### Causality FRM_q80 and FT ###############
VAR = data[, c("FRM_q80", "FT")]
VARselect(VAR, lag.max = 20, type = "both")
varest1 = VAR(VAR, p = 13, type = "both")
resid = residuals(varest1)
# ckeck first order autocorrelation among residuals.
serial.test(varest1, lags.pt = 20, type = "PT.asymptotic")
serial.test(varest1, lags.pt = 20, type = "PT.adjusted")
serial.test(varest1, lags.pt = 20, type = "BG")
serial.test(varest1, lags.pt = 20, type = "ES")

causality(varest1, cause = "FRM_q80")$Granger
causality(varest1, cause = "FT")$Granger
acf(resid, ylab = "", cex.axis = 2, lwd = 4, xlab = "", cex.main = 1.6)[1]

####### Causality FRM_q90 and FT ###############
VAR = data[, c("FRM_q90", "FT")]
VARselect(VAR, lag.max = 20, type = "both")
varest1 = VAR(VAR, p = 5, type = "both")
resid = residuals(varest1)
# ckeck first order autocorrelation among residuals.
serial.test(varest1, lags.pt = 20, type = "PT.asymptotic")
serial.test(varest1, lags.pt = 20, type = "PT.adjusted")
serial.test(varest1, lags.pt = 20, type = "BG")
serial.test(varest1, lags.pt = 20, type = "ES")

causality(varest1, cause = "FRM_q90")$Granger
causality(varest1, cause = "FT")$Granger
acf(resid, ylab = "", cex.axis = 2, lwd = 4, xlab = "", cex.main = 1.6)[1]

####### Causality FRM_iqr and FT ###############
VAR = data[, c("FRM_iqr", "FT")]
VARselect(VAR, lag.max = 20, type = "both")
varest1 = VAR(VAR, p = 5, type = "both")
resid = residuals(varest1)
# ckeck first order autocorrelation among residuals.
serial.test(varest1, lags.pt = 20, type = "PT.asymptotic")
serial.test(varest1, lags.pt = 20, type = "PT.adjusted")
serial.test(varest1, lags.pt = 20, type = "BG")
serial.test(varest1, lags.pt = 20, type = "ES")

causality(varest1, cause = "FRM_iqr")$Granger
causality(varest1, cause = "FT")$Granger
acf(resid, ylab = "", cex.axis = 2, lwd = 4, xlab = "", cex.main = 1.6)[1]


