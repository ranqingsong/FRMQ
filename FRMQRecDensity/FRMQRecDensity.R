#### Creating Plots of lambda distribution for each moving window####

# clean environment
rm(list = ls(all = TRUE))

# setwd()
setwd("")

# isntall and load packages
libraries = c("dplyr", "RColorBrewer", "goft")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
    install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

# import estimated lambda
date_start1 = 20070402
date_end1 = 20091231
date_start2 = 20190101
date_end2 = 20201231
channel = "Americas"
channel2 = "Europe"

# import csv files containing daily lambda
data_input <- function(date_start, date_end, channel) {
    lambdas_wide <- read.csv(paste0("lambdas_wide_", date_start, "-", date_end, channel, ".csv"))
    lambdas_wide_t <- as.data.frame(t(as.matrix(lambdas_wide)))
    lambdas_wide_t <- sapply(1:ncol(lambdas_wide_t), function(i) as.numeric(lambdas_wide_t[, i]))
    rownames(lambdas_wide_t) <- colnames(lambdas_wide)
    colnames(lambdas_wide_t) <- paste0("date", lambdas_wide[, 1])
    lambdas_wide_t <- lambdas_wide_t[-c(1), ]
    return(data.frame(lambdas_wide_t))
}

AM_data1 <- data_input(date_start1, date_end1, channel)
AM_data2 <- data_input(date_start2, date_end2, channel)
EU_data1 <- data_input(date_start1, date_end1, channel2)
EU_data2 <- data_input(date_start2, date_end2, channel2)

#### overlaying plot to compare lambdas during recession and expansion: kernel density estimation with gaussian kernel
par(mfrow = c(2, 2))
# comparing the overlaying plot during recession and expansion for Americas during expansion - financial crisis of 2008
plot(density(AM_data1[, 1], na.rm = T, kernel = "gaussian"), type = "h", ylim = c(0, 100), xlim = c(0, 0.2), main = "", xlab = "lambdas before and after financial crisis of 2008", 
    cex.lab = 2, cex.axis = 2, ylab = "")
for (i in c(2:196, 588:dim(AM_data1)[2])) {
    lines(density(AM_data1[, i], na.rm = T, kernel = "gaussian"), type = "h")
}
# during recession - financial crisis of 2008
plot(density(AM_data1[, 197], na.rm = T, kernel = "gaussian"), type = "h", ylim = c(0, 100), xlim = c(0, 0.2), main = "", xlab = "labmdas during fincancial crisis of 2008", 
    col = "gray", cex.lab = 2, cex.axis = 2, ylab = "")
for (i in 198:587) {
    lines(density(AM_data1[, i], na.rm = T, kernel = "gaussian"), type = "h", col = "gray")
}
# during expansion - COVID-19 pandemic
plot(density(AM_data2[, 1], na.rm = T, kernel = "gaussian"), type = "h", ylim = c(0, 100), xlim = c(0, 0.2), main = "", xlab = "lambdas before Covid-19 pandemic", 
    cex.lab = 2, cex.axis = 2, ylab = "")
for (i in 2:304) {
    lines(density(AM_data2[, i], na.rm = T, kernel = "gaussian"), type = "h")
}
# during recession- COVID-19 pandemic
plot(density(AM_data2[, 305], na.rm = T, kernel = "gaussian"), type = "h", ylim = c(0, 100), xlim = c(0, 0.2), main = "", xlab = "lambdas during Covid-10 pandemic", 
    col = "gray", cex.lab = 2, cex.axis = 2, ylab = "")
for (i in 306:dim(AM_data2)[2]) {
    lines(density(AM_data2[, i], na.rm = T, kernel = "gaussian"), type = "h", col = "gray")
}

# comparing the overlaying plot during recession and expansion for Europe during expansion - financial crisis of 2008
par(mfrow = c(2, 2))
plot(density(EU_data1[, 1], na.rm = T, kernel = "gaussian"), type = "h", ylim = c(0, 100), xlim = c(0, 0.2), main = "", xlab = "lambdas before and after financial crisis of 2008", 
    cex.lab = 2, cex.axis = 2, ylab = "")
for (i in c(2:261, 588:dim(EU_data1)[2])) {
    lines(density(EU_data1[, i], na.rm = T, kernel = "gaussian"), type = "h")
}
# during recession - financial crisis of 2008
plot(density(EU_data1[, 262], na.rm = T, kernel = "gaussian"), type = "h", ylim = c(0, 100), xlim = c(0, 0.2), main = "", xlab = "lambdas during financial crisis of 2008", 
    col = "gray", cex.lab = 2, cex.axis = 2, ylab = "")
for (i in 263:587) {
    lines(density(EU_data1[, i], na.rm = T, kernel = "gaussian"), type = "h", col = "gray")
}
# during expansion - COVID-19 pandemic
plot(density(EU_data2[, 1], na.rm = T, kernel = "gaussian"), type = "h", ylim = c(0, 100), xlim = c(0, 0.2), main = "", xlab = "lambdas before Covid-19 pandemic", 
    cex.lab = 2, cex.axis = 2, ylab = "")
for (i in 2:261) {
    lines(density(EU_data1[, i], na.rm = T, kernel = "gaussian"), type = "h")
}
# during recession - COVID-19 pandemic
plot(density(EU_data2[, 262], na.rm = T, kernel = "gaussian"), type = "h", ylim = c(0, 100), xlim = c(0, 0.2), main = "", xlab = "lambdas during Covid-19 pandemic", 
    col = "gray", cex.lab = 2, cex.axis = 2, ylab = "")
for (i in 263:dim(EU_data2)[2]) {
    lines(density(EU_data2[, i], na.rm = T, kernel = "gaussian"), type = "h", col = "gray")
}

# input datasets
inputcsv = function(date_start, date_end, channel) {
    read.csv(paste0("FRM_index_all", date_start, "-", date_end, channel, ".csv"), colClasses = c("Date", "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", "numeric"))
}
AM_frm1 = inputcsv(date_start1, date_end1, channel)
AM_frm2 = inputcsv(date_start2, date_end2, channel)
EU_frm1 = inputcsv(date_start1, date_end1, channel2)
EU_frm2 = inputcsv(date_start2, date_end2, channel2)

# create plots for FRM_mean during recession and normal periods
par(mfrow = c(2, 2))
plot(density(EU_frm1[262:587, ]$FRM_index, kernel = "gaussian"), main = paste("during recession from", date_start1, "to", date_end1), 
    xlab = "", ylab = "", xlim = c(0, 0.15), cex.lab = 2, cex.axis = 2, cex.main = 2)
lines(density(AM_frm1[197:587, ]$FRM_index, kernel = "gaussian"), lty = 2, xlim = c(0, 0.15))
plot(density(EU_frm1[c(1:261, 588:719), ]$FRM_index, kernel = "gaussian"), ylim = c(0, 60), main = paste("during expansion", date_start1, 
    "to", date_end1), xlab = "", ylab = "", xlim = c(0, 0.15), cex.lab = 2, cex.axis = 2, cex.main = 2)
lines(density(AM_frm1[c(1:196, 588:719), ]$FRM_index, kernel = "gaussian"), lty = 2, xlim = c(0, 0.15))

plot(density(EU_frm2[262:523, ]$FRM_index, kernel = "gaussian"), main = paste("during recession from", date_start2, "to", date_end2), 
    xlab = "", ylab = "", xlim = c(0, 0.15), cex.lab = 2, cex.axis = 2, cex.main = 2)
lines(density(AM_frm2[305:523, ]$FRM_index, kernel = "gaussian"), lty = 2, xlim = c(0, 0.15))
plot(density(EU_frm2[c(1:261), ]$FRM_index, kernel = "gaussian"), ylim = c(0, 200), main = paste("during expansion", date_start2, "to", 
    date_end2), xlab = "", ylab = "", xlim = c(0, 0.15), cex.lab = 2, cex.axis = 2, cex.main = 2)
lines(density(AM_frm2[c(1:304), ]$FRM_index, kernel = "gaussian"), lty = 2, xlim = c(0, 0.15))
