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
