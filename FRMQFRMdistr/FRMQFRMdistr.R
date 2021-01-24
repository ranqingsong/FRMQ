## Creating density distribution of standard FRM index through kernel density estimation## clean environment
rm(list = ls(all = TRUE))
graphics.off()

# set wd
setwd("")

# install and load packages
libraries = c("RColorBrewer")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
    install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

# settings
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
AM_data1 = inputcsv(date_start1, date_end1, channel)
AM_data2 = inputcsv(date_start2, date_end2, channel)
EU_data1 = inputcsv(date_start1, date_end1, channel2)
EU_data2 = inputcsv(date_start2, date_end2, channel2)

# create plots
par(mfrow = c(7, 2))
cols = brewer.pal(9, "Set1")
plot(density(EU_data1$FRM_index, kernel = "gaussian"), main = paste("from", date_start1, "to", date_end1), xlab = "", ylab = "", cex.axis = 3, 
    cex.lab = 3, lwd = 2, cex.main = 3, xlim = c(0, max(AM_data1$FRM_q90)))
lines(density(AM_data1$FRM_index, kernel = "gaussian"), col = "black", lwd = 3, lty = 2)
plot(density(EU_data2$FRM_index, kernel = "gaussian"), main = paste("from", date_start2, "to", date_end2), xlab = "", ylab = "", cex.axis = 3, 
    cex.lab = 3, lwd = 2, cex.main = 3, xlim = c(0, max(AM_data2$FRM_q90)))
lines(density(AM_data2$FRM_index, kernel = "gaussian"), col = "black", lwd = 3, lty = 2)

plot(density(EU_data1$FRM_q50, kernel = "gaussian"), main = "", xlab = "", ylab = "", cex.axis = 3, cex.lab = 3, lwd = 2, col = "darkred", 
    xlim = c(0, max(AM_data1$FRM_q90)))
lines(density(AM_data1$FRM_q50, kernel = "gaussian"), col = "darkred", lwd = 2, lty = 2)
plot(density(EU_data2$FRM_q50, kernel = "gaussian"), main = "", xlab = "", ylab = "", cex.axis = 3, cex.lab = 3, col = "darkred", lwd = 2, 
    xlim = c(0, max(AM_data2$FRM_q90)))
lines(density(AM_data2$FRM_q50, kernel = "gaussian"), col = "darkred", lwd = 2, lty = 2)

plot(density(EU_data1$FRM_q60, kernel = "gaussian"), main = "", xlab = "", ylab = "", cex.axis = 3, cex.lab = 3, col = cols[2], lwd = 2, 
    xlim = c(0, max(AM_data1$FRM_q90)))
lines(density(AM_data1$FRM_q60, kernel = "gaussian"), col = cols[2], lwd = 2, lty = 2)
plot(density(EU_data2$FRM_q60, kernel = "gaussian"), main = "", xlab = "", ylab = "", cex.axis = 3, cex.lab = 3, lwd = 2, col = cols[2], 
    xlim = c(0, max(AM_data2$FRM_q90)))
lines(density(AM_data2$FRM_q60, kernel = "gaussian"), col = cols[2], lwd = 2, lty = 2)

plot(density(EU_data1$FRM_q70, kernel = "gaussian"), main = "", xlab = "", ylab = "", cex.axis = 3, cex.lab = 3, lwd = 2, col = cols[3], 
    xlim = c(0, max(AM_data1$FRM_q90)))
lines(density(AM_data1$FRM_q70, kernel = "gaussian"), col = cols[3], lwd = 2, lty = 2)

plot(density(EU_data2$FRM_q70, kernel = "gaussian"), main = "", xlab = "", ylab = "", cex.axis = 3, cex.lab = 3, lwd = 2, col = cols[3], 
    xlim = c(0, max(AM_data2$FRM_q90)))
lines(density(AM_data2$FRM_q70, kernel = "gaussian"), col = cols[3], lwd = 2, lty = 2)

plot(density(EU_data1$FRM_q80, kernel = "gaussian"), main = "", xlab = "", ylab = "", cex.axis = 3, cex.lab = 3, lwd = 2, col = cols[4], 
    xlim = c(0, max(AM_data1$FRM_q90)))
lines(density(AM_data1$FRM_q80, kernel = "gaussian"), col = cols[4], lwd = 2, lty = 2)
plot(density(EU_data2$FRM_q80, kernel = "gaussian"), main = "", xlab = "", ylab = "", cex.axis = 3, cex.lab = 3, lwd = 2, col = cols[4], 
    xlim = c(0, max(AM_data2$FRM_q90)))
lines(density(AM_data2$FRM_q80, kernel = "gaussian"), col = cols[4], lwd = 2, lty = 6)

plot(density(EU_data1$FRM_q90, kernel = "gaussian"), main = "", xlab = "", ylab = "", cex.axis = 3, cex.lab = 3, lwd = 2, col = cols[5], 
    xlim = c(0, max(AM_data1$FRM_q90)))
lines(density(AM_data1$FRM_q90, kernel = "gaussian"), col = cols[5], lwd = 2, lty = 2)
plot(density(EU_data2$FRM_q90, kernel = "gaussian"), main = "", xlab = "", ylab = "", cex.axis = 3, cex.lab = 3, lwd = 2, col = cols[5], 
    xlim = c(0, max(AM_data2$FRM_q90)))
lines(density(AM_data2$FRM_q90, kernel = "gaussian"), col = cols[5], lwd = 2, lty = 2)

plot(density(EU_data1$FRM_iqr, kernel = "gaussian"), main = "", xlab = "", ylab = "", cex.axis = 3, cex.lab = 3, col = cols[7], lwd = 2)
lines(density(AM_data1$FRM_iqr, kernel = "gaussian"), col = cols[7], lwd = 2, lty = 2)
plot(density(AM_data2$FRM_iqr, kernel = "gaussian"), main = "", xlab = "", ylab = "", cex.axis = 3, cex.lab = 3, col = cols[7], lwd = 2, 
    lty = 2)
lines(density(EU_data2$FRM_iqr, kernel = "gaussian"), col = cols[7], lwd = 2)


# for simplicity: compare plots for standard FRM and FRM_q90
par(mfrow = c(2, 2))
plot(density(EU_data1$FRM_index, kernel = "gaussian"), main = paste("from", date_start1, "to", date_end1), xlab = "", ylab = "", cex.axis = 3, 
    cex.lab = 3, lwd = 2, cex.main = 3, xlim = c(0, max(AM_data1$FRM_q90)))
lines(density(AM_data1$FRM_index, kernel = "gaussian"), col = "black", lwd = 3, lty = 2)
plot(density(EU_data2$FRM_index, kernel = "gaussian"), main = paste("from", date_start2, "to", date_end2), xlab = "", ylab = "", cex.axis = 3, 
    cex.lab = 3, lwd = 2, cex.main = 3, xlim = c(0, max(AM_data2$FRM_q90)))
lines(density(AM_data2$FRM_index, kernel = "gaussian"), col = "black", lwd = 3, lty = 2)
plot(density(EU_data1$FRM_q90, kernel = "gaussian"), main = "", xlab = "", ylab = "", cex.axis = 3, cex.lab = 3, lwd = 2, col = cols[5], 
    xlim = c(0, max(AM_data1$FRM_q90)))
lines(density(AM_data1$FRM_q90, kernel = "gaussian"), col = cols[5], lwd = 2, lty = 2)
plot(density(EU_data2$FRM_q90, kernel = "gaussian"), main = "", xlab = "", ylab = "", cex.axis = 3, cex.lab = 3, lwd = 2, col = cols[5], 
    xlim = c(0, max(AM_data2$FRM_q90)))
lines(density(AM_data2$FRM_q90, kernel = "gaussian"), col = cols[5], lwd = 2, lty = 2)




