# uniperiod recession models

# clean environment
rm(list = ls(all = TRUE))

# setwd()
setwd("")

# install and load packages
libraries = c("xtable", "dplyr", "lubridate", "texreg", "RColorBrewer", "zoo", "pscl", "pROC")
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

## function to run uniperiod regression for each FRM candidate
rec_model = function(channel, lag, type) {
    ## import lagged FRM datasets
    data_m = read.csv(paste0("lagged_FRM_index_all", date_start1, "-", date_end1, channel, ".csv"))
    data_m2 = read.csv(paste0("lagged_FRM_index_all", date_start2, "-", date_end2, channel, ".csv"))
    data_m = rbind(data_m, data_m2)
    data_m$ym = as.yearmon(data_m$ym)
    data_m = cbind(data_m[, substr(colnames(data_m), 1, 4) %in% c(lag)], rec = data_m$rec, ym = data_m$ym)
    data_m = na.omit(data_m)
    model_list = list()
    # run probit regression along lagged FRM candidates
    for (i in 1:7) {
        mod = as.formula(paste("rec~", names(data_m[, c(1:7)])[i]))
        recmodel = glm(mod, family = binomial(link = type), data = data_m)
        model_list[[i]] = recmodel
    }
    names(model_list) = c("mean", "q50", "q60", "q70", "q80", "q90", "iqr")
    return(model_list)
}
# Estrella Pseudo R-square
fitness = function(AM_lag1_output, channel) {
    # import data from two periods
    data_m = read.csv(paste0("lagged_FRM_index_all", date_start1, "-", date_end1, channel, ".csv"))
    data_m2 = read.csv(paste0("lagged_FRM_index_all", date_start2, "-", date_end2, channel, ".csv"))
    data_m = rbind(data_m, data_m2)
    estrella = matrix(NA, 1, 7)
    # the log-likelihood from the intercept-only model
    lhnull = logLik(glm(rec ~ 1, family = binomial(link = "probit"), data = data_m))
    for (i in 1:7) {
        # the log-likelihood from the fitted model
        lh = logLik(AM_lag1_output[[i]])
        # the number of observation
        n = dim(AM_lag1_output[[i]]$data)[1]
        # calculate R2 for each regression
        estrella[1, i] = 1 - (lh/lhnull)^(-2/n * lhnull)
    }
    colnames(estrella) = names(AM_lag1_output)
    return(estrella)
}

# probit regressions for FRM@Americas
AM_lag1_output = rec_model(channel, "lag1", "probit")
texreg::screenreg(AM_lag1_output)
fitness(AM_lag1_output, channel)
texreg(AM_lag1_output)
AM_lag2_output = rec_model(channel, "lag2", "probit")
texreg::screenreg(AM_lag2_output)
fitness(AM_lag2_output, channel)
AM_lag3_output = rec_model(channel, "lag3", "probit")
texreg::screenreg(AM_lag3_output)
fitness(AM_lag3_output, channel)
AM_lag4_output = rec_model(channel, "lag4", "probit")
texreg::screenreg(AM_lag4_output)
fitness(AM_lag4_output, channel)
AM_lag5_output = rec_model(channel, "lag5", "probit")
texreg::screenreg(AM_lag5_output)
AM_lag6_output = rec_model(channel, "lag6", "probit")
texreg::screenreg(AM_lag6_output)

# probit regressions for FRM@Europe
EU_lag1_output = rec_model(channel2, "lag1", "probit")
texreg::screenreg(EU_lag1_output)
texreg(EU_lag1_output)
fitness(EU_lag1_output, channel2)
EU_lag2_output = rec_model(channel2, "lag2", "probit")
texreg::screenreg(EU_lag2_output)
fitness(EU_lag2_output, channel2)
EU_lag3_output = rec_model(channel2, "lag3", "probit")
texreg::screenreg(EU_lag3_output)
fitness(EU_lag3_output, channel2)
EU_lag4_output = rec_model(channel2, "lag4", "probit")
texreg::screenreg(EU_lag4_output)
fitness(EU_lag4_output, channel2)
EU_lag5_output = rec_model(channel2, "lag5", "probit")
texreg::screenreg(EU_lag5_output)
fitness(EU_lag5_output, channel2)
EU_lag6_output = rec_model(channel2, "lag6", "probit")
texreg::screenreg(EU_lag6_output)
fitness(EU_lag6_output, channel2)

# logistic regressions for FRM@Americas
AM_lag1_outputl = rec_model(channel, "lag1", "logit")
texreg::screenreg(AM_lag1_outputl)
fitness(AM_lag1_outputl, channel)
AM_lag2_outputl = rec_model(channel, "lag2", "logit")
texreg::screenreg(AM_lag2_outputl)
fitness(AM_lag2_outputl, channel)
AM_lag3_outputl = rec_model(channel, "lag3", "logit")
texreg::screenreg(AM_lag3_outputl)
fitness(AM_lag3_outputl, channel)
AM_lag4_outputl = rec_model(channel, "lag4", "logit")
texreg::screenreg(AM_lag4_outputl)
fitness(AM_lag4_outputl, channel)
AM_lag5_outputl = rec_model(channel, "lag5", "logit")
texreg::screenreg(AM_lag5_outputl)
fitness(AM_lag5_outputl, channel)
AM_lag6_outputl = rec_model(channel, "lag6", "logit")
texreg::screenreg(AM_lag6_outputl)
fitness(AM_lag6_outputl, channel)

# logstic regression for FRM@Europe
EU_lag1_outputl = rec_model(channel2, "lag1", "logit")
texreg::screenreg(EU_lag1_outputl)
fitness(EU_lag1_outputl, channel2)
EU_lag2_outputl = rec_model(channel2, "lag2", "logit")
texreg::screenreg(EU_lag2_outputl)
fitness(EU_lag2_outputl, channel2)
EU_lag3_outputl = rec_model(channel2, "lag3", "logit")
texreg::screenreg(EU_lag3_outputl)
fitness(EU_lag3_outputl, channel2)
EU_lag4_outputl = rec_model(channel2, "lag4", "logit")
texreg::screenreg(EU_lag4_outputl)
fitness(EU_lag4_outputl, channel2)
EU_lag5_outputl = rec_model(channel2, "lag5", "logit")
texreg::screenreg(EU_lag5_outputl)
fitness(EU_lag5_outputl, channel2)
EU_lag6_outputl = rec_model(channel2, "lag6", "logit")
texreg::screenreg(EU_lag6_outputl)
fitness(EU_lag6_outputl, channel2)

# estimate implied recession probability
est_rec_prob = function(month_output_list, channel) {
    # extract fitted recession probabilities from previous outputs
    pred_k1 = sapply(1:length(month_output_list), function(num_reg) assign(paste0("pred_", names(month_output_list)[num_reg]), month_output_list[[num_reg]]$fitted.values))
    colnames(pred_k1) = names(month_output_list)
    # join probabilities with date and recession indicators
    pred_k1 = pred_k1 %>% as.data.frame() %>% mutate(n = rownames(pred_k1))
    pred_k1 = cbind(ym = month_output_list[[1]]$data$ym, rec = month_output_list[[1]]$data$rec, pred_k1)
    pred_k1 = arrange(pred_k1, ym)
}

### create plot of implied probability based on combined data
create_plot_two = function(month_output_stepwise, channel, title) {
    # get implied recession probabilities and recession indicators
    pred = est_rec_prob(month_output_stepwise, channel)
    ## plot the data
    cols = brewer.pal(9, "Set1")
    par(mfrow = c(1, 2))
    index = min(which(substr(pred$ym, 5, 8) == "2019"))
    pred_1 = pred[1:index - 1, ]
    pred_2 = pred[index:length(pred$ym), ]
    # plot the result during financial crisis of 2008
    plot(x = pred_1$ym, y = pred_1$rec, type = "n", xaxt = "none", ylab = "Implied recession probability", xlab = "", cex.lab = 1.7, 
        cex.axis = 2)
    rect(xleft = min(pred_1$ym[which(pred_1$rec == 1)]), ybottom = 0, xright = max(pred_1$ym[which(pred_1$rec == 1)]), ytop = 1, col = "gray", 
        border = "#F4F6F6")
    lines(x = pred_1$ym, y = pred_1$q50, col = cols[1], type = "l", lwd = 2)
    lines(x = pred_1$ym, y = pred_1$q60, col = cols[2], lwd = 2)
    lines(x = pred_1$ym, y = pred_1$q90, col = cols[5], lwd = 2)
    lines(x = pred_1$ym, y = pred_1$iqr, col = cols[7], lwd = 3)
    lines(x = pred_1$ym, y = pred_1$q70, col = cols[3], lwd = 3)
    lines(x = pred_1$ym, y = pred_1$q80, col = cols[4], lwd = 3)
    lines(x = pred_1$ym, y = pred_1$mean, col = "black", type = "l", lwd = 3)
    axis(1, at = pred_1$ym, labels = pred_1$ym, font = 1, las = 1, cex.axis = 2)
    
    # plot the result during Covid-19 pandemic
    plot(x = pred_2$ym, y = pred_2$rec, type = "n", xaxt = "none", ylab = "", xlab = "", cex.lab = 1.7, cex.axis = 2)
    rect(xleft = min(pred_2$ym[which(pred_2$rec == 1)]), ybottom = 0, xright = max(pred_2$ym[which(pred_2$rec == 1)]), ytop = 1, col = "gray", 
        border = "#F4F6F6")
    lines(x = pred_2$ym, y = pred_2$q50, col = cols[1], type = "l", lwd = 2)
    lines(x = pred_2$ym, y = pred_2$q60, col = cols[2], lwd = 2)
    lines(x = pred_2$ym, y = pred_2$q90, col = cols[5], lwd = 2)
    lines(x = pred_2$ym, y = pred_2$iqr, col = cols[7], lwd = 3)
    lines(x = pred_2$ym, y = pred_2$q70, col = cols[3], lwd = 3)
    lines(x = pred_2$ym, y = pred_2$q80, col = cols[4], lwd = 3)
    lines(x = pred_2$ym, y = pred_2$mean, col = "black", type = "l", lwd = 3)
    axis(1, at = pred_2$ym, labels = pred_2$ym, font = 1, las = 1, cex.axis = 2)
}
# FRM@Americas k=1, probit regression
create_plot_two(AM_lag1_output, channel, "lag1")
# FRM@Americas k=1, logistic regression
create_plot_two(AM_lag1_outputl, channel, "lag1_logit")
# FRM@Europe k=1, probit regression
create_plot_two(EU_lag1_output, channel2, "lag1")
# FRM@America k=1, logistic regression
create_plot_two(EU_lag1_outputl, channel2, "lag1_logit")

# comparing auc for recession probabilities implied from FRM@Americas lagged at one month
pred = est_rec_prob(AM_lag1_output, channel)
apply(pred[3:9], 2, function(x) {
    roc(pred$rec, x, levels = c(0, 1))
})

# comparing auc for recession probabilities implied from FRM@Europe lagged at one month
pred = est_rec_prob(EU_lag1_output, channel)
apply(pred[3:9], 2, function(x) {
    roc(pred$rec, x, levels = c(0, 1))
})

# transform FRM@Americas into probability of recession based on the model with k=1
FRM_series_AM = seq(0, 0.16, by = 0.01)
output = data.frame(FRM_series_AM)
for (i in 1:7) {
    # input a series of FRM values
    FRM_series_AM = data.frame(FRM_series_AM)
    # determine the name of predictor
    objectname = AM_lag1_output[[i]]$terms[[3]]
    # calculate the recession probability based on the model regressing on determined predictor
    colnames(FRM_series_AM) = deparse(objectname)
    if (objectname != "lag1_FRM_iqr") {
        predicted = predict.glm(AM_lag1_output[[i]], FRM_series_AM, type = "response")
        output = cbind(output, predicted)
    } else {
        predicted = predict.glm(AM_lag1_output[[i]], FRM_series_AM/5, type = "response")
        output = cbind(output, predicted)
    }
}
colnames(output) = c("FRM/FRM_IQR*5", "FRM_mean", "FRM_q50", "FRM_q60", "FRM_q70", "FRM_q80", "FRM_q90", "FRM_IQR")
print(output)

# transform FRM@Europe into the probability of recession based on the model with k=1
FRM_series_EU = seq(0, 0.16, by = 0.01)
output_EU = data.frame(FRM_series_EU)
for (i in 1:7) {
    # input a series of FRM values
    FRM_series_EU = data.frame(FRM_series_EU)
    # determine the name of predictor
    objectname = EU_lag1_output[[i]]$terms[[3]]
    # calculate the recession probability based on the model regressing on determined predictor
    colnames(FRM_series_EU) = deparse(objectname)
    if (objectname != "lag1_FRM_iqr") {
        predicted = predict.glm(EU_lag1_output[[i]], FRM_series_EU, type = "response")
        output_EU = cbind(output_EU, predicted)
    } else {
        predicted = predict.glm(EU_lag1_output[[i]], FRM_series_EU/5, type = "response")
        output_EU = cbind(output_EU, predicted)
    }
}
colnames(output_EU) = c("FRM/FRM_IQR*5", "FRM_mean", "FRM_q50", "FRM_q60", "FRM_q70", "FRM_q80", "FRM_q90", "FRM_IQR")
print(output_EU)

# plots for recession probabilities implied from FRMs with k=2,3,4
create_plot_sum = function(month_output_stepwise_list, channel, title) {
    par(mfrow = c(length(month_output_stepwise_list), 2))
    for (i in 1:length(month_output_stepwise_list)) {
        pred = est_rec_prob(month_output_stepwise_list[[i]], channel)
        cols = brewer.pal(9, "Set1")
        index = min(which(substr(pred$ym, 5, 8) == "2019"))
        pred_1 = pred[1:index - 1, ]
        pred_2 = pred[index:length(pred$ym), ]
        plot(x = pred_1$ym, y = pred_1$rec, type = "n", xaxt = "none", ylab = "Implied recession probability", xlab = "", cex.lab = 1.5, 
            cex.axis = 2)
        rect(xleft = min(pred_1$ym[which(pred_1$rec == 1)]), ybottom = 0, xright = max(pred_1$ym[which(pred_1$rec == 1)]), ytop = 1, 
            col = "gray", border = "#F4F6F6")
        lines(x = pred_1$ym, y = pred_1$q50, col = cols[1], type = "l", lwd = 2)
        lines(x = pred_1$ym, y = pred_1$q60, col = cols[2], lwd = 2)
        lines(x = pred_1$ym, y = pred_1$q90, col = cols[5], lwd = 2)
        lines(x = pred_1$ym, y = pred_1$iqr, col = cols[7], lwd = 3)
        lines(x = pred_1$ym, y = pred_1$q70, col = cols[3], lwd = 3)
        lines(x = pred_1$ym, y = pred_1$q80, col = cols[4], lwd = 3)
        lines(x = pred_1$ym, y = pred_1$mean, col = "black", type = "l", lwd = 3)
        title(sub = paste(""))
        axis(1, at = pred_1$ym, labels = pred_1$ym, font = 1, las = 1, cex.axis = 2)
        
        plot(x = pred_2$ym, y = pred_2$rec, type = "n", xaxt = "none", ylab = "", xlab = "", cex.lab = 1.5, cex.axis = 2)
        rect(xleft = min(pred_2$ym[which(pred_2$rec == 1)]), ybottom = 0, xright = max(pred_2$ym[which(pred_2$rec == 1)]), ytop = 1, 
            col = "gray", border = "#F4F6F6")
        lines(x = pred_2$ym, y = pred_2$q50, col = cols[1], type = "l", lwd = 2)
        lines(x = pred_2$ym, y = pred_2$q60, col = cols[2], lwd = 2)
        lines(x = pred_2$ym, y = pred_2$q90, col = cols[5], lwd = 2)
        lines(x = pred_2$ym, y = pred_2$iqr, col = cols[7], lwd = 3)
        lines(x = pred_2$ym, y = pred_2$q70, col = cols[3], lwd = 3)
        lines(x = pred_2$ym, y = pred_2$q80, col = cols[4], lwd = 3)
        lines(x = pred_2$ym, y = pred_2$mean, col = "black", type = "l", lwd = 3)
        axis(1, at = pred_2$ym, labels = pred_2$ym, font = 1, las = 1, cex.axis = 2)
    }
    
}
month_output_stepwise_list = list(AM_lag2_output, AM_lag3_output, AM_lag4_output)
month_output_stepwise_list_2 = list(EU_lag2_output, EU_lag3_ouput)

create_plot_sum(month_output_stepwise_list, channel, "lag2_to_lag4")
create_plot_sum(month_output_stepwise_list_2, channel2, "lag2_to_lag3")
