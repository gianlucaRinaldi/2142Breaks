###################################################################################
# Here we see if high frequency data for last 6 months gives different results
# load daily measures for last 180 days
require(highfrequency)
require(quantmod)
require(qmao)

corr_meas <- merge(DCCcorr_measures,WINcorr_measures,by="date") # drop dates for which we don't have one of the measures
recent_corr <- tail(corr_meas,134)
# import data
Sys.timezone(location = TRUE)
stock_dat <- read.csv("sp500.csv",skip =1, header = TRUE, stringsAsFactors = FALSE)
bond_dat <- read.csv("treasury10y.csv",skip =1,header = TRUE, stringsAsFactors = FALSE)

stock_date <- strptime(as.character(stock_dat$Date),format="%m/%d/%Y %H:%M")
stock_open <- xts(stock_dat$OPEN,stock_date) 

bond_date <- strptime(as.character(bond_dat$Date), format = "%m/%d/%Y %H:%M") # "%m/%d/%Y" SLOW to get everything
bond_open <- xts(bond_dat$OPEN, bond_date) 

stock_log_dr <- tail(diff(log(stock_open)),-1) # Stock daily returns (remove 1st ob)
bond_log_dr <- tail(-diff(log(bond_open+100)),-1) # Remove first observation of log daily bond returns
bond_log_dr <- bond_log_dr[!is.na(index(bond_log_dr))] 

minute_returns <- merge.xts(stock_log_dr,bond_log_dr,join = "inner")
minute_returns <- ExcludeDates(minute_returns, exclude = c("2016-11-11", "2016-10-10"))
## Estimate daily correlations and volatilities using high frequency (minute level) data


# Compute daily realized variances
realized_var_stock <- rCov(minute_returns[,1], align.by = "minutes", align.period= NULL)
realized_var_bond <- rCov(minute_returns[,2], align.by = "minutes", align.period = NULL)
realized_cov <- rCov(minute_returns, cor = TRUE, align.by = "minutes", align.period = NULL)
realized_corr <- rapply(realized_cov, function(x) head(x, 1)[2])
realized_corr <- xts(realized_corr,index(realized_var_bond))

corr_plot <- merge.xts(realized_corr, xts(recent_corr[,4:7],index(realized_var_bond)))

coly = c(1,2,3,4,5)
pdf("HighFreqComparison.pdf")
plot.zoo(corr_plot, plot.type = "single", col =coly, ylab ="", xlab = "")
abline(h =0, col = "1")
title("High frequency comparison")
legend("topright", c("High Frequency realized correlation","30 days","90 days","180 days", "360 days"), col = coly, lty=rep(1,5), bty="n")
dev.off()

## Normalize data by their standard deviations
daily_returns<- tail(log_dr,134)
daily_returns[,1] <- daily_returns[,1]/as.vector(sqrt(realized_var_stock))
daily_returns[,2] <- daily_returns[,2]/as.vector(sqrt(realized_var_bond))

plot(realized_cov[2,1])
