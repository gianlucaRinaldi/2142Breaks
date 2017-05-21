###############################################################################
# DCC - GARCH estimation of time varying correlation
# Working with daily data
###############################################################################
# loading packages
require(PerformanceAnalytics) 
require(xts)
require(xtsExtra)
require(RColorBrewer)
require(quantmod)
require(lubridate)
require(RcppRoll)
require(ggplot2)
require(roll) # to compute rolling statistics 
require(rmgarch) # Package to implement multivariate GARCH models


stock_daily_dat <- read.csv("sp500daily.csv",skip =1, header = TRUE, stringsAsFactors = FALSE)
bond_daily_dat <- read.csv("treasury10ydaily.csv",skip =1,header = TRUE, stringsAsFactors = FALSE)
stock_daily_date <- strptime(as.character(stock_daily_dat$Date),format="%m/%d/%Y")
bond_daily_date <- strptime(as.character(bond_daily_dat$Date),format="%m/%d/%Y")
stock_open <- xts(stock_daily_dat$PX_OPEN,stock_daily_date) 
bond_open <- xts(as.numeric(bond_daily_dat$PX_OPEN),bond_daily_date) 
stock_log_dr <- tail(diff(log(stock_open)),-1) # Stock daily returns (remove 1st ob)
bond_log_dr <- tail(-diff(log(bond_open+100)),-1) # Remove first observation of log daily bond returns

# set start dates
stock_log_dr <- stock_log_dr['1985-01-01/']
bond_log_dr <- bond_log_dr['1985-01-01/']

# Bind together the two series of daily returns for the common coverage sample
log_dr <- merge.xts(stock_log_dr,bond_log_dr,join="inner") 

# We will fit a DCC model by first running standard GARCH on each of the series and then working with the residuals

####### DCC specification

# specify univariate garch for individual series
xspec = ugarchspec(mean.model = list(armaOrder = c(2, 2)), variance.model = list(garchOrder = c(3,3), model = 'eGARCH'), distribution.model = 'norm')
# repeat to have it for both
uspec = multispec(replicate(2, xspec))

# Estimate the first stage GARCH  
multf = multifit(uspec, log_dr)

# construct several possible DCC specification
spec1 = dccspec(uspec = uspec, dccOrder = c(1, 1), distribution = 'mvnorm')
spec1a = dccspec(uspec = uspec, dccOrder = c(1, 1), model='aDCC', distribution = 'mvnorm')
spec2 = dccspec(uspec = uspec, dccOrder = c(1, 1), distribution = 'mvlaplace')
spec2a = dccspec(uspec = uspec, dccOrder = c(1, 1), model='aDCC', distribution = 'mvlaplace')

####### DCC estimation
fit1 = dccfit(spec1, data = log_dr, fit.control = list(eval.se = TRUE), fit = multf)
fit1a = dccfit(spec1a, data = log_dr, fit.control = list(eval.se = TRUE), fit = multf)
fit2 = dccfit(spec2, data = log_dr, fit.control = list(eval.se = TRUE), fit = multf)
fit2a = dccfit(spec2a, data = log_dr, fit.control = list(eval.se = TRUE), fit = multf)

library(timeSeries)
R1 = rcor(fit1)
R2 = rcor(fit2)
colx = c(colors()[24], colors()[33], colors()[139])

pdf("DCC.pdf")
RR = xts(cbind(R1[1,2,],R2[1,2,]), index(log_dr))
lines(RR[,1], ylab = "correlation", col = colx[1], lty=1 ,lwd=1)
lines(RR[,2], col = colx[2], lty = 2, lwd=1+2/10)
abline(h =0, col = "1")
title("Two specifications of DCC GARCH")
legend("bottomright", c("DCC(normal)", "DCC(laplace)"), col = colx, lty=1:3, bty="n")
dev.off()
colnames(RR) <- c("DCCnormal","DCClaplace")
indexClass(RR) <- "Date"

DCCcorr_measures <- data.frame(date = index(RR),RR, row.names=NULL)


