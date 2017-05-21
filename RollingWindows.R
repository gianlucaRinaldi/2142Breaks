###############################################################################
## Working with daily data
###############################################################################
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
############################################################################
# compute rolling correlations (can also compute covariances using roll_cov)
win30_corr <- roll_cor(log_dr, width = 30 , weights = rep(1, 30), center = TRUE,
                       scale = TRUE,  complete_obs = TRUE,
                       na_restore = FALSE, parallel_for = c("rows", "cols"))[1,2,]

win90_corr <- roll_cor(log_dr, width = 90 , weights = rep(1, 90), center = TRUE,
                       scale = TRUE, complete_obs = TRUE,
                       na_restore = FALSE, parallel_for = c("rows", "cols"))[1,2,]

win180_corr <- roll_cor(log_dr, width = 180 , weights = rep(1, 180), center = TRUE,
                        scale = TRUE,  complete_obs = TRUE,
                        na_restore = FALSE, parallel_for = c("rows", "cols"))[1,2,]

win360_corr <- roll_cor(log_dr, width = 360 , weights = rep(1, 360), center = TRUE,
                        scale = TRUE,  complete_obs = TRUE,
                        na_restore = FALSE, parallel_for = c("rows", "cols"))[1,2,]

win30_corr <- xts(win30_corr, index(log_dr))
win90_corr <- xts(win90_corr, index(log_dr))
win180_corr <- xts(win180_corr, index(log_dr))
win360_corr <- xts(win360_corr, index(log_dr))
win_corr <- tail(merge.xts(win30_corr,win90_corr,win180_corr,win360_corr),-360+1)
indexTZ(win_corr) <- "America/Chicago"
coly = c(1,2,3,4)
pdf("RollWind_join.pdf")
plot.zoo(win_corr, plot.type = "single", ylim = c(-1,1), col =coly, ylab ="", xlab = "")
abline(h =0, col = "1")
title("Rolling Windows")
legend("topright", c("30 days", "90 days", "180 days","360 days"), col = coly, lty=rep(1,4), bty="n")
dev.off()

win_corr_lines = merge.xts(win_corr,xts(rep(0,length(win_corr)/4),index(win_corr)),xts(rep(0,length(win_corr)/4),index(win_corr)),xts(rep(0,length(win_corr)/3),index(win_corr))) # ro plot zero line on each panel
pdf("RollWind.pdf")
plot.zoo(win_corr_lines, plot.type = "multiple",  screens=c(1,2,3,1,2,3), ylim = c(-1,1), 
         col =c(20,2,3), ylab =c("30 days","90 days", "180 days"), xlab = "", main= "Rolling Windows correlation of daily returns")
dev.off()

############################################################################
# Exponentially decaying rolling correlations
weights <- 0.98 ^ (360:0)
win30_corr_exp <- roll_cor(log_dr, width = 30 , weights = head(weights,30), center = TRUE,
                           scale = TRUE,  complete_obs = TRUE,
                           na_restore = FALSE, parallel_for = c("rows", "cols"))[1,2,]
win90_corr_exp <- roll_cor(log_dr, width = 90 , weights = head(weights,90), center = TRUE,
                           scale = TRUE, complete_obs = TRUE,
                           na_restore = FALSE, parallel_for = c("rows", "cols"))[1,2,]
win180_corr_exp <- roll_cor(log_dr, width = 180 , weights = head(weights,180), center = TRUE,
                            scale = TRUE,  complete_obs = TRUE,
                            na_restore = FALSE, parallel_for = c("rows", "cols"))[1,2,]
win360_corr_exp <- roll_cor(log_dr, width = 360 , weights = head(weights,360), center = TRUE,
                            scale = TRUE,  complete_obs = TRUE,
                            na_restore = FALSE, parallel_for = c("rows", "cols"))[1,2,]
win30_corr_exp <- xts(win30_corr_exp, index(log_dr))
win90_corr_exp <- xts(win90_corr_exp, index(log_dr))
win180_corr_exp <- xts(win180_corr_exp, index(log_dr))
win360_corr_exp <- xts(win360_corr_exp, index(log_dr))
win_corr_exp <- tail(merge.xts(win30_corr_exp,win90_corr_exp,win180_corr_exp,win360_corr_exp),-360+1)
indexTZ(win_corr_exp) <- "America/Chicago"

pdf("RollWindExp_join.pdf")
plot.zoo(win_corr_exp, plot.type = "single", ylim = c(-1,1), col =coly, ylab ="", xlab ="")
abline(h =0, col = "1")
title("Exponentially decaying rolling correlations")
legend("topright", c("30 days", "90 days","180 days", "360 days"), col = coly, lty=rep(1,4), bty="n")
dev.off()


win_corr_exp_lines = merge.xts(win_corr_exp,xts(rep(0,length(win_corr_exp)/3),index(win_corr_exp)),xts(rep(0,length(win_corr_exp)/3),index(win_corr_exp)),xts(rep(0,length(win_corr_exp)/3),index(win_corr_exp))) # ro plot zero line on each panel
pdf("RollWindExp.pdf")
plot.zoo(win_corr_exp_lines, plot.type = "multiple",  screens=c(1,2,3,1,2,3), ylim = c(-1,1), 
         col =c(20,2,3), ylab =c("30 days","90 days", "180 days"), xlab = "", main= "Exponentially decaying rolling windows correlation of daily returns")
dev.off()

##################################################
# construct dataframe of all correlation measures
##################################################
indexClass(win_corr) <- "Date" # get rid of time (daily data)
indexClass(win_corr_exp) <- "Date"
windows <- merge.xts(win_corr,win_corr_exp)

WINcorr_measures <- data.frame(date = index(windows),windows, row.names=NULL)

