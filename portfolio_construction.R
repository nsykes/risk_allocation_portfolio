#################################################
###                                           ###
###     15.417 Laboratory in Investments      ###
###                Spring 2018                ###
###               Final Project               ###
###                                           ###
#################################################

# Required Packages
library(readr)
library(xts)
library(quantmod)
library(ggplot2)
library(reshape)
library(scales)

###############################################
###               Data Import               ###
###############################################

# Working Directory (change as necessary)
setwd('/home/nyle/Dropbox/MIT/Freshman Year/15.417/risk_allocation_portfolio/')

# Import Data
VCR <- read_csv("VCR.csv", 
                col_types = cols(PERMNO = col_skip(), 
                                 date = col_datetime(format = "%Y%m%d")))
VDC <- read_csv("VDC.csv", 
                col_types = cols(PERMNO = col_skip(), 
                                 date = col_datetime(format = "%Y%m%d")))
VDE <- read_csv("VDE.csv", 
                col_types = cols(PERMNO = col_skip(), 
                                 date = col_datetime(format = "%Y%m%d")))
VFH <- read_csv("VFH.csv", 
                col_types = cols(PERMNO = col_skip(), 
                                 date = col_datetime(format = "%Y%m%d")))
VHT <- read_csv("VHT.csv", 
                col_types = cols(PERMNO = col_skip(), 
                                 date = col_datetime(format = "%Y%m%d")))
VIS <- read_csv("VIS.csv", 
                col_types = cols(PERMNO = col_skip(), 
                                 date = col_datetime(format = "%Y%m%d")))
VGT <- read_csv("VGT.csv", 
                col_types = cols(PERMNO = col_skip(), 
                                 date = col_datetime(format = "%Y%m%d")))
VAW <- read_csv("VAW.csv", 
                col_types = cols(PERMNO = col_skip(), 
                                 date = col_datetime(format = "%Y%m%d")))
VNQ <- read_csv("VNQ.csv", 
                col_types = cols(PERMNO = col_skip(), 
                                 date = col_datetime(format = "%Y%m%d")))
VOX <- read_csv("VOX.csv", 
                col_types = cols(PERMNO = col_skip(), 
                                 date = col_datetime(format = "%Y%m%d")))
VPU <- read_csv("VPU.csv", 
                col_types = cols(PERMNO = col_skip(), 
                                 date = col_datetime(format = "%Y%m%d")))

# Load Data for S&P500
symbol="^GSPC" 
startday=as.Date("2004-10-04")
endday=as.Date("2017-12-30")
getSymbols(symbol, src = "yahoo",from = startday, to = endday)

# Clean Data
VCR[6,]$PRC <- -VCR[6,]$PRC
rows <- c(25, 43, 59, 89, 102)
VIS[rows,]$PRC <- -VIS[rows,]$PRC
VOX <- VOX[1:3335,]

# Time Series of all Returns
ETF.R <- data.frame(VCR$RET, VDC$RET, VDE$RET, VFH$RET, VHT$RET, VIS$RET, VGT$RET, VAW$RET, VNQ$RET, VOX$RET, VPU$RET)
colnames(ETF.R) <- c("VCR", "VDC", "VDE", "VFH", "VHT", "VIS", "VGT", "VAW", "VNQ", "VOX", "VPU")

# Covariance Calculation
# cv <- cov(ETF.R)
# cv.risks <- (rowSums(cv) / sum(rowSums(cv))) * 100

# Capital Allocation of S&P500 - (same order as ETF.R)
allocation <- c(0.1270, 0.0750, 0.0550, 0.15, 0.1380, 0.1020, 0.2520, 0.0290, 0.0260, 0.0190, 0.0270)
# cap.returns <- ETF.R * allocation
# cap.R.annual <- (mean(cap.returns$VCR) + mean(cap.returns$VDC) + mean(cap.returns$VDE) + mean(cap.returns$VFH) +
#                    mean(cap.returns$VHT) + mean(cap.returns$VIS) + mean(cap.returns$VGT) + mean(cap.returns$VAW) +
#                    mean(cap.returns$VNQ) + mean(cap.returns$VOX) + mean(cap.returns$VPU)) * 252

# risk allocation of S&P500 - same order
# risk.allocation <- (allocation/cv.risks)/sum(allocation/cv.risks)
# risk.returns <- ETF.R * risk.allocation
# risk.R.annual <- (mean(risk.returns$VCR) + mean(risk.returns$VDC) + mean(risk.returns$VDE) + mean(risk.returns$VFH) +
#                    mean(risk.returns$VHT) + mean(risk.returns$VIS) + mean(risk.returns$VGT) + mean(risk.returns$VAW) +
#                    mean(risk.returns$VNQ) + mean(risk.returns$VOX) + mean(risk.returns$VPU)) * 252

###############################################
###               Functions                 ###
###############################################

# Portfolio Function
rebal.risk <- function(dates, returns, desired, days) {
  # dates           | column of dates
  # returns         | simple returns for all securities
  # desired         | desired risk allocation
  # days            | days before rebalancing
  # portfolio return
  portfolio <- data.frame()
  # current allocation
  current.alloc <- data.frame()
  # counter variable
  i <- days + 1
  # iterate through each row
  while (i <= nrow(returns)) {
    # case: first time
    if (i == days + 1) {
      # x day trailing window
      trailing.window <- returns[(i-days):(i-1),]
      # compute covariance, convert to normalized risk
      cv <- cov(trailing.window)
      cv.risks <- (rowSums(cv) / sum(rowSums(cv))) * 100
      # change risks to capital allocation
      current.alloc <- (desired/cv.risks)/sum(desired/cv.risks)
      # total return for the day, initial price ($1130.56)
      portfolio <- data.frame(1130.56, sum(returns[i,] * current.alloc))
      colnames(portfolio) <- c("PRC", "RTN")
    } else {
      # adjust current allocation
      # yesterday's allocation * yesterday's returns, then normalize
      current.alloc <- (current.alloc * (1+returns[i-1,]))/sum(current.alloc * (1+returns[i-1,]))
      # get day's return
      day.R <- sum(returns[i,] * current.alloc)
      # calculate today's price from yesterday's price and yesterday's return
      day.PRC <- portfolio$PRC[i-days-1] * (1 + portfolio$RTN[i-days-1])
      # combine into data.frame and merge
      day.DF <- data.frame(day.PRC, day.R)
      colnames(day.DF) <- c("PRC", "RTN")
      portfolio <- rbind(portfolio, day.DF)
    }
    # case: rebalance (at end of day)
    if (i %% days == 0) {
      # 9-month trailing window
      lower.bound <- i - 189
      if (lower.bound < 1) {
        lower.bound = 1
      }
      trailing.window <- returns[lower.bound:(i-1),]
      # compute covariance, convert to normalized risk
      cv <- cov(trailing.window)
      cv.risks <- (rowSums(cv) / sum(rowSums(cv))) * 100
      # change risks to capital allocation
      current.alloc <- (desired/cv.risks)/sum(desired/cv.risks)
    }
    i <- i + 1
  }
  portfolio <- cbind(dates[(days + 1):length(dates)], portfolio)
  colnames(portfolio) <- c("date", "PRC", "RTN")
  # convert to XTS object
  return <- xts(portfolio[,-1],order.by=portfolio$date)
  return
}

# Benchmark Function
rebal.cap <- function(dates, returns, desired, days) {
  # dates           | column of dates
  # returns         | simple returns for all securities
  # desired         | desired risk allocation
  # days            | days before rebalancing
  portfolio <- data.frame()
  # current allocation
  current.alloc <- data.frame()
  # counter variable
  i <- days + 1
  # iterate through each row
  while (i <= nrow(returns)) {
    # case: first time
    if (i == days + 1) {
      current.alloc <- desired
      # total return for the day, initial price ($1130.56)
      portfolio <- data.frame(1130.56, sum(returns[i,] * current.alloc))
      colnames(portfolio) <- c("PRC", "RTN")
    } else {
      # adjust current allocation
      # yesterday's allocation * yesterday's returns, then normalize
      current.alloc <- (current.alloc * (1+returns[i-1,]))/sum(current.alloc * (1+returns[i-1,]))
      # get day's return
      day.R <- sum(returns[i,] * current.alloc)
      # calculate today's price from yesterday's price and yesterday's return
      day.PRC <- portfolio$PRC[i-days-1] * (1 + portfolio$RTN[i-days-1])
      # combine into data.frame and merge
      day.DF <- data.frame(day.PRC, day.R)
      colnames(day.DF) <- c("PRC", "RTN")
      portfolio <- rbind(portfolio, day.DF)
    }
    # case: rebalance (at end of day)
    if (i %% days == 0) {
      current.alloc <- desired
    }
    i <- i + 1
  }
  portfolio <- cbind(dates[(days + 1):length(dates)], portfolio)
  colnames(portfolio) <- c("date", "PRC", "RTN")
  # convert to XTS object
  return <- xts(portfolio[,-1],order.by=portfolio$date)
  return
}

# Converts returns into prices
r.to.prc <- function(returns) {
  # dates           | column of dates
  # returns         | asset returns
  prices <- data.frame()
  # counter variable
  i <- 1
  while (i <= nrow(returns)) {
    if (i == 1) {
      # initial price ($1130.56)
      prices <- data.frame(1130.56)
      colnames(prices) <- c("PRC")
    } else {
      day.DF <- data.frame(as.numeric(prices$PRC[i-1]) * as.numeric(1 + returns$GSPC.Adjusted[i-1]))
      colnames(day.DF) <- c("PRC")
      prices <- rbind(prices, day.DF)
    }
    i <- i + 1
  }
  prices
}

###############################################
###           Simulated Scenarios           ###
###############################################

# normal crash
# 6 months normal
# 7 days (-3%, -, -3%, -, -1%, -, -1%) (cumulative -7.78%)
# 2 months (0.19309% each day)

# large crash
# 6 months normal
# 9 days (-4%, -, -2%, -, -2%, -, -2%, -, -2%) (cumulative −11.45%)
# 3 months (0.19325% each day)

# 3 columns: large crash, normal crash, none
# 3 rows (volatility): +30%, +15%, 0%
sim.market <- function(returns, scale) {
  # returns         | 12 months of returns
  # scale           | 0<scale<1
  sim.returns <- data.frame(returns, returns, returns,
                            returns, returns, returns,
                            returns, returns, returns)
  colnames(sim.returns) <- c("RTN", "RTN.1", "RTN.2", "RTN.3", "RTN.4", "RTN.5", "RTN.6", "RTN.7", "RTN.8")
  # 6 months = day 126
  # large crash column
  sim.returns$RTN[126:134] <- sim.returns$RTN[126:134] - (c(0.04, 0, 0.02, 0, 0.02, 0, 0.02, 0, 0.02) * scale)
  sim.returns$RTN.3[126:134] <- sim.returns$RTN.3[126:134] - (c(0.04, 0, 0.02, 0, 0.02, 0, 0.02, 0, 0.02) * scale)
  sim.returns$RTN.6[126:134] <- sim.returns$RTN.6[126:134] - (c(0.04, 0, 0.02, 0, 0.02, 0, 0.02, 0, 0.02) * scale)
  # normal crash column
  sim.returns$RTN.1[126:132] <- sim.returns$RTN.1[126:132] - (c(0.03, 0, 0.03, 0, 0.01, 0, 0.01) * scale)
  sim.returns$RTN.4[126:132] <- sim.returns$RTN.4[126:132] - (c(0.03, 0, 0.03, 0, 0.01, 0, 0.01) * scale)
  sim.returns$RTN.7[126:132] <- sim.returns$RTN.7[126:132] - (c(0.03, 0, 0.03, 0, 0.01, 0, 0.01) * scale)
  # large crash recovery period
  i <- 135
  while (i <= 198) {
    sim.returns$RTN[i] <- sim.returns$RTN[i] + 0.0019325 * scale
    sim.returns$RTN.3[i] <- sim.returns$RTN.3[i] + 0.0019325 * scale
    sim.returns$RTN.6[i] <- sim.returns$RTN.6[i] + 0.0019325 * scale
    i <- i + 1
  }
  # normal crash recovery period
  i <- 133
  while (i <= 175) {
    # normal crash recovery
    sim.returns$RTN.1[i] <- sim.returns$RTN.1[i] + 0.0019309 * scale
    sim.returns$RTN.4[i] <- sim.returns$RTN.4[i] + 0.0019309 * scale
    sim.returns$RTN.7[i] <- sim.returns$RTN.7[i] + 0.0019309 * scale
    i <- i + 1
  }
  # volatility
  i <- 126
  while (i <= 198) {
    # +30% row
    sim.returns$RTN[i] <- sim.returns$RTN[i] * (1 + 0.3 * scale)
    sim.returns$RTN.1[i] <- sim.returns$RTN.1[i] * (1 + 0.3 * scale)
    sim.returns$RTN.2[i] <- sim.returns$RTN.2[i] * (1 + 0.3 * scale)
    # +15% row
    sim.returns$RTN.3[i] <- sim.returns$RTN.3[i] * (1 + 0.15 * scale)
    sim.returns$RTN.4[i] <- sim.returns$RTN.4[i] * (1 + 0.15 * scale)
    sim.returns$RTN.5[i] <- sim.returns$RTN.5[i] * (1 + 0.15 * scale)
    i <- i + 1
  }
  sim.returns
}

###############################################
###                Testing                  ###
###############################################

# S&P500 Benchmark
GSPC.r <- diff(log(GSPC$GSPC.Adjusted))[22:nrow(GSPC)]

# Testing Function
dates <- VCR$date
portfolio <- rebal.risk(dates, ETF.R, allocation, 21)
# benchmark <- rebal.cap(dates, ETF.R, allocation, 21)

# Sharpe Ratio
SR.portfolio <-(mean(portfolio$RTN) * 252)/(sd(portfolio$RTN) * sqrt(252))
SR.benchmark <-(mean(GSPC.r$GSPC.Adjusted) * 252)/(sd(GSPC.r$GSPC.Adjusted) * sqrt(252))

# Testing Simulations
dates <- VGT$date[2100:2352]
# VGT simulations
VGT.sim <- sim.market(VGT$RET[2100:2352], 1)
ETF.R.sim.1 <- ETF.R[2100:2352,]
ETF.R.sim.1$VGT <- VGT.sim$RTN
ETF.R.sim.2 <- ETF.R[2100:2352,]
ETF.R.sim.2$VGT <- VGT.sim$RTN.1
ETF.R.sim.3 <- ETF.R[2100:2352,]
ETF.R.sim.3$VGT <- VGT.sim$RTN.2
ETF.R.sim.4 <- ETF.R[2100:2352,]
ETF.R.sim.4$VGT <- VGT.sim$RTN.3
ETF.R.sim.5<- ETF.R[2100:2352,]
ETF.R.sim.5$VGT <- VGT.sim$RTN.4
ETF.R.sim.6 <- ETF.R[2100:2352,]
ETF.R.sim.6$VGT <- VGT.sim$RTN.5
ETF.R.sim.7 <- ETF.R[2100:2352,]
ETF.R.sim.7$VGT <- VGT.sim$RTN.6
ETF.R.sim.8 <- ETF.R[2100:2352,]
ETF.R.sim.8$VGT <- VGT.sim$RTN.7
ETF.R.sim.9 <- ETF.R[2100:2352,]
ETF.R.sim.9$VGT <- VGT.sim$RTN.8
# simulations
portfolio.sim.1 <- rebal.risk(dates, ETF.R.sim.1, allocation, 21)
portfolio.sim.2 <- rebal.risk(dates, ETF.R.sim.2, allocation, 21)
portfolio.sim.3 <- rebal.risk(dates, ETF.R.sim.3, allocation, 21)
portfolio.sim.4 <- rebal.risk(dates, ETF.R.sim.4, allocation, 21)
portfolio.sim.5 <- rebal.risk(dates, ETF.R.sim.5, allocation, 21)
portfolio.sim.6 <- rebal.risk(dates, ETF.R.sim.6, allocation, 21)
portfolio.sim.7 <- rebal.risk(dates, ETF.R.sim.7, allocation, 21)
portfolio.sim.8 <- rebal.risk(dates, ETF.R.sim.8, allocation, 21)
portfolio.sim.9 <- rebal.risk(dates, ETF.R.sim.9, allocation, 21)
# benchmark
GSPC.r <- diff(log(GSPC$GSPC.Adjusted))[2100:2352]
colnames(GSPC.r) <- c("RTN")
# simulation
GSPC.sim <- sim.market(GSPC.r, 0.25)

###############################################
###            Graph Construction           ###
###############################################

# extract price data
GSPC.PRC <- GSPC$GSPC.Adjusted[22:nrow(GSPC)]
portfolio.PRC <- portfolio$PRC
# define time series
dates <- VCR$date
date <- dates[22:length(dates)]
ts = data.frame("Date" = date,  SP500 = GSPC.PRC,  Portfolio = portfolio.PRC)
colnames(ts) = c("Date", "SP500", "Portfolio")
# data pre-process (melt into ggplot compatible form)
ts <- melt(ts, id.vars = "Date", measure.vars = colnames(ts)[2:ncol(ts)], variable.name = "Assets", value.name = "Price")
ts$Date = as.Date(ts$Date, format = "%Y.%m.%d")
colnames(ts) = c("Date", "Assets", "Price")

# ggplot
p <- ggplot(ts,aes(x= Date, y = Price, group = Assets))
colour_labels = c("violet","green")  #"blue","red","firebrick","gray75","seagreen","violet","green","gold","orange","grey1", and etc...
p <- p + geom_line(aes(colour= Assets, linetype = "solid"),size = 1.1)
p <- p + labs(x="Date", y="Price")+scale_x_date(breaks = date_breaks("2 years"),labels = date_format("%Y %m"))+ ggtitle("Comparison Between SP500 and Portfolio")
p
