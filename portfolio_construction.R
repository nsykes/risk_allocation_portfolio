#################################################
###                                           ###
###     15.417 Laboratory in Investments      ###
###                Spring 2018                ###
###               Final Project               ###
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

###############################################
###           Simulated Scenarios           ###
###############################################

# normal crash (+15% volatility)
# 6 months normal,
# 7 days (-3%, -, -3%, -, -1%, -, -1%) (cumulative -7.78%)
# 2 months (0.19309% each day)
sim.crash.1 <- function(returns) {
  # returns         | 12 months of returns
  # 6 months = day 126
  returns[126:132] <- returns[126:132] - c(0.03, 0, 0.03, 0, 0.01, 0, 0.01)
  # counter variable
  i <- 133
  # recovery period
  while (i <= 175) {
    returns[i] <- returns[i] + 0.0019309
    i <- i + 1
  }
  # volatility
  i <- 126
  while (i <= 175) {
    returns[i] <- returns[i] * 1.15
    i <- i + 1
  }
  returns
}

# large crash (+30% volatility)
# 6 months normal
# 9 days (-4%, -, -2%, -, -2%, -, -2%, -, -2%) (cumulative −11.45%)
# 3 months (0.19325% each day)
sim.crash.2 <- function(returns) {
  # returns         | 12 months of returns
  # 6 months = day 126
  returns[126:134] <- returns[126:134] - c(0.04, 0, 0.02, 0, 0.02, 0, 0.02, 0, 0.02)
  # counter variable
  i <- 135
  # recovery period
  while (i <= 198) {
    returns[i] <- returns[i] + 0.0019325
    i <- i + 1
  }
  # volatility
  i <- 126
  while (i <= 198) {
    returns[i] <- returns[i] * 1.3
    i <- i + 1
  }
  returns
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
# SR.benchmark <- (mean(benchmark$RTN) * 252)/(sd(benchmark$RTN) * sqrt(252))

# Testing Simulations
dates <- VGT$date[2100:2352]
# small crash for VGT
VGT.crash.1 <- sim.crash.1(VGT$RET[2100:2352])
ETF.R.crash.1 <- ETF.R[2100:2352,]
ETF.R.crash.1$VGT <- VGT.crash.1
# large crash for VGT
VGT.crash.2 <- sim.crash.2(VGT$RET[2100:2352])
ETF.R.crash.2 <- ETF.R[2100:2352,]
ETF.R.crash.2$VGT <- VGT.crash.2
# running simulations
portfolio.crash.1 <- rebal.risk(dates, ETF.R.crash.1, allocation, 21)
portfolio.crash.1.bm <- rebal.cap(dates, ETF.R.crash.1, allocation, 21)
portfolio.crash.2 <- rebal.risk(dates, ETF.R.crash.2, allocation, 21)
portfolio.crash.2.bm <- rebal.cap(dates, ETF.R.crash.2, allocation, 21)


###############################################
###            Graph Construction           ###
###############################################

# extract price data
GSPC.PRC <- GSPC$GSPC.Adjusted[22:nrow(GSPC)]
portfolio.PRC <- portfolio$PRC
# define time series
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