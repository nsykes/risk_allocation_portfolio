library(readr)
library(xts)

# importing all data, removing PERMNO column, formatting date
VCR <- read_csv("Dropbox/MIT/Freshman Year/15.417/risk_allocation_portfolio/VCR.csv", 
                col_types = cols(PERMNO = col_skip(), 
                                 date = col_datetime(format = "%Y%m%d")))
VDC <- read_csv("Dropbox/MIT/Freshman Year/15.417/risk_allocation_portfolio/VDC.csv", 
                col_types = cols(PERMNO = col_skip(), 
                                 date = col_datetime(format = "%Y%m%d")))
VDE <- read_csv("Dropbox/MIT/Freshman Year/15.417/risk_allocation_portfolio/VDE.csv", 
                col_types = cols(PERMNO = col_skip(), 
                                 date = col_datetime(format = "%Y%m%d")))
VFH <- read_csv("Dropbox/MIT/Freshman Year/15.417/risk_allocation_portfolio/VFH.csv", 
                col_types = cols(PERMNO = col_skip(), 
                                 date = col_datetime(format = "%Y%m%d")))
VHT <- read_csv("Dropbox/MIT/Freshman Year/15.417/risk_allocation_portfolio/VHT.csv", 
                col_types = cols(PERMNO = col_skip(), 
                                 date = col_datetime(format = "%Y%m%d")))
VIS <- read_csv("Dropbox/MIT/Freshman Year/15.417/risk_allocation_portfolio/VIS.csv", 
                col_types = cols(PERMNO = col_skip(), 
                                 date = col_datetime(format = "%Y%m%d")))
VGT <- read_csv("Dropbox/MIT/Freshman Year/15.417/risk_allocation_portfolio/VGT.csv", 
                col_types = cols(PERMNO = col_skip(), 
                                 date = col_datetime(format = "%Y%m%d")))
VAW <- read_csv("Dropbox/MIT/Freshman Year/15.417/risk_allocation_portfolio/VAW.csv", 
                col_types = cols(PERMNO = col_skip(), 
                                 date = col_datetime(format = "%Y%m%d")))
VNQ <- read_csv("Dropbox/MIT/Freshman Year/15.417/risk_allocation_portfolio/VNQ.csv", 
                col_types = cols(PERMNO = col_skip(), 
                                 date = col_datetime(format = "%Y%m%d")))
VOX <- read_csv("Dropbox/MIT/Freshman Year/15.417/risk_allocation_portfolio/VOX.csv", 
                col_types = cols(PERMNO = col_skip(), 
                                 date = col_datetime(format = "%Y%m%d")))
# fix VOX
VOX <- VOX[1:3335,]
VPU <- read_csv("Dropbox/MIT/Freshman Year/15.417/Portfolio/VPU.csv", 
                col_types = cols(PERMNO = col_skip(), 
                                 date = col_datetime(format = "%Y%m%d")))

# put all returns into one time series
ETF.R <- data.frame(VCR$RET, VDC$RET, VDE$RET, VFH$RET, VHT$RET, VIS$RET, VGT$RET, VAW$RET, VNQ$RET, VOX$RET, VPU$RET)
colnames(ETF.R) <- c("VCR", "VDC", "VDE", "VFH", "VHT", "VIS", "VGT", "VAW", "VNQ", "VOX", "VPU")

# compute covariance
cv <- cov(ETF.R)
cv.risks <- (rowSums(cv) / sum(rowSums(cv))) * 100

# capital allocation of S&P 500 - same order as ETF.R
allocation <- c(0.1270, 0.0750, 0.0550, 0.15, 0.1380, 0.1020, 0.2520, 0.0290, 0.0260, 0.0190, 0.0270)
cap.returns <- ETF.R * allocation
cap.R.annual <- (mean(cap.returns$VCR) + mean(cap.returns$VDC) + mean(cap.returns$VDE) + mean(cap.returns$VFH) +
                   mean(cap.returns$VHT) + mean(cap.returns$VIS) + mean(cap.returns$VGT) + mean(cap.returns$VAW) +
                   mean(cap.returns$VNQ) + mean(cap.returns$VOX) + mean(cap.returns$VPU)) * 252

# risk allocation of S&P 500 - same order
risk.allocation <- (allocation/cv.risks)/sum(allocation/cv.risks)
risk.returns <- ETF.R * risk.allocation
risk.R.annual <- (mean(risk.returns$VCR) + mean(risk.returns$VDC) + mean(risk.returns$VDE) + mean(risk.returns$VFH) +
                   mean(risk.returns$VHT) + mean(risk.returns$VIS) + mean(risk.returns$VGT) + mean(risk.returns$VAW) +
                   mean(risk.returns$VNQ) + mean(risk.returns$VOX) + mean(risk.returns$VPU)) * 252

rebal.risk <- function(dates, returns, desired, days) {
  # dates           | column of dates
  # returns         | simple returns for all securities
  # desired         | desired risk allocation
  # days            | days before rebalancing
  # portfolio return
  portfolio.R <- data.frame()
  # current allocation
  current.alloc <- data.frame()
  # counter variable
  i <- days + 1
  # iterate through each row
  while (i <= nrow(returns)) {
    # case: first time
    if (i == days + 1) {
      # 30 day trailing window
      trailing.window <- returns[(i-days):(i-1),]
      # compute covariance, convert to normalized risk
      cv <- cov(trailing.window)
      cv.risks <- (rowSums(cv) / sum(rowSums(cv))) * 100
      # change risks to capital allocation
      current.alloc <- (desired/cv.risks)/sum(desired/cv.risks)
      # total return for the day, initial price ($1000)
      portfolio.R <- data.frame(1000, sum(returns[i,] * current.alloc))
      colnames(portfolio.R) <- c("PRC", "RTN")
    } else {
      # adjust current allocation
      # yesterday's allocation * yesterday's returns, then normalize
      current.alloc <- (current.alloc * (1+returns[i-1,]))/sum(current.alloc * (1+returns[i-1,]))
      # get day's return
      day.R <- sum(returns[i,] * current.alloc)
      # calculate today's price from yesterday's price and yesterday's return
      day.PRC <- portfolio.R$PRC[i-days-1] * (1 + portfolio.R$RTN[i-days-1])
      # combine into data.frame and merge
      day.DF <- data.frame(day.PRC, day.R)
      colnames(day.DF) <- c("PRC", "RTN")
      portfolio.R <- rbind(portfolio.R, day.DF)
    }
    # case: rebalance (at end of day)
    if (i %% days == 0) {
      # 30 day trailing window
      trailing.window <- returns[(i-days):(i-1),]
      # compute covariance, convert to normalized risk
      cv <- cov(trailing.window)
      cv.risks <- (rowSums(cv) / sum(rowSums(cv))) * 100
      # change risks to capital allocation
      current.alloc <- (desired/cv.risks)/sum(desired/cv.risks)
    }
    i <- i + 1
  }
  portfolio.R <- cbind(dates[(days + 1):length(dates)], portfolio.R)
  colnames(portfolio.R) <- c("date", "PRC", "RTN")
  # convert to XTS object
  return <- xts(portfolio.R[,-1],order.by=portfolio.R$date)
  return
}

# benchmark function
rebal.cap <- function(dates, returns, desired, days) {
  # dates           | column of dates
  # returns         | simple returns for all securities
  # desired         | desired risk allocation
  # days            | days before rebalancing
  # portfolio return
  portfolio.R <- data.frame()
  # current allocation
  current.alloc <- data.frame()
  # counter variable
  i <- days + 1
  # iterate through each row
  while (i <= nrow(returns)) {
    # case: first time
    if (i == days + 1) {
      current.alloc <- desired
      # total return for the day, initial price ($1000)
      portfolio.R <- data.frame(1000, sum(returns[i,] * current.alloc))
      colnames(portfolio.R) <- c("PRC", "RTN")
    } else {
      # adjust current allocation
      # yesterday's allocation * yesterday's returns, then normalize
      current.alloc <- (current.alloc * (1+returns[i-1,]))/sum(current.alloc * (1+returns[i-1,]))
      # get day's return
      day.R <- sum(returns[i,] * current.alloc)
      # calculate today's price from yesterday's price and yesterday's return
      day.PRC <- portfolio.R$PRC[i-days-1] * (1 + portfolio.R$RTN[i-days-1])
      # combine into data.frame and merge
      day.DF <- data.frame(day.PRC, day.R)
      colnames(day.DF) <- c("PRC", "RTN")
      portfolio.R <- rbind(portfolio.R, day.DF)
    }
    # case: rebalance (at end of day)
    if (i %% days == 0) {
      current.alloc <- desired
    }
    i <- i + 1
  }
  portfolio.R <- cbind(dates[(days + 1):length(dates)], portfolio.R)
  colnames(portfolio.R) <- c("date", "PRC", "RTN")
  # convert to XTS object
  return <- xts(portfolio.R[,-1],order.by=portfolio.R$date)
  return
}

# testing function
dates <- VOX$date
portfolio.R <- rebal.risk(dates, ETF.R, allocation, 25)
benchmark <- rebal.cap(dates, ETF.R, allocation, 25)