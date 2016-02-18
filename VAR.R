# VAR model
# Michael Kilchenmann
# February 2016

# object.size(mat)
  
  print(paste("Start: ", Sys.time()))
  library(quantmod)
  library(vars)
  srcDrc <- getSrcDirectory(function(x) {x})
  setwd(srcDrc)
  source("Data_Retrieve.r")
  #dev.off(dev.list()["RStudioGD"])
  
  actual.diff <- diff(sc.data)
  i <- 0
  
  # Collect index (S&P 500) data from Yahoo
  
    date.start <- paste(format(start(sc.data),"%Y"),format(start(sc.data),"%m"),format(start(sc.data),"%d"))
    date.end <- paste(format(end(sc.data),"%Y"),format(end(sc.data),"%m"),format(end(sc.data),"%d"))
  
    SPX <- getYahooData("^GSPC", start=date.start, end=date.end)
    SPX <- SPX$Close

  
  # collect Gold Prices
    XAU <- getFX("XAU/USD",
                     from = Sys.Date() - 360,
                     tp = Sys.Date(),
                     env = parent.frame(),
                     verbose = FALSE,
                     warning = TRUE,
                     auto.assign = FALSE)
    
  # Create Matrix with all prices and ommit NA days along the ways :)
    mat <- merge(sc.data, XAU, SPX)
    mat <- data.frame(value=coredata(mat),timestamp=index(mat))
    mat <- na.omit(mat)
    colnames(mat)[3] <- "value.SPX"
    mat <- xts(mat[,-4],order.by=mat[,4])
    #xts(mat)
  
        
  if (i==1){
    par(mfrow=c(2,3))
    plot(data, main="data 500 days rolling")
    plot(data_fd, main = "first differences 500 days rolling")
    hist(data_fd, main = "histogramm of fd")
    acf(data, main = "ACF data")
    acf(data_fd, main = "ACF fd", na.action=na.omit)
    pacf(data_fd, main = "PACF fd",  na.action=na.omit)
  }
  
  par(mfrow=c(2,2))
  plot(mat[,1],main="EURUSD")
  plot(mat[,2],main="XAUUSD")
  plot(mat[,3],main="SPX")
  acf(mat,lag.max=10)
  acf(mat,type="partial",lag.max=10)
  
  
  print(paste("End: ", Sys.time()))
  
  