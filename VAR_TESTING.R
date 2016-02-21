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
  # dev.off(dev.list()["RStudioGD"])
  
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
    # Use first differences
    mat <- na.omit(diff(mat))
    
    #xts(mat)

  # par(mfrow=c(2,2))
  # plot(mat[,1],main="EURUSD")
  # plot(mat[,2],main="XAUUSD")
  # plot(mat[,3],main="SPX")
  # acf(mat,lag.max=10)
  # acf(mat,type="partial",lag.max=10)
  
  # print('dim mat')
  # print(dim(mat))
  
  
      np <- 0
      n <- 0
    
      for(j in 100:248){
      
      mat <- mat[(j-99):j,]
      
  mat.VAR.const <- VARselect(mat, lag.max=12, type="const")
  print('mat.VAR.const')
  print(mat.VAR.const$selection)
  
  # VAR estimation
  mat.VAR.const.0 <- VAR(mat, p=mat.VAR.const$selection[3],type="const")
  options(show.signif.stars=TRUE)
  
  # VAR Forecast
  var.pred <- predict(mat.VAR.const.0, n.ahead=1, ci=0.8)
  print(summary(mat.VAR.const.0))
  
  # Plot Impulse Respnse Functions
  # plot(irf(mat.VAR.const.0, impulse="value.EUR.USD"))
  
  # Ordinary and Partial Autocorrelations of Differenced Series

  # par(mfrow=c(2,2))
  # plot(mat[,1],main="EURUSD")
  # plot(mat[,2],main="XAUUSD")
  # plot(mat[,3],main="SPX")
  
  # Calculation of Hit Rate          
  if (sign(actual.diff[(j+1),])==sign(var.pred$fcst$value.EUR.USD)){
    if(i==1){
      print(actual.diff[(j+1),])
      print(eurusd.pred$pred)
    }
    np=np+1
  }
  n=n+1
  
  }    
  print(paste("End: ", Sys.time()))
  
  