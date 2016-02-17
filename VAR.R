# VAR model
# Michael Kilchenmann
# February 2016
  
  print(paste("Start: ", Sys.time()))
  rm(list=ls())
  
  library(Quandl)
  library(quantmod)
  srcDrc <- getSrcDirectory(function(x) {x})
  oanda <- 0
  quandl <- 0
  google <- 1
  if (oanda==1){
    sc.data <- getFX("EUR/USD",
                  from = Sys.Date() - 360,
                  tp = Sys.Date(),
                  env = parent.frame(),
                  verbose = FALSE,
                  warning = TRUE,
                  auto.assign = FALSE)
  }
  if (quandl==1){
    sc.data <-Quandl("CHRIS/ICE_T1", type="xts", start_date="2014-12-31", collapse="daily", force_irregular=TRUE)
    sc.data <- tail(sc.data$Settle,400)
  }
  if (google==1){
    sc.data <- getSymbols("YHOO",src="google", # from google finance 
    from = Sys.Date() - 200,
    tp = Sys.Date(),
    env = parent.frame(),
    verbose = FALSE,
    warning = TRUE,
    auto.assign = FALSE)
  }
  
    actual.diff <- diff(sc.data)
  np <- 0
  n <- 0
  
  for(j in 100:358){
      
      data <- sc.data[(j-99):j,]
      
      # data <- head(data,-3)
        
      # plot switch (i=1 -> plotting / i = 0 -> no plotting)
        i <- 0
        data_fd <- diff(data)
        
      # Overview
        if (i==1){
          par(mfrow=c(3,3))
          plot(data, main="data 500 days rolling")
          plot(data_fd, main = "first differences 500 days rolling")
          hist(data_fd, main = "histogramm of fd")
          acf(data, main = "ACF data")
          acf(data_fd, main = "ACF fd", na.action=na.omit)
          pacf(data_fd, main = "PACF fd",  na.action=na.omit)
        }
        
        data.fit <- arima(data_fd, order=c(1,0,1), include.mean=FALSE)
          # optional: seasonal=list(order=c(1,1,0), period=12), include.mean=FALSE)
          # include.mean=TRUE -> assume constant trend in time series
        
        if (i==1){
          print(data.fit)
        }
        
        data.pred <-predict(data.fit, n.ahead=1)
        data_fd <- ts(data_fd)
        
        if (i==1){
          plot(data_fd)
          lines(data.pred$pred, col="blue")
          # see how well model predicts
          lines(data.pred$pred + 2*data.pred$se, col="red")
          lines(data.pred$pred - 2*data.pred$se, col="red")
          # abline(h=0.002, col="red)
        }
        
        if (i==1){
          print(data.pred$pred)
        }

      # Calculation of Hit Rate          
        if (sign(actual.diff[(j+1),])==sign(data.pred$pred)){
          if(i==1){
          print(actual.diff[(j+1),])
          print(data.pred$pred)
          }
          np=np+1
        }
        n=n+1
  }
  
  print(np/n)
  print(paste("End: ", Sys.time()))
  
  