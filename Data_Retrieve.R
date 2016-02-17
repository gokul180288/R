# Data Retrieve
# Michael Kilchenmann

  library(quantmod)
  library(Quandl)
  srcDrc <- getSrcDirectory(function(x) {x})
  rm(list=ls())  
  # Select Asset & Data Source
  asset <- "EURUSD"
  oanda <- 0
  quandl <- 1
  google <- 0
  
  if (oanda==1){
  sc.data <- getFX(asset,
                from = Sys.Date() - 360,
                tp = Sys.Date(),
                env = parent.frame(),
                verbose = FALSE,
                warning = TRUE,
                auto.assign = FALSE)
  }
  
  if (google==1){
    sc.data <- getSymbols(asset,
                     from = Sys.Date() - 200,
                     tp = Sys.Date(),
                     #env = parent.frame(),
                     verbose = FALSE,
                     warning = TRUE,
                     auto.assign = FALSE,
                     src="google")
  }

  if (Quandl==1){
    
  }
    
  data <- sc.data

