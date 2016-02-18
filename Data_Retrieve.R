# Data Retrieve
# Michael Kilchenmann

  library(quantmod)
  library(Quandl)
  srcDrc <- getSrcDirectory(function(x) {x})
  rm(list=ls())  
  # Select Asset & Data Source
  asset <- "EUR/USD"
  oanda <- 1
  quandl <- 0
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

  if (quandl==1){
    Quandl.api_key("g1nk1GaNG6mp7zrP5Nps")
    sc.data <- Quandl("CHRIS/CME_WS1", type="xts")
    sc.data <- sc.data$Last
  }
    
  data <- sc.data

