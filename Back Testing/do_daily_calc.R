library(dplyr)
library(stringr)
library(zoo)
library(tidyr)

############################################################################################################################
## This function loops over each day to calculate PnL (USD$) and Trade Size (# of contracts) ##

do_daily_calc<- function(market2,  AS = 1000000 , PVConstant = 20 , StartRow ){
  
  dates <- market2$Date%%100
  dates_lag <- lag(dates)
  
  market2<- mutate(market2,AS = AS,beg_of_month = (dates_lag > dates), TS= NA, PnL=NA)
  
  # Trading day TS
  row_trading <- market2$Trade!= 0 & market2$Date >= market2$Date[StartRow]
  row_holding <- !row_trading & market2$Date >= market2$Date[StartRow]
  
  row_trading[is.na(row_trading)] <- FALSE
  market2$TS[row_trading] <- 0.001* AS /(PVConstant * market2$VOL[row_trading])
 
  # Non-trading day TS, roll from previous trading day
  
  market2 <- market2%>% fill(TS)

  #Trading day PnL, return_old_trade from today's Open- yeserday's Close + return_new_trade from today's Close -Open
  
  TS_lag <- lag(market2$TS)
  Close_lag <- lag(market2$Close)
  
  market2$PnL[row_trading] <- market2$TS[row_trading] * (market2$Close[row_trading]- market2$Open[row_trading]) + TS_lag[row_trading] *(market2$Open[row_trading]- Close_lag[row_trading]) 
  
  # Non-trading day PnL, return from yeserday's Close 
  
  market2$PnL[row_holding] <- TS_lag[row_holding] * (market2$Close[row_holding] - Close_lag[row_holding])

  return (market2)
}