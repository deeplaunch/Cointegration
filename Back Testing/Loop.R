## This function loops over each day to calculate PnL (USD$), Account Size (USD$) and Trade Size (# of contracts) ##

do_daily_strategy <- function(market2,  AS_Initial = 1000000 , PV = 20 , StartRow = 506){

  dates <- market2$Date%%100
  dates_lag <- lag(dates)
  
  market2<-mutate(market2,AS = 0,beg_of_month = (dates_lag > dates),AS_BoM = NA, AS = NA , TS= NA)
  
  market2[StartRow,"AS"]<- AS_Initial
  
  for (row in StartRow+1:(nrow(market2)-StartRow)) {
    
      if ( market2[row ,"Trade"] == 0) {
          market2[row, "TS"] <- market2[row-1,"TS"]
          market2[row, "PnL_Total"]<- market2[row,"TS"]*(market2[row,"Close"]-market2[row-1,"Close"])
      } else{
          market2[row,"TS"] <- market2[row-1,"AS"]/(PV * market2[row-1 ,"VOL"])
          market2[row,"PnL_Total"]<- market2[row, "Holding"] * (market2[row,"TS"]*(market2[row,"Close"]-market2[row,"Open"]) + market2[row-1,"TS"]* (market2[row,"Open"]- market2[row-1,"Close"]) )                                       
      }
    
      market2[row,"AS"] <- market2[row-1,"AS"] + market2[row,"PnL_Total"]
    
      if (market2[row,"beg_of_month"]==TRUE) {
          market2[row,"AS_BoM"] <- market2[row,"AS"]
      } else{
          market2[row,"AS_BoM"] <- market2[row-1 ,"AS_BoM"]
      }
  }
  
  return (market2)
}