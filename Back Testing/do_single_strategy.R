
############################################################################################################################
## This function computes strategies
do_single_strategy <- function(market, PV, strategy="CB", lookback=50, shortT = 10, longT = 100, from = 19900101) {
  # Setup
  VOL <- rollapplyr(market$Close, 100, FUN = sd, fill = NA)
  
  # Create Trading Siganls for both types of strategies
  
  if(strategy=="CB") {  # Channel Breakout 
    HC <- rollmaxr(market$Close, lookback, fill = NA)
    LC <- -rollmaxr(-market$Close, lookback, fill = NA)
    Long <- (market$Close == HC) & (!is.na(HC))
    Short <- (market$Close == LC) & (!is.na(LC))
  }
  
  if(strategy=="MA") {  # Moving Average
    MA_ShortT <- rollmean(market$Close,shortT, fill = NA)
    MA_LongT <- rollmean(market$Close,longT, fill = NA)
    Long <- MA_ShortT > MA_LongT & !is.na(MA_LongT)
    Short <- MA_ShortT < MA_LongT & !is.na(MA_LongT)
  }
  
  # Calculate Long/Short directions
  Long <-lag(Long)
  Short <- lag(Short)
  
  Signal <- (Long == TRUE) - (Short == TRUE) # 1 = Long, -1 = Short, 0/NA = No
  Signal[Signal == 0]<-NA
  
  Holding <- na.locf(Signal, na.rm = FALSE) # Filldown Signal, 1= Long position, -1 = Short position
  Holding_1 <- lag(Holding)
  
  Trade <- (Holding-Holding_1) / 2 # 1 = Buy, -1 = Sell, 0 = Hold
  
  # Calculate starting date and index
  
  StartDate <- min(market$Date[!is.na(VOL)])  # initialize as first date with Vol value
  
  if (StartDate < from ) {
      Index <- match(c(from:(from+5)), market$Date) # Allow for a few missing dates
      Index <- min(Index[!is.na(Index)])
      Trade[Index]<- Holding[Index]  # First Trade direction always same as holding If VOL available on 19900101
      StartDate <- market$Date[Index]
      
  } else{
      
      Index <- min(match(c(1,-1),Signal)) #First day with signal
      
      if (StartDate < market$Date[Index]) {
          StartDate <- market$Date[Index]
          Trade[Index] <- Signal[Index]  # Vol exists before first signal date, first trade direction always the same as signal
      } else {
          Index <- match(StartDate,market$Date) + 1
          Trade[Index] <- Holding[Index]   # Vol exists after first signal date, first trade direction always the same as signal
      }
  }
  
  Holding[1: Index-1] <- 0
  
  #Put everything together
  
  market <- mutate(market, Signal, Trade, Holding, VOL)
  
  #Calculate PV
  
  
  PVConstant <- as.numeric(PV[4])
  PVOperation <- PV[5]

  if (PVOperation == "DIV" & !is.na(PVOperation)) {
    PVConstant <- 1/ PVConstant
  }

  # Calculate Trade Size TS, PnL and Account Size AS by running loop everyday
  
  market <- do_daily_calc (market,  AS = 1000000 , PVConstant = PVConstant , StartRow = Index)
 
  return (market)
}
