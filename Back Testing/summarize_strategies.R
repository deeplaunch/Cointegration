summarize_strategies <- function (strategy_basket, startDate = 19900101, nameList) {

  library(lubridate)
  library(reshape)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  
  category <- nameList%>%distinct(SectorGrp)
  
  newNames <- names(strategy_basket)
  
  pnl_basket <- lapply(names(strategy_basket), function (x) 
    select(strategy_basket[[x]], Date, PnL)%>%
      filter(Date >= startDate)
  )
  
  pnl_basket <- Reduce(function(...) merge(..., all=TRUE, by = "Date"), pnl_basket)
  
  pnl_basket <- setNames(pnl_basket, c("Date", newNames))
  
  pnl_basket$Date <- ymd(pnl_basket$Date)
  
  pnl_basket <- tbl_df(pnl_basket)
  
  
  pnl_sum_length <- dim(pnl_basket)[1]
  pnl_sum_width <- dim(category)[1] +1
  
  
  pnl_sum <- setNames(data.frame(matrix(ncol = pnl_sum_width, nrow = pnl_sum_length)), c("Date",c(t(category))))%>%as_tibble
 
  pnl_sum$Date <- pnl_basket$Date
  
  cum_pnl_sum <- setNames(data.frame(matrix(ncol = pnl_sum_width, nrow = pnl_sum_length)), c("Date",c(t(category))))%>%as_tibble
  
  cum_pnl_sum$Date <- pnl_basket$Date
  
  cum_pnl_return <- setNames(data.frame(matrix(ncol = pnl_sum_width, nrow = pnl_sum_length)), c("Date",c(t(category))))%>%as_tibble
  
  cum_pnl_return$Date <- pnl_basket$Date
  
  category <- c(t(category))
  
  for (i in category) {
    sub_basket <- pnl_basket%>%select(c(t(nameList%>%filter(SectorGrp == toupper(i))%>%select(Contract))))
    pnl_sum[i] <- rowSums(sub_basket, na.rm = TRUE)
    cum_pnl_sum[i] <- cumsum(pnl_sum[i])
    total_beg <- 100000 * dim(sub_basket)[2]
    cum_pnl_return[i] <- cum_pnl_sum[i] /total_beg +1
  }
  
   ggplot(data = cum_pnl_return, aes(x= Date, y= value)) + 
   geom_line(aes(y = COMMODITY)) +
   geom_line(aes(y = EQUITY)) +
   geom_line(aes(y = FI)) +
   geom_line(aes(y = FX)) 
  
  return (category)
}