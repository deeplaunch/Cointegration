### Main Setup###

rm(list=ls())

library(dplyr)
library(tidyr)
library(stringr)
library(zoo)
library(readxl)
library(lubridate)
library(reshape)
library(ggplot2)

source("C:/Users/PZhao/Desktop/R/code/do_daily_calc.R")
source ("C:/Users/PZhao/Desktop/R/code/do_single_strategy.R")
source ("C:/Users/PZhao/Desktop/R/code/summarize_strategies.R")


main_folder<- 'C:/Users/PZhao/Desktop/R/'
setwd(main_folder)
sub_folder_price <- './Data - 24 markets/'
pv_file <- './Support/MarketsList.xlsx'

# load text data files into R to get daily market data

list_of_files = list.files('./Data - 24 markets/')

new_names<-str_replace_all(list_of_files,"_EOD.TXT","")

DT <- sapply(paste(sub_folder_price,list_of_files, sep = ""), read.table, simplify = FALSE, sep =",")

names(DT) <- new_names

DT <- lapply(DT,setNames,c('Date','Open','High','Low','Close','Other1','Other2'))

DT <- lapply(DT, as_tibble) # Transform from dataframe to table for more efficient manipulation
 
DT <- lapply(DT, select, Date, Open, Close)


# load excel files into R to get contract-level data

PV_DT<- read_excel(pv_file)
PV_DT$Contract[PV_DT$Contract=="FEI"] <-"FEI2" # Rename to stay consistent with the price file
row.names(PV_DT)<- PV_DT$Contract
PV_DT<- as.data.frame(t(PV_DT))


strategy_MA <- mapply(do_single_strategy, market = DT, PV = PV_DT, strategy = "MA", shortT = 10, longT = 100, from = 19900101, USE.NAMES = TRUE, SIMPLIFY = FALSE)

strategy_CB <- mapply(do_single_strategy, market = DT, PV = PV_DT, strategy = "CB", lookback=50, from = 19900101, USE.NAMES = TRUE, SIMPLIFY = FALSE)

# summarize strategies

nameList <- tbl_df(t(PV_DT))%>%select(SectorGrp, Contract)

summarize_strategies (strategy_MA, startDate = 19900101, nameList)

summarize_strategies (strategy_CB, startDate = 19900101, nameList)

