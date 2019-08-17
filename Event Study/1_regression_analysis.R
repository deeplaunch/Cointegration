# @ Harry Zhao
# Created on 2019/08/16
# Purpose: Analyze Bilateral Swap Line Effect on EM Credit Pricing


rm(list = ls())

setwd("U:/My Documents/R/Financial Analysis/Event Study")

library(tidyr)
library(dplyr)
library(stringr)
library(lubridate)
library(openxlsx)
library(reshape2)

load("C:/Users/PZhao/Box/Effectiveness/Database/New folder/panel data.Rda")

df_main <- df_main%>%filter(year(date)>= 2000) # drop 8 rows before 2000


## genrate bsl dummy
df_main <- df_main%>%arrange(ifs,date)%>%
    group_by(ifs)%>%
    mutate(bsl_dummy = bsl/bsl)%>%
    ungroup()

## generate unique ID for each bsl event (not necessarily ordered by time) for each borrower
df_main <- df_main%>%group_by(ifs)%>%mutate(bsl_id = cumsum(bsl_dummy))

## generate dummy for t-5 to t+5 by filling between lag and lead

df_main <- df_main%>%mutate(bsl_lag = lag(bsl_dummy, 5))%>%
    mutate(bsl_lead = lead(bsl_dummy, 5) * 2 )

df_main%>%mutate(bsl_merge = bsl_lag + bsl_lead)%>%
    mutate(bsl_range = fill(bsl_merge, direction = 'down'))

df_main[df_main$bsl_range==2,]$max_date <- NA

df_main%>%group_by(ifs, bsl_range)%>%
    mutate(bsl_rel_date = cumsum(bsl_range) - 5)



# ## generate date range for df_bsl (+1, -1) 
# df_main <- df_main%>%group_by(ifs)%>%
#     mutate(min_date = date - 5)%>%
#     mutate(max_date = date + 5) 
# 
# ## set NA for non bsl dates
# df_main[is.na(df_main$bsl),]$max_date <- NA
# df_main[is.na(df_main$bsl),]$min_date <- NA

