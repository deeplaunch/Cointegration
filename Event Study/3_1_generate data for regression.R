# @ Harry Zhao
# Created on 2019/08/28
# Project: Analyze Bilateral Swap Line Effect on EM Credit Pricing
# Purpose: Take residuls from earlier regression of credit pricing on global risk factors (US 10yr, VIX) 
# and build 5-day and 120-day rolling sum for each event.

rm(list = ls())

library(readstata13)
library(tidyr)
library(dplyr)
library(lubridate)


setwd("U:/My Documents/R/Financial Analysis/Event Study")
data_folder <- "C:/Users/PZhao/Box/Effectiveness/Database/2. Database"
saveFolder <- "C:/Users/PZhao/Box/Effectiveness/Database/4. R data"

# Load Residuls
daily_file <- "e_embicds_long.dta"
monthly_file <- "e_embicds_long_m.dta"

daily_df <- read.dta13(paste(data_folder, daily_file, sep ='/'))
daily_df <- tbl_df(daily_df)

# Load BSL event dummy
load(paste(saveFolder, "panel data.rda", sep = "/"))

# Merge two dataframes
main_df <- left_join(daily_df, df_main, by = c("date","ifs","embi","cds"))


# aggregare multiple lenders into one
event_df <- main_df%>%filter(bsl>0)
short_event_df<- event_df%>%
    select(date,ifs,lender)%>%
    group_by(date,ifs)%>%
    summarise(all_lender = paste(lender, collapse = ','), lender = first(lender))%>%
    ungroup()

short_event_df <-short_event_df%>%
    arrange(date)%>%
    mutate(event_id = row_number())

main_df <- left_join(main_df,short_event_df,by = c("date","ifs"))%>%
    filter(lender.x == lender.y | (is.na(lender.x) & is.na(lender.y)))

### Generate Dummy for bsl t to t+4
main_df <- main_df%>%arrange(ifs, date)%>%
    mutate(bsl_dummy = bsl/bsl)%>%
    group_by(ifs)%>%
    mutate(bsl_dummy_lag = lag(bsl_dummy, 5)*2)%>%
    ungroup()

main_df <- main_df%>%
    rowwise()%>%
    mutate(range = sum(bsl_dummy, bsl_dummy_lag, na.rm =TRUE))%>%
    ungroup()

### select only bsl_range is 1
main_df[which(main_df$range==0),]$range<- NA

main_df <- main_df%>%
    group_by(ifs)%>%
    fill(range)%>%
    ungroup()

main_df <- main_df%>%filter(range ==1)

main_df <- main_df%>%arrange(ifs,date)%>%
    group_by(ifs)%>%
    fill(event_id)%>%
    ungroup()

### Get sum of residuals for each event
main_df <- main_df%>%group_by(event_id)%>%
    summarise(date=min(date), ifs = first(ifs), cds = first(cds), embi = first(embi), china = first(china), 
              e_embi_5d = sum(e_embi), e_cds_5d = sum(e_cds), vix = first(vix), us10 = first(us10),
              lender = first(lender.x), borrower = first(borrower), bsl = first(bsl), all_lender = first(all_lender))


View(main_df%>%filter(year(date) >= 2010 & ifs == 536))

### save
hist(main_df$e_embi_5d)
hist(main_df$e_cds_5d)
save(main_df, file = paste(saveFolder, "event_data.Rda", sep ="/"))
