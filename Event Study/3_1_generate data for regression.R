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
library(RcppRoll)
library(reshape2)

setwd("U:/My Documents/R/Financial Analysis/Event Study")
raw_data_folder <-"C:/Users/PZhao/Box/Effectiveness/Database/0. Raw data"
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

# Aggregare multiple lenders into one
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

##============ Generate rolling sum of residual for t + 5 and t + 120, and t -20 ==========#

main_df <- main_df%>%group_by(ifs)%>%
    arrange(date)%>%
    mutate(e_embi_5d_m = roll_mean(e_embi, n = 5, fill = NA, align = "left"))%>%
    mutate(e_embi_5d_m_l = lag(e_embi_5d_m, n = 5))%>%
    mutate(e_embi_5d_m_diff = (e_embi_5d_m - e_embi_5d_m_l))%>%
    mutate(e_embi_4d_lead = lead(e_embi, n=4))%>%
    mutate(e_embi_1d_lag = lag(e_embi,n=1))%>%
    mutate(e_embi_5d_diff = e_embi_4d_lead - e_embi_1d_lag)%>%
    ungroup()

# View(main_df%>%filter(year(date) >= 2010 & ifs == 536))
# mutate(e_embi_120d_m = roll_mean(e_embi, n = 120, fill = NA, align = "left"))%>%
# mutate(e_cds_5d_m = roll_mean(e_cds, n = 5, fill = NA, align = "left"))%>%
#     mutate(e_cds_l_5d_m = lag(e_cds_5d_m, n = 5, fill = NA, align = "right"))%>%
#     mutate(e_cds_120d_m = roll_mean(e_cds, n = 120, fill = NA, align = "left"))%>%


### Kepp Dummy

main_df <- main_df%>%
    arrange(ifs, date)%>%
    mutate(bsl_dummy = bsl/bsl)

main_df <- main_df%>%filter(bsl_dummy == 1)


##===================== Load GDP =========================##
gdp_file <- "gdp.dta"
gdp_df <- tbl_df(read.dta13((paste(data_folder, gdp_file, sep ='/'))))

# merge with main dataframe
main_df <- main_df%>%mutate(year = year(date))
main_df <- left_join(main_df, gdp_df, by = c("ifs","year"))
main_df%>%arrange(ifs,date)

main_df <- main_df%>%mutate(size = bsl/gdp)

##=========== Generate Dummy for "First" ===============##

main_df <- main_df%>%
    group_by(ifs)%>%
    arrange(date)%>%
    mutate(is_first = row_number())

main_df[which(main_df$is_first>1),]$is_first <- 0

##============= Generate Lagged Reserve to GDP =========#

# Load Reserve
reserve_file <- "res.dta"
reserve_df <- tbl_df(read.dta13((paste(data_folder, reserve_file, sep ='/'))))
reserve_df <- left_join(reserve_df,gdp_df, by=c("ifs","year"))

# Generate Lag
reserve_df <- reserve_df%>%
    mutate(res_gdp = res/gdp)%>%
    group_by(ifs)%>%
    arrange(year)%>%
    mutate(res_gdp_lag = lag(res_gdp))%>%
    ungroup()
# Merge with main dataframe
reserve_df <- reserve_df%>%
    select(ifs,year,res,res_gdp_lag)

main_df <- left_join(main_df,reserve_df, by = c("ifs","year"))
main_df <- main_df%>%ungroup()


##====== Generate Lagged Reserve to M2 ===========#

# Load M2
m2_file <- "m2.dta"
m2_df <- tbl_df(read.dta13((paste(data_folder, m2_file, sep ='/'))))
m2_df <- left_join(reserve_df,m2_df, by=c("ifs","year"))

# Generate Lag
m2_df <- m2_df%>%
    mutate(res_m2= res/m2)%>%
    group_by(ifs)%>%
    arrange(year)%>%
    mutate(res_m2_lag = lag(res_m2))%>%
    ungroup()
# Merge with main dataframe
m2_df <- m2_df%>%
    select(ifs,year,m2,res_m2_lag)

main_df <- left_join(main_df,m2_df, by = c("ifs","year"))
main_df <- main_df%>%ungroup()


##====== Generate Lagged Short-term Debt to GDP ===========#

# Load Short-term Debt
debt_file <- "debt.dta"
debt_df <- tbl_df(read.dta13((paste(data_folder, debt_file, sep ='/'))))
debt_df <- left_join(gdp_df,debt_df, by=c("ifs","year"))

# Generate Lag
debt_df <- debt_df%>%
    mutate(debt_gdp= debt/gdp)%>%
    group_by(ifs)%>%
    arrange(year)%>%
    mutate(debt_gdp_lag = lag(debt_gdp))%>%
    ungroup()
# Merge with main dataframe
debt_df <- debt_df%>%
    select(ifs,year,debt,debt_gdp_lag)

main_df <- left_join(main_df,debt_df, by = c("ifs","year"))
main_df <- main_df%>%ungroup()

##======== Generate Lagged Short-term Debt to M2 ===========#

main_df <- main_df%>%mutate(debt_m2 = debt/m2)%>%
    group_by(ifs)%>%
    arrange(year)%>%
    mutate(debt_m2_lag = lag(debt_m2))%>%
    ungroup()%>%
    select(-debt_m2)

##================= Load Program Dummy ===================#
prog_file <- "program.xlsx"
df_prog <- readxl::read_xlsx(path = paste(raw_data_folder, prog_file, sep = '/'))
df_prog<- df_prog%>%select(countrycode, year, program)
colnames(df_prog) <- c('ifs','year','program')
main_df <- left_join(main_df,df_prog, by=c("ifs","year"))

##================ Generate Lagged Current Account to GDP ==========##
ca_file <- "Current Account.xlsx"
df_ca_gdp <- readxl::read_xlsx(path = paste(raw_data_folder, ca_file, sep = '/'),sheet ="ca",na = c('n.a.', '', '.', '#TSREF!','0','#N/A N/A'))

df_ca_gdp <- df_ca_gdp%>%
    select(CountryCode, starts_with('2'))%>%
    gather(year, ca_gdp, -CountryCode)

colnames(df_ca_gdp)[1] <- "ifs"
df_ca_gdp$ year <- as.double(df_ca_gdp$year)

df_ca_gdp <- df_ca_gdp%>%
    group_by(ifs)%>%
    arrange(year)%>%
    mutate(ca_gdp_lag = lag(ca_gdp))%>%
    ungroup()%>%
    select(-ca_gdp)

main_df <- left_join(main_df,df_ca_gdp, by=c("ifs","year"))
##======================= Save Results =========================##
hist(main_df$e_embi_5d)
hist(main_df$e_cds_5d)
save(main_df, file = paste(saveFolder, "event_panel_data.Rda", sep ="/"))



# ### Alternative code for generating Dummy for bsl t to t+4
# main_df <- main_df%>%
#     arrange(ifs, date)%>%
#     mutate(bsl_dummy = bsl/bsl)%>%
#     group_by(ifs)%>%
#     mutate(bsl_dummy_lag = lag(bsl_dummy, 5)*2)%>%
#     ungroup()
# 
# main_df <- main_df%>%
#     mutate(range = rowSums(cbind(bsl_dummy, bsl_dummy_lag), na.rm =TRUE))%>%
#     ungroup()
# 
# ### Select only bsl_range is 1
# main_df[which(main_df$range==0),]$range<- NA
# 
# main_df <- main_df%>%
#     group_by(ifs)%>%
#     fill(range)%>%
#     ungroup()
# 
# df_5d <- main_df%>%filter(range ==1)
# 
# main_df <- main_df%>%arrange(ifs,date)%>%
#     group_by(ifs)%>%
#     fill(event_id)%>%
#     ungroup()
# 
# ### Get sum of residuals for each event
# main_df <- main_df%>%group_by(event_id)%>%
#     summarise(date=min(date), ifs = first(ifs), cds = first(cds), embi = first(embi), china = first(china), 
#               e_embi_5d = sum(e_embi), e_cds_5d = sum(e_cds), vix = first(vix), us10 = first(us10),
#               lender = first(lender.x), borrower = first(borrower), bsl = first(bsl), all_lender = first(all_lender))
