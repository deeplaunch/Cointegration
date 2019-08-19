# @ Harry Zhao
# Created on 2019/08/16
# Project: Analyze Bilateral Swap Line Effect on EM Credit Pricing
# Purpose: Initial loading from raw excel files, generate one panel for further process

rm(list = ls())

setwd("U:/My Documents/R/Financial Analysis/Event Study")
data_folder <- "C:/Users/PZhao/Box/Effectiveness/Database/0. Raw data"
saveFolder <- "C:/Users/PZhao/Box/Effectiveness/Database/New Folder"

library(tidyr)
library(tidyverse)
library(dplyr)
library(stringr)
library(lubridate)
library(openxlsx)
library(reshape2)

# myfunc <-c('load_and_clean.R','single_table_func.R','reshaping.R','two_table_func.R','process_for_tableau.R')
# lapply(myfunc, source, verbose = FALSE)

## Setup Data Files
bsl_file = "Dataset@1.xlsx"
macro_file = "Dataset@2.xlsx"
price_file = "Dataset@3_2.xlsx"

## Load data (specify numerical columns for price data)

df_cds <- readxl::read_xlsx(path = paste(data_folder, price_file, sep = '/'),
                            sheet= "cds", na = c('n.a.', '', '.', '#TSREF!','0','#N/A N/A'),
                            col_types = c("date", rep(c("numeric"),77)))

df_embi <- readxl::read_xlsx(path = paste(data_folder, price_file, sep = '/'),
                            sheet= "embi", na = c('n.a.', '', '.', '#TSREF!','0','#N/A N/A'),
                            col_types = c("date", rep(c("numeric"),83)))


df_bsl <- readxl::read_xlsx(path = paste(data_folder, bsl_file, sep = '/'),
                            sheet= "bsl", na = c('n.a.', '', '.', '#TSREF!','0','#N/A N/A'))

df_global <- readxl::read_xlsx(path = paste(data_folder, price_file, sep = '/'),
                             sheet= "vix_us10", na = c('n.a.', '', '.', '#TSREF!','0','#N/A N/A'))

## tranform from wide to long format

df_cds <- tbl_df(df_cds %>%melt(
    id = c("date"),
    variable.name = "ifs",
    value.name = "cds"
    ))

df_embi <- tbl_df(df_embi %>%melt(
    id= c("date"),
    variable.name = "ifs",
    value.name = "embi"
) )

## convert to double
df_cds$ifs <- as.double(as.character(df_cds$ifs))
df_embi$ifs <- as.double(as.character(df_embi$ifs))

## merge all tables
df_main <- tbl_df(full_join(df_cds, df_embi, by = c('ifs', 'date')))%>%arrange(ifs, date)
df_main <- tbl_df(full_join(df_main, df_global, by = c('date')))%>%arrange(ifs, date)
df_main <- tbl_df(full_join(df_main, df_bsl, by = c('ifs','date')))%>%arrange(ifs, date) 
# note: IFS was mapped to "borrower", whose credit pricing we are interested in

## save results as a list of panels, each panel corresponds to one event
df_main$date <- date(df_main$date)

View(df_main%>%filter(date == '2009-12-01'))

df_main <- df_main%>%filter(!is.na(ifs))

save(df_main, file = paste(saveFolder, "panel data.Rda", sep ="/"))
