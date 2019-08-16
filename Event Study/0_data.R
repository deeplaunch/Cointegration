# @ Harry Zhao
# Created on 2019/08/16
# Purpose: Analyze Bilateral Swap Line Effect on EM Credit Pricing

rm(list = ls())

setwd("Q:/DATA/SPRAIMU/4_SysRisk/R Code")
data_folder <- "C:/Users/PZhao/Box/Effectiveness/Database/0. Raw data"
saveFolder <- "C:/Users/PZhao/Box/Effectiveness/Database/New Folder"

library(tidyr)
library(dplyr)
library(stringr)
library(lubridate)
library(openxlsx)
library(reshape2)

myfunc <-c('load_and_clean.R','single_table_func.R','reshaping.R','two_table_func.R','process_for_tableau.R')
lapply(myfunc, source, verbose = FALSE)

## Setup Data Files
bsl_file = "Dataset@1.xlsx"
macro_file = "Dataset@2.xlsx"
price_file = "Dataset@3.xlsx"

## Load data

df_bsl <- readxl::read_xlsx(path = paste(data_folder, bsl_file, sep = '/'),
                            sheet= "bsl", na = c('n.a.', '', '.', '#TSREF!','0','#N/A N/A'))
df_cds <- readxl::read_xlsx(path = paste(data_folder, price_file, sep = '/'),
                              sheet= "cds", na = c('n.a.', '', '.', '#TSREF!','0','#N/A N/A'))
df_embi <- readxl::read_xlsx(path = paste(data_folder, price_file, sep = '/'),
                            sheet= "embi", na = c('n.a.', '', '.', '#TSREF!','0','#N/A N/A'))
df_global <- readxl::read_xlsx(path = paste(data_folder, price_file, sep = '/'),
                             sheet= "vix_us10", na = c('n.a.', '', '.', '#TSREF!','0','#N/A N/A'))

## tranform from wide to long format

df_cds <- df_cds %>%melt(
    id.var = c("date"),
    variable.name = "ifs",
    value.name = "cds"
    ) 

df_embi <- df_embi %>%melt(
    id.var = c("date"),
    variable.name = "ifs",
    value.name = "embi"
) 


df_cds$ifs <- as.double(substring(as.character(df_cds$ifs), 2))
df_embi$ifs <- as.double(substring(as.character(df_embi$ifs), 2))

## merge all tables
df_main <- tbl_df(full_join(df_cds, df_embi, by = c('ifs', 'date')))%>%arrange(ifs, date)
df_main <- tbl_df(full_join(df_main, df_global, by = c('date')))%>%arrange(ifs, date)
df_main <- tbl_df(full_join(df_main, df_bsl, by = c('ifs','date')))%>%arrange(ifs, date)

## save resutls
save(df_main, file = paste(saveFolder, "panel data.Rda", sep ="/"))
