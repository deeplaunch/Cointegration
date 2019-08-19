# @ Harry Zhao
# Created on 2019/08/16
#========== Project: Analyze Bilateral Swap Line Effect on EM Credit Pricing
#========== Purpose: Generate a list of full panel, whose every element is for an event date

rm(list = ls())

setwd("U:/My Documents/R/Financial Analysis/Event Study")

library(tidyr)
library(dplyr)
library(stringr)
library(lubridate)
library(openxlsx)
library(reshape2)

load("C:/Users/PZhao/Box/Effectiveness/Database/New folder/panel data.Rda")
saveFolder <- "C:/Users/PZhao/Box/Effectiveness/Database/New Folder"

############################ 1. data cleaning ########################

df_main <- df_main%>%filter(year(date)>= 2000) # drop 8 rows before 2000
df_main$date <- as.Date(df_main$date)
## View(df_main%>%filter(bsl>0))

## 1. genrate bsl event dummy
df_main <- df_main%>%arrange(date)%>%
    group_by(ifs)%>%
    mutate(bsl_dummy = bsl/bsl)%>%
    ungroup()

## generate unique ID for each bsl event identified by Date

df_unique_date <- df_main%>%filter(bsl_dummy ==1)%>%
    arrange(date)%>%
    select(date)%>%
    distinct()%>%
    mutate(bsl_id = row_number(date))

# df_main <- df_main%>%mutate(bsl_id = paste(replace_na(borrower,""), as.character(date), sep = "_"))
# df_main[ str_length(df_main$bsl_id)==11, ]$bsl_id <- NA

################# 2. loop over each unique bsl_id ################

date_list <- list() # Very important step to convert to a list of strings
date_list[as.character(df_unique_date%>%pull(date))] <- as.character(df_unique_date$date)

## use vectorization to save time

generate_event_data<- function(event_date_str){
    
    ## Each event is selected regardless of potentially overlapping with other events
    event_date <- as.Date(event_date_str)
    # use t +/- 200
    df_panel <- df_main%>%
        filter(date >= as.Date(event_date- 200) & date <= as.Date(event_date + 200))
    df_panel$bsl_id <- event_date

    return (df_panel)
}

df_panel_list <- lapply(date_list, generate_event_data)

####################### 3. save results ###############################
print(paste("Total number of events saved:",length(date_list)))
save(df_panel_list, file = paste(saveFolder, "event panel list.Rda", sep ="/"))

## df_event_panel <- data.frame(Reduce(rbind, df_panel_list)) # Take very long to process

# ## generate date range for df_bsl (+1, -1) 
# df_main <- df_main%>%group_by(ifs)%>%
#     mutate(min_date = date - 5)%>%
#     mutate(max_date = date + 5) 
# 
# ## set NA for non bsl dates
# df_main[is.na(df_main$bsl),]$max_date <- NA
# df_main[is.na(df_main$bsl),]$min_date <- NA