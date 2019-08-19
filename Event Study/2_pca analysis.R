# @ Harry Zhao
# Created on 2019/08/16
#========== Project: Analyze Bilateral Swap Line Effect on EM Credit Pricing
#========== Purpose: Iteracte each event: regress on PCA components, get residual

rm(list = ls())

setwd("U:/My Documents/R/Financial Analysis/Event Study")

library(tidyr)
library(dplyr)
library(stringr)
library(lubridate)
library(openxlsx)
library(reshape2)

load("C:/Users/PZhao/Box/Effectiveness/Database/New folder/event panel list.Rda")
saveFolder <- "C:/Users/PZhao/Box/Effectiveness/Database/New Folder"

#df_panel <- df_panel_list$`2011-11-01`
df_panel<- df_panel_list$`2000-05-01`

add_log_diff <- function(df){
    ## Add log difference to the panel dataset
    
    df <- df%>%mutate(embi_lg = log(embi))
    
    df <- df%>%group_by(ifs)%>%
        arrange(date,lender)%>%
        mutate(embi_lg_d = embi_lg - lag(embi_lg, 1))%>%
        ungroup()
    
    return (df)
}

select_data <-function(df){
    ## Prepare for PCA ##
    df_embi <- df%>%
        select(date,ifs,embi_lg_d)%>%
        unique()%>%
        group_by(date,ifs)%>%
        arrange(lender)%>%
        summarise(embi_lg_d = first(embi_lg_d))%>%
        ungroup()%>%
        spread(ifs, embi_lg_d)
    
    ## replace value = 0 with NAs
    df_embi <- df_embi%>%na_if(0) # df_embi <- df_embi%>%fill(-date, .direction ="up") # NA fill-up
    
    ## keep only columns with less than 100 NAs and non-constant
    df_embi <- df_embi[ , colSums(is.na(df_embi)) <= 40]
    df_embi <- df_embi[, sapply(df_embi, function(v) var(v, na.rm=TRUE)!=0)]
    
    ## keep rows with more than 5 values
    df_embi <- df_embi%>%
        mutate(not_na_count = rowSums(!is.na(df_embi)))%>%
        filter(not_na_count >= 5)%>%
        select(-not_na_count)
    
    return (df_embi)
}

get_pca_res <- function(df_embi, df_panel){
    
    ## Perform PCA, get residuals from fitting 1st-principle component
    pca_embi <- prcomp(na.exclude(df_embi%>%select(-date)), center = TRUE, scale = TRUE)
    comp_1 <- pca_embi$x[,"PC1"]
    Y <- na.omit(as.matrix.data.frame(df_embi%>%select(-date)))
    X <- matrix(comp_1,  ncol = 1)
    fit<- lm(Y~X)
    
    res <- resid(fit)
    df_res <- tbl_df(res)
    df_res$date <- na.exclude(df_embi)$date
    df_res <- df_res%>%gather(key = "ifs", value="pca_res", -date)
    df_res$ifs <- as.double(df_res$ifs)
    
    df_res <- left_join(df_panel, df_res, by = c("date","ifs"))
    
    return (df_res)
}


do_pca_analysis <-function(df){
    
    ## main function for getting pca residual
    df_panel <- add_log_diff(df)
    df_embi <- select_data(df_panel)
    df_res <- get_pca_res(df_embi, df_panel)
    
    return (df_res)
}

df_pca_res_list <- lapply(df_panel_list, do_pca_analysis)


#plot(pca_embi$x[,"PC1"],pca_embi$x[,"PC2"], xlab="PC1 (75.4%)", ylab = "PC2 (14.2%)", main = "PC1 / PC2 - plot")
#plot(pca_embi$x[,"PC1"], df_embi$`298`, xlab ="PC1", ylab ="Country_298")
#screeplot(pca_embi)
#summary(pca_embi)
#str(pca_embi)
#cor(df_embi$`186`, pca_embi$x[,"PC1"])