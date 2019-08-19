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

df_panel <- df_panel_list$`2011-11-01`

###################### Prepare for PCA #########################
## generate log difference


df_panel <- df_panel%>%mutate(embi_lg = log(embi))

df_panel <- df_panel%>%group_by(ifs)%>%
    arrange(date)%>%
    mutate(embi_lg_d = embi_lg - lag(embi_lg, 1))%>%
    ungroup()

df_embi <- df_panel%>%select(date,ifs,embi_lg_d)%>%
    unique()%>%
    spread(ifs, embi_lg_d)

##  replace value = 0 with NAs, remove rows with all NAs
df_embi <- df_embi%>%na_if(0) # df_embi <- df_embi%>%fill(-date, .direction ="up") # NA fill-up

df_embi <- df_embi%>%mutate(na_count = rowSums(is.na(df_embi)))%>%
    filter(na_count<= 30)%>%select(-na_count)

## select only columns with less than 100 NAs and non-constant
df_embi <- df_embi[ , colSums(is.na(df_embi)) <= 50]
df_embi <- df_embi[,sapply(df_embi, function(v) var(v, na.rm=TRUE)!=0)]

################## Perform PCA, get residual from fitting 1st-principle component ################

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

#plot(pca_embi$x[,"PC1"],pca_embi$x[,"PC2"], xlab="PC1 (75.4%)", ylab = "PC2 (14.2%)", main = "PC1 / PC2 - plot")
#plot(pca_embi$x[,"PC1"], df_embi$`298`, xlab ="PC1", ylab ="Country_298")
#screeplot(pca_embi)
#summary(pca_embi)
#str(pca_embi)
#cor(df_embi$`186`, pca_embi$x[,"PC1"])