# Installing packges
#tidyverse 
#here
#janitor
#skimr
#reshape2
#plm

# load packges 
library(tidyverse)
library(here)
library(janitor)
library(skimr)
library(reshape2)
library(plm)
library(marginaleffects)
library(fixest)

getwd()
setwd("Desktop/R working folder 2022_BV")
list.files()
#read in data
ESG_scores_G_B <- read.csv("Data ESG copy/ESG scores- Governance.csv", sep=";", dec=",")
ESG_scores_O_B <- read.csv("Data ESG copy/ESG scores- Overall .csv", sep=";", dec=",")
ESG_scores_E_B <- read.csv("Data ESG copy/ESG scores-Environmental.csv", sep=";", dec=",")
ESG_scores_S_B <- read.csv("Data ESG copy/ESG scores-Social.csv", sep=";", dec=",")
Data_Orbis_BV <- read.csv("Data orbis BV/Data Beverages.csv", sep=";" , dec=",", na.strings = "n.a.")
str(Data_Orbis_BV)
#not used below
#Data_Orbis$Profit.margin.....2012<-sub("n.s.", Data_Orbis$Profit.margin.....2012, NA)

#viewing data 

View(ESG_scores_G_B)
View(ESG_scores_O_B)
View(ESG_scores_E_B)
View(ESG_scores_S_B)
View(Data_Orbis_BV)

# 1. get complete data (inputs)
# 2. make sure everything is properly imported
# 3. merge bz company ID and year (using either merge() or join())


#New material 
#Overall ESG melting------------------------------------------------------  
MELT_O_ESG <- melt (ESG_scores_O_B, id.vars = "ISIN.code")
MELT_O_ESG $variable <- as.numeric(substr(MELT_O_ESG $variable,3,6)) 

colnames(MELT_O_ESG)[3]<-"ESG_O"
colnames(MELT_O_ESG)[2]<-"year"
View(MELT_O_ESG)

#Envirioment ESG melt
MELT_E_ESG <- melt (ESG_scores_E_B, id.vars = "ISIN.code")
MELT_E_ESG $variable <- as.numeric(substr(MELT_E_ESG $variable,3,6)) 

colnames(MELT_E_ESG)[3]<-"ESG_E"
colnames(MELT_E_ESG)[2]<-"year"
View(MELT_E_ESG)

#Social ESG melt 
MELT_S_ESG <- melt (ESG_scores_S_B, id.vars = "ISIN.code")
MELT_S_ESG $variable <- as.numeric(substr(MELT_S_ESG $variable,3,6)) 

colnames(MELT_S_ESG)[3]<-"ESG_S"
colnames(MELT_S_ESG)[2]<-"year"
View(MELT_S_ESG)

#Governance ESG melt 
MELT_G_ESG <- melt (ESG_scores_G_B, id.vars = "ISIN.code")
MELT_G_ESG $variable <- as.numeric(substr(MELT_G_ESG $variable,3,6)) 

colnames(MELT_G_ESG)[3]<-"ESG_G"
colnames(MELT_G_ESG)[2]<-"year"
View(MELT_G_ESG)

#Orbis melting --------------------------------------------------------

#Ebitda
MELT_EBITDA <- Data_Orbis_BV
MELT_EBITDA <-melt(MELT_EBITDA [,1:10], id.vars = c("ISIN.code"))
MELT_EBITDA $variable <- as.numeric(substr(MELT_EBITDA $variable,8,11)) 
colnames(MELT_EBITDA)[3]<-"EBITDA"
colnames(MELT_EBITDA)[2]<-"year"
View(MELT_EBITDA)
#names 

MELT_names <- Data_Orbis_BV [,-c(1:121)]
MELT_names <-melt(MELT_names [1], id.vars = c("ISIN.code"))
view(MELT_names)

#EBITDA_Margin

MELT_EBITDA_M <- Data_Orbis_BV [,-c(2:11)]
MELT_EBITDA_M <-melt(MELT_EBITDA_M [,1:10], id.vars = c("ISIN.code"))
MELT_EBITDA_M $variable <- as.numeric(substr(MELT_EBITDA_M $variable,10,13)) 
colnames(MELT_EBITDA_M)[3]<-"EBITDA_M"
colnames(MELT_EBITDA_M)[2]<-"year"
View(MELT_EBITDA_M)


#Total sales
MELT_S <- Data_Orbis_BV [,-c(2:21)]
MELT_S <-melt(MELT_S [,1:10], id.vars = c("ISIN.code"))
MELT_S $variable <- as.numeric(substr(MELT_S $variable,3,6)) 
colnames(MELT_S)[3]<-"S"
colnames(MELT_S)[2]<-"year"
View(MELT_S)

#Profit margin
MELT_PM <- Data_Orbis_BV [,-c(2:31)]
MELT_PM <-melt(MELT_PM [,1:10], id.vars = c("ISIN.code"))
MELT_PM $variable <- as.numeric(substr(MELT_PM $variable,4,7)) 
colnames(MELT_PM)[3]<-"PM"
colnames(MELT_PM)[2]<-"year"
View(MELT_PM)

#Current ratio 

MELT_CR <- Data_Orbis_BV [,-c(2:41)]
MELT_CR <-melt(MELT_CR [,1:10], id.vars = c("ISIN.code"))
MELT_CR $variable <- as.numeric(substr(MELT_CR $variable,4,7)) 

colnames(MELT_CR)[3]<-"CR"
colnames(MELT_CR)[2]<-"year"
View(MELT_CR)

#Cost of goods sold

MELT_CGS <- Data_Orbis_BV [,-c(2:51)]
MELT_CGS <-melt(MELT_CGS [,1:10], id.vars = c("ISIN.code"))
MELT_CGS $variable <- as.numeric(substr(MELT_CGS $variable,5,8)) 

colnames(MELT_CGS)[3]<-"CGS"
colnames(MELT_CGS)[2]<-"year"
View(MELT_CGS)
#Employees

MELT_COE <- Data_Orbis_BV [,-c(2:61)]
MELT_COE <-melt(MELT_COE [,1:10], id.vars = c("ISIN.code"))
MELT_COE $variable <- as.numeric(substr(MELT_COE $variable,5,8)) 

colnames(MELT_COE)[3]<-"COE"
colnames(MELT_COE)[2]<-"year"
View(MELT_COE)

#Raw Materials 
MELT_RM <- Data_Orbis_BV [,-c(2:71)]
MELT_RM <-melt(MELT_RM [,1:10], id.vars = c("ISIN.code"))
MELT_RM $variable <- as.numeric(substr(MELT_RM $variable,4,7)) 

colnames(MELT_RM)[3]<-"RM"
colnames(MELT_RM)[2]<-"year"
View(MELT_RM)

#Solvency ratio
MELT_SR <- Data_Orbis_BV [,-c(2:81)]
MELT_SR <-melt(MELT_SR [,1:10], id.vars = c("ISIN.code"))
MELT_SR $variable <- as.numeric(substr(MELT_SR $variable,4,7)) 

colnames(MELT_SR)[3]<-"SR"
colnames(MELT_SR)[2]<-"year"
View(MELT_SR)
#Research and Development 
MELT_RD <- Data_Orbis_BV [,-c(2:91)]
MELT_RD <-melt(MELT_RD [,1:10], id.vars = c("ISIN.code"))
MELT_RD $variable <- as.numeric(substr(MELT_RD $variable,4,7)) 

colnames(MELT_RD)[3]<-"RD"
colnames(MELT_RD)[2]<-"year"
View(MELT_RD)

#Total Assets 
MELT_TA <- Data_Orbis_BV [,-c(2:101)]
MELT_TA <-melt(MELT_TA [,1:10], id.vars = c("ISIN.code"))
MELT_TA $variable <- as.numeric(substr(MELT_TA $variable,4,7)) 

colnames(MELT_TA)[3]<-"TA"
colnames(MELT_TA)[2]<-"year"
View(MELT_TA)

#leverage
MELT_L <- Data_Orbis_BV [,-c(2:111)]
MELT_L <-melt(MELT_L [,1:10], id.vars = c("ISIN.code"))
MELT_L $variable <- as.numeric(substr(MELT_L $variable,3,6)) 

colnames(MELT_L)[3]<-"L"
colnames(MELT_L)[2]<-"year"
View(MELT_L)

#Financial expenses 
MELT_FE <- Data_Orbis_BV [,-c(2:121)]
MELT_FE <-melt(MELT_FE [,1:10], id.vars = c("ISIN.code"))
MELT_FE $variable <- as.numeric(substr(MELT_FE $variable,4,7)) 

colnames(MELT_FE)[3]<-"FE"
colnames(MELT_FE)[2]<-"year"
View(MELT_FE)


#Mergin---------------------------------------------------------------
#step1
Merge1 <- merge(MELT_O_ESG,MELT_E_ESG, all = TRUE)
Merge2 <- merge(MELT_S_ESG,MELT_G_ESG, all = TRUE)
Merge3 <- merge(MELT_EBITDA,MELT_EBITDA_M, all = TRUE)
Merge4 <- merge(MELT_S,MELT_PM, all = TRUE)
Merge5 <- merge(MELT_CR,MELT_CGS, all = TRUE)
Merge6 <- merge(MELT_RD,MELT_RM, all = TRUE)
Merge7 <- merge(MELT_SR,MELT_TA, all = TRUE)
Merge8 <- merge(Merge7,MELT_L, all = TRUE)
Merge9 <- merge(Merge8,MELT_FE, all = TRUE)


MELT_RM
#Step 2

Merge12 <- merge(Merge1,Merge2, all = TRUE)
Merge34 <- merge(Merge3,Merge4, all = TRUE)
Merge56 <- merge(Merge5,Merge6, all = TRUE)
Merge567 <- merge(Merge56,Merge7, all = TRUE)
Merge5678 <- merge(Merge567,Merge8, all = TRUE)
Merge56789 <- merge(Merge5678,Merge9, all = TRUE)


#Step 3

Merge3456789 <- merge(Merge34,Merge56789, all = TRUE)
MergeFINAL <- merge(Merge12,Merge3456789)

#MergeFINAL $CGS <-MergeFINAL $CGS * -1
#MergeFINAL $D <-MergeFINAL $D * -1
view(MergeFINAL)

#Complete data --- Only cells with values in it - only companies with all values available. 
final_complete_all <-MergeFINAL[complete.cases(MergeFINAL[,c("S","FE","PM","ESG_G","CGS","ESG_S","ESG_E","ESG_O","SR","CR","RD","TA","ISIN.code","EBITDA","EBITDA_M","L","year","RM")]),c("S","FE","PM","ESG_G","CGS","ESG_S","ESG_E","ESG_O","SR","CR","RD","TA","ISIN.code","EBITDA","EBITDA_M","L","year","RM")]
#final_complete_all$COE<-as.numeric (final_complete_all$COE)
final_complete_all$CGS<-as.numeric (final_complete_all$CGS)
final_complete_all$RD<-as.numeric (final_complete_all$RD)
final_complete_all$TA<-as.numeric (final_complete_all$TA)
final_complete_all$EBITDA<-as.numeric (final_complete_all$EBITDA)
final_complete_all$RM<-as.numeric (final_complete_all$RM)
final_complete_all$S<-as.numeric (final_complete_all$S)

view(final_complete_all)
summary(final_complete_all)
str(final_complete_all)


#data should be ready for the model- for some reason it is not working - ask Tobias
#Model ---------------------------------------------------------

#Model EBITDA Margin mean - using feols. 

MODEL_G_EBITDA_M <- feols(EBITDA_M ~ ESG_G + CGS + CR + RD + SR + L + RM + TA + FE + I(CGS*CGS) + I(RD*CGS) + I(RM*CGS) + I(FE*CGS) + I(RM*RM) + I(RM*RD) + I(RM*FE) + I(RD*RD) + I(FE*RD) + I(FE*FE) |year+ISIN.code, cluster = c("year", "ISIN.code"), data = final_complete_all)
summary (MODEL_G_EBITDA_M)
MODEL_S_EBITDA_M <- feols(EBITDA_M ~ ESG_S + CGS + CR + RD + SR + L + RM + TA + FE + I(CGS*CGS) + I(RD*CGS) + I(RM*CGS) + I(FE*CGS) + I(RM*RM) + I(RM*RD) + I(RM*FE) + I(RD*RD) + I(FE*RD) + I(FE*FE) |year+ISIN.code, cluster = c("year", "ISIN.code"), data = final_complete_all)
summary (MODEL_S_EBITDA_M)
MODEL_E_EBITDA_M <- feols(EBITDA_M ~ ESG_E + CGS + CR + RD + SR + L + RM + TA + FE + I(CGS*CGS) + I(RD*CGS) + I(RM*CGS) + I(FE*CGS) + I(RM*RM) + I(RM*RD) + I(RM*FE) + I(RD*RD) + I(FE*RD) + I(FE*FE) |year+ISIN.code, cluster = c("year", "ISIN.code"), data = final_complete_all)
summary (MODEL_E_EBITDA_M)
MODEL_O_EBITDA_M <- feols(EBITDA_M ~ ESG_O + CGS + CR + RD + SR + L + RM + TA + FE + I(CGS*CGS) + I(RD*CGS) + I(RM*CGS) + I(FE*CGS) + I(RM*RM) + I(RM*RD) + I(RM*FE) + I(RD*RD) + I(FE*RD) + I(FE*FE) |year+ISIN.code, cluster = c("year", "ISIN.code"), data = final_complete_all)
summary (MODEL_O_EBITDA_M)

#marginal effect 
marginaleffects(MODEL_G_EBITDA_M, newdata=datagrid(), vcov=T)
summary(marginaleffects(MODEL_G_EBITDA_M))

marginaleffects(MODEL_S_EBITDA_M, newdata=datagrid(), vcov=T)
summary(marginaleffects(MODEL_S_EBITDA_M))

marginaleffects(MODEL_E_EBITDA_M, newdata=datagrid(), vcov=T)
summary(marginaleffects(MODEL_E_EBITDA_M))

marginaleffects(MODEL_O_EBITDA_M, newdata=datagrid(), vcov=T)
summary(marginaleffects(MODEL_O_EBITDA_M))


#semivarience --------------------------------------
#residuals **** did not work ******
final_complete_all$residual_G <- resid(MODEL_G_EBITDA_M)
View(final_complete_all)

final_complete_all$residual_E <- resid(MODEL_E_EBITDA_M)
View(final_complete_all)

final_complete_all$residual_S <- resid(MODEL_S_EBITDA_M)
View(final_complete_all)

final_complete_all$residual_O <- resid(MODEL_O_EBITDA_M)
View(final_complete_all)


#squared residuals

final_complete_all$squaredresidual_G <- resid(MODEL_G_EBITDA_M)*resid(MODEL_G_EBITDA_M)
View(final_complete_all)

final_complete_all$squaredresidual_E <- resid(MODEL_E_EBITDA_M)*resid(MODEL_E_EBITDA_M)
View(final_complete_all)

final_complete_all$squaredresidual_S <- resid(MODEL_S_EBITDA_M)*resid(MODEL_S_EBITDA_M)
View(final_complete_all)

final_complete_all$squaredresidual_O <- resid(MODEL_O_EBITDA_M)*resid(MODEL_O_EBITDA_M)
View(final_complete_all)


#model for Semi-varience of EBITDA margins

MODEL_G_SV <- feols(squaredresidual_G ~ ESG_G + CGS + CR + RD + SR + L + RM + TA + FE + I(CGS*CGS) + I(RD*CGS) + I(RM*CGS) + I(FE*CGS) + I(RM*RM) + I(RM*RD) + I(RM*FE) + I(RD*RD) + I(FE*RD) + I(FE*FE) |year+ISIN.code, cluster = c("year", "ISIN.code"), data = final_complete_all[which(final_complete_all$residual_G<0),])
summary (MODEL_G_SV)
MODEL_S_SV <- feols(squaredresidual_S ~ ESG_S + CGS + CR + RD + SR + L + RM + TA + FE + I(CGS*CGS) + I(RD*CGS) + I(RM*CGS) + I(FE*CGS) + I(RM*RM) + I(RM*RD) + I(RM*FE) + I(RD*RD) + I(FE*RD) + I(FE*FE) |year+ISIN.code, cluster = c("year", "ISIN.code"), data = final_complete_all[which(final_complete_all$residual_G<0),])
summary (MODEL_S_SV)
MODEL_E_SV <- feols(squaredresidual_E ~ ESG_E + CGS + CR + RD + SR + L + RM + TA + FE + I(CGS*CGS) + I(RD*CGS) + I(RM*CGS) + I(FE*CGS) + I(RM*RM) + I(RM*RD) + I(RM*FE) + I(RD*RD) + I(FE*RD) + I(FE*FE) |year+ISIN.code, cluster = c("year", "ISIN.code"), data = final_complete_all[which(final_complete_all$residual_G<0),])
summary (MODEL_E_SV)
MODEL_O_SV <- feols(squaredresidual_O ~ ESG_O + CGS + CR + RD + SR + L + RM + TA + FE + I(CGS*CGS) + I(RD*CGS) + I(RM*CGS) + I(FE*CGS) + I(RM*RM) + I(RM*RD) + I(RM*FE) + I(RD*RD) + I(FE*RD) + I(FE*FE) |year+ISIN.code, cluster = c("year", "ISIN.code"), data = final_complete_all[which(final_complete_all$residual_G<0),])
summary (MODEL_O_SV)

#Marginal effects SV 
marginaleffects(MODEL_G_SV, newdata=datagrid(), vcov=T)
summary(marginaleffects(MODEL_G_SV))

marginaleffects(MODEL_S_SV, newdata=datagrid(), vcov=T)
summary(marginaleffects(MODEL_S_SV))

marginaleffects(MODEL_E_SV, newdata=datagrid(), vcov=T)
summary(marginaleffects(MODEL_E_SV))

marginaleffects(MODEL_O_SV, newdata=datagrid(), vcov=T)
summary(marginaleffects(MODEL_O_SV))


#Robustness check-Sales 

MODEL_G_S <- feols(S ~ ESG_G + CGS + CR + RD + SR + L + RM + TA + FE + I(CGS*CGS) + I(RD*CGS) + I(RM*CGS) + I(FE*CGS) + I(RM*RM) + I(RM*RD) + I(RM*FE) + I(RD*RD) + I(FE*RD) + I(FE*FE) |year+ISIN.code, cluster = c("year", "ISIN.code"), data = final_complete_all)
summary (MODEL_G_S)
MODEL_S_S <- feols(S ~ ESG_S + CGS + CR + RD + SR + L + RM + TA + FE + I(CGS*CGS) + I(RD*CGS) + I(RM*CGS) + I(FE*CGS) + I(RM*RM) + I(RM*RD) + I(RM*FE) + I(RD*RD) + I(FE*RD) + I(FE*FE) |year+ISIN.code, cluster = c("year", "ISIN.code"), data = final_complete_all)
summary (MODEL_S_S)
MODEL_E_S <- feols(S ~ ESG_E + CGS + CR + RD + SR + L + RM + TA + FE + I(CGS*CGS) + I(RD*CGS) + I(RM*CGS) + I(FE*CGS) + I(RM*RM) + I(RM*RD) + I(RM*FE) + I(RD*RD) + I(FE*RD) + I(FE*FE) |year+ISIN.code, cluster = c("year", "ISIN.code"), data = final_complete_all)
summary (MODEL_E_S)
MODEL_O_S <- feols(S ~ ESG_O + CGS + CR + RD + SR + L + RM + TA + FE + I(CGS*CGS) + I(RD*CGS) + I(RM*CGS) + I(FE*CGS) + I(RM*RM) + I(RM*RD) + I(RM*FE) + I(RD*RD) + I(FE*RD) + I(FE*FE) |year+ISIN.code, cluster = c("year", "ISIN.code"), data = final_complete_all)
summary (MODEL_O_S)



#semivarienece for ROE rubustness check 
#residuals 
final_complete_all$residual_G_R <- resid(MODEL_G_S)
View(final_complete_all)

final_complete_all$residual_S_R <- resid(MODEL_S_S)
View(final_complete_all)

final_complete_all$residual_E_R <- resid(MODEL_E_S)
View(final_complete_all)

final_complete_all$residual_O_R <- resid(MODEL_O_S)
View(final_complete_all)
# squared residuals ROE

final_complete_all$squaredresidual_G_R <- resid(MODEL_G_S)*resid(MODEL_G_S)
View(final_complete_all)

final_complete_all$squaredresidual_E_R <- resid(MODEL_E_S)*resid(MODEL_E_S)
View(final_complete_all)

final_complete_all$squaredresidual_S_R <- resid(MODEL_S_S)*resid(MODEL_S_S)
View(final_complete_all)

final_complete_all$squaredresidual_O_R <- resid(MODEL_O_S)*resid(MODEL_O_S)
View(final_complete_all)

#SV of Sales 
MODEL_G_SVR <- feols(squaredresidual_G_R ~ ESG_G + CGS + CR + RD + SR + L + RM + TA + FE + I(CGS*CGS) + I(RD*CGS) + I(RM*CGS) + I(FE*CGS) + I(RM*RM) + I(RM*RD) + I(RM*FE) + I(RD*RD) + I(FE*RD) + I(FE*FE) |year+ISIN.code, cluster = c("year", "ISIN.code"), data = final_complete_all)
summary (MODEL_G_SVR)
MODEL_S_SVR <- feols(squaredresidual_S_R ~ ESG_G + CGS + CR + RD + SR + L + RM + TA + FE + I(CGS*CGS) + I(RD*CGS) + I(RM*CGS) + I(FE*CGS) + I(RM*RM) + I(RM*RD) + I(RM*FE) + I(RD*RD) + I(FE*RD) + I(FE*FE) |year+ISIN.code, cluster = c("year", "ISIN.code"), data = final_complete_all)
summary (MODEL_S_SVR)
MODEL_E_SVR <- feols(squaredresidual_E_R ~ ESG_G + CGS + CR + RD + SR + L + RM + TA + FE + I(CGS*CGS) + I(RD*CGS) + I(RM*CGS) + I(FE*CGS) + I(RM*RM) + I(RM*RD) + I(RM*FE) + I(RD*RD) + I(FE*RD) + I(FE*FE) |year+ISIN.code, cluster = c("year", "ISIN.code"), data = final_complete_all)
summary (MODEL_E_SVR)
MODEL_O_SVR <- feols(squaredresidual_O_R ~ ESG_G + CGS + CR + RD + SR + L + RM + TA + FE + I(CGS*CGS) + I(RD*CGS) + I(RM*CGS) + I(FE*CGS) + I(RM*RM) + I(RM*RD) + I(RM*FE) + I(RD*RD) + I(FE*RD) + I(FE*FE) |year+ISIN.code, cluster = c("year", "ISIN.code"), data = final_complete_all)
summary (MODEL_O_SVR)

#Marginal effects Sales 
marginaleffects(MODEL_G_S, newdata=datagrid(), vcov=T)
summary(marginaleffects(MODEL_G_S))

marginaleffects(MODEL_S_S, newdata=datagrid(), vcov=T)
summary(marginaleffects(MODEL_S_S))

marginaleffects(MODEL_E_S, newdata=datagrid(), vcov=T)
summary(marginaleffects(MODEL_E_S))

marginaleffects(MODEL_O_S, newdata=datagrid(), vcov=T)
summary(marginaleffects(MODEL_O_S))

#Marginal effects Semivarience - Sales 

marginaleffects(MODEL_G_SVR, newdata=datagrid(), vcov=T)
summary(marginaleffects(MODEL_G_SVR))

marginaleffects(MODEL_S_SVR, newdata=datagrid(), vcov=T)
summary(marginaleffects(MODEL_S_SVR))

marginaleffects(MODEL_E_SVR, newdata=datagrid(), vcov=T)
summary(marginaleffects(MODEL_E_SVR))

marginaleffects(MODEL_O_SV_R, newdata=datagrid(), vcov=T)
summary(marginaleffects(MODEL_O_SVR))

#summary statistics
#ESG
summary(final_complete_all$ESG_G)
summary(final_complete_all$ESG_S)
summary(final_complete_all$ESG_E)
summary(final_complete_all$ESG_O)
#financials 
summary(final_complete_all$PM)
summary(final_complete_all$EBITDA)
summary(final_complete_all$EBITDA_M)
summary(final_complete_all$L)
summary(final_complete_all$SR)
summary(final_complete_all$CR)
summary(final_complete_all$FE)
summary(final_complete_all$RD)
summary(final_complete_all$RM)
summary(final_complete_all$S)
#SD 
sd(final_complete_all$ESG_G)
sd(final_complete_all$ESG_S)
sd(final_complete_all$ESG_E)
sd(final_complete_all$ESG_O)
#financials 
sd(final_complete_all$PM)
sd(final_complete_all$EBITDA_M)
sd(final_complete_all$SR)
sd(final_complete_all$CR)
sd(final_complete_all$L)
sd(final_complete_all$RM)
sd(final_complete_all$S)
sd(final_complete_all$FE)
sd(final_complete_all$RD)


