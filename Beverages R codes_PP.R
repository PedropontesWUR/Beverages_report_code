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
#not used below
Data_Orbis$Profit.margin.....2012<-sub("n.s.", Data_Orbis$Profit.margin.....2012, NA)
str(Data_Orbis_BV)
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


#Mergin---------------------------------------------------------------
#step1
Merge1 <- merge(MELT_O_ESG,MELT_E_ESG, all = TRUE)
Merge2 <- merge(MELT_S_ESG,MELT_G_ESG, all = TRUE)
Merge3 <- merge(MELT_EBITDA,MELT_EBITDA_M, all = TRUE)
Merge4 <- merge(MELT_S,MELT_PM, all = TRUE)
Merge5 <- merge(MELT_CR,MELT_CGS, all = TRUE)
Merge6 <- merge(MELT_COE,MELT_RM, all = TRUE)
Merge7 <- merge(MELT_SR,MELT_RD, all = TRUE)
Merge8 <- merge(Merge7,MELT_TA, all = TRUE)
Merge9 <- merge(Merge8,MELT_L, all = TRUE) 

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
final_complete_all <-MergeFINAL[complete.cases(MergeFINAL[,c("PM","ESG_G","CGS","ESG_S","ESG_E","COE","ESG_O","SR","CR","RD","TA","ISIN.code","EBITDA","EBITDA_M","L","year","RM")]),c("PM","ESG_G","CGS","ESG_S","ESG_E","COE","ESG_O","SR","CR","RD","TA","ISIN.code","EBITDA","EBITDA_M","L","year","RM")]
#final_complete_all$COE<-as.numeric (final_complete_all$COE)
#final_complete_all$COE<-as.numeric (final_complete_all$CGS)
view(final_complete_all)
summary(final_complete_all)
str(final_complete_all)


#data should be ready for the model- for some reason it is not working - ask Tobias
#Model ---------------------------------------------------------

#Model Profit Margin mean - using feols. 

MODEL_G_PM <- feols(PM ~ ESG_G + COE + CGS + CR + RD + SR + L + RM + TA + I(COE*COE)+ I(CGS*CGS) + RD:COE + RD:CGS + CGS:COE + RM:COE + RM:CGS + RM:RD |year+ISIN.code, cluster = final_complete_all[,c("year", "ISIN.code")], data = final_complete_all)
summary (MODEL_G_PM)
MODEL_S_PM <- feols(PM ~ ESG_S + COE + CGS + CR + RD + SR + L + RM + TA + I(COE*COE)+ I(CGS*CGS) + RD:COE + RD:CGS + CGS:COE + RM:COE + RM:CGS + RM:RD |year+ISIN.code, cluster = final_complete_all[,c("year", "ISIN.code")], data = final_complete_all)
summary (MODEL_S_PM)
MODEL_E_PM <- feols(PM ~ ESG_E + COE + CGS + CR + RD + SR + L + RM + TA + I(COE*COE)+ I(CGS*CGS) + RD:COE + RD:CGS + CGS:COE + RM:COE + RM:CGS + RM:RD |year+ISIN.code, cluster = final_complete_all[,c("year", "ISIN.code")], data = final_complete_all)
summary (MODEL_E_PM)
MODEL_O_PM <- feols(PM ~ ESG_O + COE + CGS + CR + RD + SR + L + RM + TA + I(COE*COE)+ I(CGS*CGS) + RD:COE + RD:CGS + CGS:COE + RM:COE + RM:CGS + RM:RD |year+ISIN.code, cluster = final_complete_all[,c("year", "ISIN.code")], data = final_complete_all)
summary (MODEL_O_PM)

#marginal effect 
marginaleffects(MODEL_G_PM, newdata=datagrid(), vcov=T)
summary(marginaleffects(MODEL_G_PM))

marginaleffects(MODEL_S_PM, newdata=datagrid(), vcov=T)
summary(marginaleffects(MODEL_S_PM))

marginaleffects(MODEL_E_PM, newdata=datagrid(), vcov=T)
summary(marginaleffects(MODEL_E_PM))

marginaleffects(MODEL_O_PM, newdata=datagrid(), vcov=T)
summary(marginaleffects(MODEL_O_PM))


#semivarience --------------------------------------
#residuals **** did not work ******
final_complete_all$residual_G <- resid(MODEL_G_PM)
View(final_complete_all)

final_complete_all$residual_E <- resid(MODEL_E_PM)
View(final_complete_all)

final_complete_all$residual_S <- resid(MODEL_S_PM)
View(final_complete_all)

final_complete_all$residual_O <- resid(MODEL_O_PM)
View(final_complete_all)


#squared residuals

final_complete_all$squaredresidual_G <- resid(MODEL_G_PM)*resid(MODEL_G_PM)
View(final_complete_all)

final_complete_all$squaredresidual_E <- resid(MODEL_E_PM)*resid(MODEL_E_PM)
View(final_complete_all)

final_complete_all$squaredresidual_S <- resid(MODEL_S_PM)*resid(MODEL_S_PM)
View(final_complete_all)

final_complete_all$squaredresidual_O <- resid(MODEL_O_PM)*resid(MODEL_O_PM)
View(final_complete_all)


#model for Semi-varience.#### it worked on tuesday. gives me an error today. 

MODEL_G_SV <- feols(squaredresidual_G ~ ESG_G + COE + CGS + CR + RD + SR + L + RM + TA + I(COE*COE)+ I(CGS*CGS) + RD:COE + RD:CGS + CGS:COE + RM:COE + RM:CGS + RM:RD |ISIN.code+year, cluster = c("year", "ISIN.code"), data = final_complete_all[which(final_complete_all$residual_G<0),])
summary (MODEL_G_SV)
# i mistakenly deleted the code you gave me to fix the SV. Can i have it again? 
MODEL_S_SV <- feols(squaredresidual_S ~ ESG_S + COE + CGS + CR + RD + SR + L + RM + TA + I(COE*COE)+ I(CGS*CGS) + RD:COE + RD:CGS + CGS:COE + RM:COE + RM:CGS + RM:RD |ISIN.code+year, cluster = c("year", "ISIN.code"), data = final_complete_all[which(final_complete_all$residual_S<0),])
summary (MODEL_S_SV)
MODEL_E_SV <- feols(squaredresidual_E ~ ESG_E + COE + CGS + CR + RD + SR + L + RM + TA + I(COE*COE)+ I(CGS*CGS) + RD:COE + RD:CGS + CGS:COE + RM:COE + RM:CGS + RM:RD |ISIN.code+year, cluster = c("year", "ISIN.code"), data = final_complete_all[which(final_complete_all$residual_E<0),])
summary (MODEL_E_SV)
MODEL_O_SV <- feols(squaredresidual_O ~ ESG_O + COE + CGS + CR + RD + SR + L + RM + TA + I(COE*COE)+ I(CGS*CGS) + RD:COE + RD:CGS + CGS:COE + RM:COE + RM:CGS + RM:RD |ISIN.code+year, cluster = c("year", "ISIN.code"), data = final_complete_all[which(final_complete_all$residual_O<0),])
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


#Robustness check-EBITDA

MODEL_G_EBITDA <- feols(EBITDA ~ ESG_G + COE + CGS + CR + RD + SR + L + RM + TA + I(COE*COE)+ I(CGS*CGS) + RD:COE + RD:CGS + CGS:COE + RM:COE + RM:CGS + RM:RD |year+ISIN.code, cluster = final_complete_all[,c("year", "ISIN.code")], data = final_complete_all)
summary (MODEL_G_EBITDA)
MODEL_S_EBITDA <- feols(EBITDA ~ ESG_S + COE + CGS + CR + RD + SR + L + RM + TA + I(COE*COE)+ I(CGS*CGS) + RD:COE + RD:CGS + CGS:COE + RM:COE + RM:CGS + RM:RD |year+ISIN.code, cluster = final_complete_all[,c("year", "ISIN.code")], data = final_complete_all)
summary (MODEL_S_EBITDA)
MODEL_E_EBITDA <- feols(EBITDA ~ ESG_E + COE + CGS + CR + RD + SR + L + RM + TA + I(COE*COE)+ I(CGS*CGS) + RD:COE + RD:CGS + CGS:COE + RM:COE + RM:CGS + RM:RD |year+ISIN.code, cluster = final_complete_all[,c("year", "ISIN.code")], data = final_complete_all)
summary (MODEL_E_EBITDA)
MODEL_O_EBITDA <- feols(EBITDA ~ ESG_O + COE + CGS + CR + RD + SR + L + RM + TA + I(COE*COE)+ I(CGS*CGS) + RD:COE + RD:CGS + CGS:COE + RM:COE + RM:CGS + RM:RD |year+ISIN.code, cluster = final_complete_all[,c("year", "ISIN.code")], data = final_complete_all)
summary (MODEL_G_EBITDA)



#semivarienece for ROE rubustness check 
#residuas 
final_complete_all$residual_G_R <- resid(MODEL_G_EBITDA)
View(final_complete_all)

final_complete_all$residual_S_R <- resid(MODEL_S_EBITDA)
View(final_complete_all)

final_complete_all$residual_E_R <- resid(MODEL_E_EBITDA)
View(final_complete_all)

final_complete_all$residual_O_R <- resid(MODEL_O_EBITDA)
View(final_complete_all)
# squared residuals ROE

final_complete_all$squaredresidual_G_R <- resid(MODEL_G_EBITDA)*resid(MODEL_G_EBITDA)
View(final_complete_all)

final_complete_all$squaredresidual_E_R <- resid(MODEL_E_EBITDA)*resid(MODEL_E_EBITDA)
View(final_complete_all)

final_complete_all$squaredresidual_S_R <- resid(MODEL_S_EBITDA)*resid(MODEL_S_EBITDA)
View(final_complete_all)

final_complete_all$squaredresidual_O_R <- resid(MODEL_O_EBITDA)*resid(MODEL_O_EBITDA)
View(final_complete_all)

#SV ROE 
MODEL_G_SV_R <- feols(squaredresidual_G_R ~ ESG_G + COE + CGS + CR + RD + SR + L + RM + TA + I(COE*COE)+ I(CGS*CGS) + RD:COE + RD:CGS + CGS:COE + RM:COE + RM:CGS + RM:RD |ISIN.code+year, cluster = c("year", "ISIN.code"), data = final_complete_all[which(final_complete_all$residual_G<0),])
summary (MODEL_G_SV_R)
MODEL_S_SV_R <- feols(squaredresidual_S_R ~ ESG_S + COE + CGS + CR + RD + SR + L + RM + TA + I(COE*COE)+ I(CGS*CGS) + RD:COE + RD:CGS + CGS:COE + RM:COE + RM:CGS + RM:RD |ISIN.code+year, cluster = c("year", "ISIN.code"), data = final_complete_all[which(final_complete_all$residual_S<0),])
summary (MODEL_S_SV_R)
MODEL_E_SV_R <- feols(squaredresidual_E_R ~ ESG_E + COE + CGS + CR + RD + SR + L + RM + TA + I(COE*COE)+ I(CGS*CGS) + RD:COE + RD:CGS + CGS:COE + RM:COE + RM:CGS + RM:RD |ISIN.code+year, cluster = c("year", "ISIN.code"), data = final_complete_all[which(final_complete_all$residual_E<0),])
summary (MODEL_E_SV_R)
MODEL_O_SV_R <- feols(squaredresidual_O_R ~ ESG_O + COE + CGS + CR + RD + SR + L + RM + TA + I(COE*COE)+ I(CGS*CGS) + RD:COE + RD:CGS + CGS:COE + RM:COE + RM:CGS + RM:RD |ISIN.code+year, cluster = c("year", "ISIN.code"), data = final_complete_all[which(final_complete_all$residual_O<0),])
summary (MODEL_O_SV_R)

#Marginal effects Robustness check 
marginaleffects(MODEL_G_EBITDA, newdata=datagrid(), vcov=T)
summary(marginaleffects(MODEL_G_EBITDA))

marginaleffects(MODEL_S_EBITDA, newdata=datagrid(), vcov=T)
summary(marginaleffects(MODEL_s_EBITDA))

marginaleffects(MODEL_E_EBITDA, newdata=datagrid(), vcov=T)
summary(marginaleffects(MODEL_E_EBITDA))

marginaleffects(MODEL_O_EBITDA, newdata=datagrid(), vcov=T)
summary(marginaleffects(MODEL_O_EBITDA))

#Marginal effects ROE - SV

marginaleffects(MODEL_G_SV_R, newdata=datagrid(), vcov=T)
summary(marginaleffects(MODEL_G_SV_R))

marginaleffects(MODEL_S_SV_R, newdata=datagrid(), vcov=T)
summary(marginaleffects(MODEL_S_SV_R))

marginaleffects(MODEL_E_SV_R, newdata=datagrid(), vcov=T)
summary(marginaleffects(MODEL_E_SV_R))

marginaleffects(MODEL_O_SV_R, newdata=datagrid(), vcov=T)
summary(marginaleffects(MODEL_O_SV_R))

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

summary(final_complete_all$COE)
summary(final_complete_all$RD)
summary(final_complete_all$RM)

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

sd(final_complete_all$COE)
sd(final_complete_all$RD)

#**************************************************************************
#word cloud 
install.packages("wordcloud")
library(wordcloud)
install.packages("RColorBrewer")
library(RColorBrewer)
install.packages("tm")
library(tm)


write.table(MODEL_G_PM, 'MODEL_G_PM_.csv', row.names = FALSE)

#checking if model value chage if we dont take ESG scores into account. 
MODEL_PM_W <- feols(PM ~ CGS + COE + CR + RD + SR + A + CGS:COE + I(CGS*CGS) + I(COE*COE)+ RD:COE + RD:CGS |year+ISIN.code, cluster = final_complete_all[,c("year", "ISIN.code")], data = final_complete_all)
summary (MODEL_G_PM_W)
MODEL_SV_W <- feols(squaredresidual_W ~ CGS + COE + CR + RD + SR + A + CGS:COE + I(CGS*CGS) + I(COE*COE)+ RD:COE + RD:CGS|ISIN.code+year, cluster = c("year", "ISIN.code"), data = final_complete_all[which(final_complete_all$residual_G<0),])
summary (MODEL_SV_W)

marginaleffects(MODEL_PM_W, newdata=datagrid(), vcov=T)
summary(marginaleffects(MODEL_PM_W))

marginaleffects(MODEL_SV_W, newdata=datagrid(), vcov=T)
summary(marginaleffects(MODEL_SV_W))

#ESG summary analisys all ESG listed companies sample
summary(ESG_scores_G)
summary(ESG_scores_E)
summary(ESG_scores_S)
summary(ESG_scores_O)
#70 Aniaml protein/ dairy 

summary(MergeFINAL$ESG_G)
summary(MergeFINAL$ESG_S)
summary(MergeFINAL$ESG_E)
summary(MergeFINAL$ESG_O)
