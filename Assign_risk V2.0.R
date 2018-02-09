
#Install and load Required Packages
list.of.packages <- c("stringr"
                      ,"magrittr"
                      ,"reshape2"
                      ,"lubridate"
                      ,"dplyr"
                      ,"RODBC"
                      ,"data.table")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages,require, character.only = TRUE)
setwd("F:/Fire Prevention/FDTN/Risk Profiles")
FULL_FD_KNIME<-fread("F:/Fire Prevention/FDTN/Risk Profiles/Full_Output.csv")

#assign a flag to each block considered high risk.
FULL_FD_KNIME<-FULL_FD_KNIME%>%mutate(flag=(ifelse(RiskScore>=25,"HighRisk",0)))

FULL_FD_KNIME<-FULL_FD_KNIME%>%mutate(flag=case_when(RiskScore>=25 ~ "HighRisk",
                                                     RiskScore>=15 & RiskScore<25 ~ "MediumRisk",
                                                     RiskScore<15 ~  "LowRisk"))
#pad zeroes for FDIDs.
FULL_FD_KNIME$FDID<-formatC(FULL_FD_KNIME$FDID, width = 5, format = "d", flag = "0")

#group by FDID and risk flag and sum population for each.
Risk_Flag<-FULL_FD_KNIME%>%
  group_by(NAME,FDID,flag)%>%
  summarise(n=sum(TOTALPOP))%>%
#add percentages of population w/ and without flag. 
  mutate(x=(n/sum(n))*100)


#pivot the flag status (x).
Risk_Flag<-dcast(Risk_Flag,NAME+FDID +n ~ flag)

#substutute NA values with zeroes. 
Risk_Flag[is.na(Risk_Flag)|Risk_Flag==""]<-0

#add up risk percentages (this is to reduce rows, summation should always be value + 0)
Risk_Flag<-Risk_Flag%>%group_by(FDID, NAME)%>%summarise(Population=sum(n)
                                                        ,PercHighRisk=sum(HighRisk)
                                                        ,PercMediumRisk=sum(MediumRisk)
                                                        ,PercLowRisk=sum(LowRisk))%>%
#add new columns indicating number of population for each risk category.
  mutate(HighRisk=Population*(PercHighRisk/100)
         ,MediumRisk=Population*(PercMediumRisk/100)
         ,LowRisk=Population*(PercLowRisk/100))

#bin by quantile 10, every fire department according to the percentage of their population at risk. 
Risk_Flag<-Risk_Flag%>%ungroup()%>%mutate(Risk_Bin=ntile(PercHighRisk,10))

#Calculate FDs risk population vs TN.
Risk_Flag<-Risk_Flag%>%mutate(PercOfWhole=(HighRisk/sum(HighRisk))*100)

#bin by quantile 10, every fire department according to proportion of TN pop at risk. 
Risk_Flag<-Risk_Flag%>%mutate(Pop_At_Risk_Bin=ntile(PercOfWhole,10))%>%arrange(desc(PercOfWhole))

#created a weighted risk score that weighs population @ risk twice as much as risk itself. 
Risk_Flag<-Risk_Flag%>%mutate(Wt_Risk=(Pop_At_Risk_Bin)*2 + Risk_Bin)

#bin by quantile 10, every fire department according to weighted risk score. 
Risk_Flag<-Risk_Flag%>%mutate(Wt_Risk_Bin=ntile(Wt_Risk,10))%>%arrange(desc(PercOfWhole))


Risk_Flag<-dplyr::rename(Risk_Flag,PERC_FD_AT_HIGH_RISK='PercHighRisk'
                                  ,PERC_FD_AT_MEDIUM_RISK='PercMediumRisk'
                                  ,PERC_FD_AT_LOW_RISK='PercLowRisk'
                                  ,FD_POPULATION='Population'
                                  ,NUM_FD_AT_HIGH_RISK='HighRisk'
                                  ,NUM_FD_AT_MEDIUM_RISK='MediumRisk'
                                  ,NUM_FD_AT_LOW_RISK='LowRisk'
                   , SCORE_BY_NUM_FD_AT_HIGH_RISK = 'Risk_Bin'
                   , PROP_OF_RISK_POP_TO_TN='PercOfWhole'
                   , SCORE_BY_PROP_OF_HighRisk_TO_TN='Pop_At_Risk_Bin'
                   , FD_WIGHTED_RISK = 'Wt_Risk'
                   , SCORE_FD_WEIGHTED_RISK = 'Wt_Risk_Bin')


Risk_Flag<-Risk_Flag[Risk_Flag$NAME!="0",]

View(Risk_Flag)

write.csv(Risk_Flag, "FD_Risk.csv", row.names = FALSE)




####V2.0 Starts Here####

setwd("F:/Fire Prevention/FDTN/Risk Profiles")

RISKDEMO<-fread("FD_Risk_Demo.csv")

colnames(RISKDEMO)[colnames(RISKDEMO)=="FDID_RISK_SCORE(3-30)"]<-"RISK_DEMO_SCORE"

RISKDEMO<-RISKDEMO%>%mutate(NUM_FD_MEDHIGH_WEIGHTED=NUM_FD_AT_HIGH_RISK+(NUM_FD_AT_MEDIUM_RISK/3))

RISKDEMO<-RISKDEMO%>%mutate(NUM_FD_MEDHIGH_WEIGHTED_BINNED=ntile(NUM_FD_MEDHIGH_WEIGHTED,100))

RISKDEMO<-RISKDEMO%>%mutate(SCORE_FDID_RISK_SCORE = ntile(RISKDEMO$RISK_DEMO_SCORE,100))

RISKDEMO<-RISKDEMO%>%mutate(RAW_RISK = (NUM_FD_MEDHIGH_WEIGHTED_BINNED*3)+SCORE_FDID_RISK_SCORE)

RISKDEMO<-RISKDEMO%>%mutate(RISK = ntile(RAW_RISK,7))%>%arrange(desc(RAW_RISK))

RISKDEMO<-RISKDEMO%>%select(NAME, FDID, FD_POPULATION, RISK_DEMO_SCORE, NUM_FD_AT_HIGH_RISK,
                            NUM_FD_AT_MEDIUM_RISK,RAW_RISK, RISK)

write.csv(RISKDEMO, "F:/RISKDEMO_1.3.18.csv")


####V3.0 Starts Here####

qtiles<-c(0, .1, .2, .3, .4, .5, .6, .7, .8, .9, .925, .95, .975,.99, 1)

setwd("F:/Fire Prevention/FDTN/Risk Profiles")

RISKDEMO<-fread("FD_Risk_Demo.csv")

RISKDEMO<-RISKDEMO[!is.na(RISKDEMO$`FDID_RISK_SCORE(3-30)`),]

BINS<-as.integer(cut(RISKDEMO$`FDID_RISK_SCORE(3-30)`, quantile(RISKDEMO$`FDID_RISK_SCORE(3-30)`, qtiles)))
colnames(BINS)<-BINS
RISKBINNED<-RISKDEMO%>%select(NAME, `FDID_RISK_SCORE(3-30)`, FD_POPULATION)%>%
  bind_cols(BINS=BINS)%>%
  arrange(desc(BINS))


table(BINS$BINS)


