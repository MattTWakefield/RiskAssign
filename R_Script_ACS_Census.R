library(acs)
my.tract=geo.make(state="TN", check=T)


State_Income=acs.fetch(geo=my.tract, table.number="B19001", endyear = 2013)
State_Income<-data.frame(estimate(State_Income))
State_Income<-melt(State_Income)
State_Value_IN<-sum(State_Income[c(2:9),"value"])
State_Whole_IN<-State_Income[c(1),"value"]
PERC_BELOW_IN<-State_Value_IN/State_Whole_IN*100

State_Education=acs.fetch(geo=my.tract, table.number="B15003", endyear = 2013)
State_Education<-data.frame(estimate(State_Education))
State_Education<-melt(State_Education)
State_Value_BD<-sum(State_Education[c(2:21),"value"])
State_Whole_BD<-State_Education[c(1),"value"]
PERC_BELOW_BD<-State_Value_BD/State_Whole_BD*100

State_Home_Val=acs.fetch(geo=my.tract, table.number="B25075", endyear = 2013)
State_Home_Val<-data.frame(estimate(State_Home_Val))
State_Home_Val<-melt(State_Home_Val)
State_Value_HV<-sum(State_Home_Val[c(2:15),"value"])
State_Whole_HV<-State_Home_Val[c(1),"value"]
PERC_BELOW_HV<-State_Value_HV/State_Whole_HV*100

State_Home_Age=acs.fetch(geo=my.tract, table.number="B25034", endyear = 2013)
State_Home_Age<-data.frame(estimate(State_Home_Age))
State_Home_Age<-melt(State_Home_Age)
State_Value_HA<-sum(State_Home_Age[c(6:10),"value"])
State_Whole_HA<-State_Home_Age[c(1),"value"]
PERC_BELOW_HA<-State_Value_HA/State_Whole_HA*100


State_Age=acs.fetch(geo=my.tract, table.number="B01001", endyear = 2013)
State_Age<-data.frame(estimate(State_Age))
State_Age<-melt(State_Age)
State_Value_AG<-sum(State_Age[c(45:49,20:25),"value"])
State_Whole_AG<-State_Age[c(1),"value"]
PERC_BELOW_AG<-State_Value_AG/State_Whole_AG*100

STATE_DEMO<-cbind(PERC_BELOW_AG, PERC_BELOW_HA, PERC_BELOW_HV, PERC_BELOW_BD, PERC_BELOW_IN)
STATE_DEMO<-melt(STATE_DEMO)
STATE_DEMO<-STATE_DEMO%>%select(c(2,3))
colnames(STATE_DEMO)<-c("Category","State_Value")






