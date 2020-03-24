rm(list=ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(reshape2)
library(zoo)

#covid data comes from Johns Hopkins University; sourced from Kaggle
covid <- read.csv("covid_19_data.csv",stringsAsFactors = FALSE) 
covid$ObservationDate=as.Date(covid$ObservationDate,format="%m/%d/%Y")

colors=rainbow(15, alpha = 1, rev = FALSE)

covid=as.data.frame(group_by(covid,ObservationDate,Province.State,Country.Region,Last.Update) %>% summarise(Confirmed = sum(Confirmed),Deaths=sum(Deaths),Recovered=sum(Recovered)))
covid=merge(covid,as.data.frame(group_by(covid, Province.State) %>% summarise(start=min(ObservationDate))),by=c("Province.State"),all=FALSE) #add date of first case
covid=merge(covid,as.data.frame(group_by(covid, Province.State) %>% filter(Confirmed>=100) %>% summarise(start.c.100=min(ObservationDate))),by="Province.State",all=TRUE) # add date of 100th case
covid=merge(covid,as.data.frame(group_by(covid, Province.State) %>% filter(Deaths>=10) %>% summarise(start.d.10=min(ObservationDate))),by="Province.State",all=TRUE) #add date of 10th death

covid$since_start=covid$ObservationDate-covid$start
covid$since_case_100=covid$ObservationDate-covid$start.c.100
covid$since_death_10=covid$ObservationDate-covid$start.d.10

covid$ObservationDate=as.Date(covid$ObservationDate)
#top countries are those with over 50 cases
covid.top=subset(covid,Confirmed>50)
top.countries=unique(covid.top$Province.State)
covid=arrange(covid,ObservationDate)


#aggregate across states/cities for each country
us.all=subset(covid,Country.Region=="US")
us=as.data.frame(group_by(us.all,ObservationDate, Country.Region) %>% summarise(confirmed = sum(Confirmed),deaths=sum(Deaths),Recovered=sum(Recovered)))
us$ObservationDate=as.Date(us$ObservationDate)

covid.agg=as.data.frame(group_by(covid,ObservationDate, Country.Region) %>% summarise(confirmed = sum(Confirmed),deaths=sum(Deaths),Recovered=sum(Recovered)))

covid.agg=merge(covid.agg,as.data.frame(group_by(covid.agg, Country.Region) %>% summarise(start=min(ObservationDate))),by=c("Country.Region"),all=TRUE) #add date of first case
covid.agg=merge(covid.agg,as.data.frame(group_by(covid.agg, Country.Region) %>% filter(confirmed>=100) %>% summarise(start.c.100=min(ObservationDate))),by="Country.Region",all=TRUE) # add date of 100th case
covid.agg=merge(covid.agg,as.data.frame(group_by(covid.agg, Country.Region) %>% filter(deaths>=10) %>% summarise(start.d.10=min(ObservationDate))),by="Country.Region",all=TRUE) #add date of 10th death

covid.agg$since_start=covid.agg$ObservationDate-covid.agg$start
covid.agg$since_case_100=covid.agg$ObservationDate-covid.agg$start.c.100
covid.agg$since_death_10=covid.agg$ObservationDate-covid.agg$start.d.10

covid.agg$ObservationDate=as.Date(covid.agg$ObservationDate)
#top countries are those with over 500 cases
covid.agg.top=subset(covid.agg,confirmed>50)
top.countries=unique(covid.agg.top$Country.Region)
covid.agg=arrange(covid.agg,ObservationDate)

#log
measures=c("confirmed","deaths","Recovered")
for (i in 1:3){
  covid.agg[,paste0(measures[i],".log")]=log(covid.agg[,measures[i]])
}

#plots

plot(x=covid.agg[covid.agg$Country.Region==top.countries[1] & covid.agg$since_case_100>(-1 ),"since_case_100"],y=log(covid.agg[covid.agg$Country.Region==top.countries[1] & covid.agg$since_case_100>(-1 ),"confirmed"]),type='l',xlab="Days since 100th case",ylab="log count",main="Confirmed COVID-19 Cases",ylim=c(min(covid.agg[covid.agg$Country.Region==top.countries[i] & covid.agg$since_case_100>(-1 ),"confirmed.log"]),12),xlim=c(-1,56))


for (i in (2:length(top.countries))){
  lines(x=covid.agg[covid.agg$Country.Region==top.countries[i] & covid.agg$since_case_100>(-1 ),"since_case_100"] ,y=covid.agg[covid.agg$Country.Region==top.countries[i] & covid.agg$since_case_100>(-1 ),"confirmed.log"],col=colors[i])
}


#deaths
plot(x=covid.agg[covid.agg$Country.Region==top.countries[1] & covid.agg$since_death_10>(-1 ),"since_death_10"],y=log(covid.agg[covid.agg$Country.Region==top.countries[1] & covid.agg$since_death_10>(-1 ),"deaths"]),type='l',xlab="Days since 10th death",ylab="log count",main="COVID-19 Deaths",ylim=c(2,8),xlim=c(-1,35))


for (i in (2:length(top.countries))){
  lines(x=covid.agg[covid.agg$Country.Region==top.countries[i] & covid.agg$since_death_10>(-1 ),"since_death_10"] ,y=covid.agg[covid.agg$Country.Region==top.countries[i] & covid.agg$since_death_10>(-1 ),"deaths.log"],col=colors[i])
}

covid.agg.confirmed=dcast(covid.agg,since_case_100~Country.Region,value.var="confirmed")
covid.agg.confirmed.log=dcast(covid.agg,since_case_100~Country.Region,value.var="confirmed.log")

colnames(covid.agg)=c("Country.Region","Date","Confirmed Cases","Deaths","Recovered Cases","start"      ,"start.c.100","start.d.10","Since 1st Case","Since 100th Case","Since 10th Death","confirmed.log","deaths.log","Recovered.log")


covid=as.data.frame(group_by(covid, Province.State) %>% mutate(case.max=max(Confirmed,na.rm=T))) # find max number of cases per country

colnames(covid)=c("Province.State","Date","Country.Region","Last.Update","Confirmed Cases","Deaths","Recovered Cases","start","start.c.100","start.d.10","Since 1st Case","Since 100th Case","Since 10th Death","Case_Max")
covid=subset(covid,Case_Max>=50) #limit states to those with at least 50 cases (~20th percentile)
covid$Case_Max=NULL

