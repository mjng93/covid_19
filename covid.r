rm(list=ls())
library(ggplot2)
library(tidyr)
library(dplyr)
library(RColorBrewer)
library(reshape2)
library(zoo)
library(gridExtra)
library(data.table)
library(lobstr)

print(mem_used()/1000000000)

Sys.setenv(TZ="America/Chicago")

today=Sys.Date()

#covid data comes directly from Johns Hopkins University github page

confirmed <-fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv",stringsAsFactors=F)
deaths = fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv",stringsAsFactors=F)
recovered = fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv",stringsAsFactors=F)

print("New Country-level JHU data loaded from Github")

confirmed=melt(subset(confirmed,select=-c(Lat,Long)),id.vars=c("Province/State","Country/Region"))
confirmed$variable=as.Date(confirmed$variable,format="%m/%d/%y")
colnames(confirmed)=c("Province/State","Country/Region","ObservationDate","Confirmed")

colnames(deaths)[c(1,2)]=c("Province/State","Country/Region")
deaths=melt(subset(deaths,select=-c(Lat,Long)),id.vars=c("Province/State","Country/Region"))
deaths$variable=as.Date(deaths$variable,format="%m/%d/%y")
colnames(deaths)=c("Province/State","Country/Region","ObservationDate","Deaths")

colnames(recovered)[c(1,2)]=c("Province/State","Country/Region")
recovered=melt(subset(recovered,select=-c(Lat,Long)),id.vars=c("Province/State","Country/Region"))
recovered$variable=as.Date(recovered$variable,format="%m/%d/%y")
colnames(recovered)=c("Province/State","Country/Region","ObservationDate","Recovered")

covid=merge(confirmed,deaths,by=c("Province/State","Country/Region","ObservationDate"),all=FALSE)
covid=merge(covid,recovered,by=c("Province/State","Country/Region","ObservationDate"),all=FALSE)
colnames(covid)=gsub("/","_",colnames(covid))

rm(confirmed)
rm(deaths)
rm(recovered)

covid$Country_Region=as.character(covid$Country_Region)
covid$Province_State=as.character(covid$Province_State)

#read in population data from the world bank
pop.match <- read.csv("world_bank_pop_match1.csv",stringsAsFactors = FALSE) 
pop <- read.csv("wb_population.csv",stringsAsFactors = FALSE) 
pop=merge(pop,pop.match,by="WB.Country.Region",all=FALSE)
colnames(pop)=gsub("\\.","_",colnames(pop))
covid=merge(covid,pop,by="Country_Region",all=FALSE)
covid$ObservationDate=as.Date(covid$ObservationDate,format="%m/%d/%Y")
covid=arrange(covid,ObservationDate)

#colors=rainbow(15, alpha = 1, rev = FALSE)

#aggregate across states/cities for each country
# us.all=subset(covid,Country_Region=="US")
# us=as.data.table(group_by(us.all,ObservationDate, Country_Region) %>% summarise(confirmed = sum(Confirmed),deaths=sum(Deaths),Recovered=sum(Recovered)))
# us$ObservationDate=as.Date(us$ObservationDate)

covid.agg=as.data.table(group_by(covid,ObservationDate, Country_Region) %>% summarise(confirmed = sum(Confirmed),deaths=sum(Deaths),Recovered=sum(Recovered),Population=max(population)))#aggregate by country/Region

covid.agg=as.data.table(group_by(covid.agg, Country_Region) %>% mutate(start=min(ObservationDate))) #add date of first case
covid.agg=merge(covid.agg,as.data.table(group_by(covid.agg, Country_Region) %>% filter(confirmed>=100) %>% summarise(start.c.100=min(ObservationDate))),by="Country_Region",all=TRUE) # add date of 100th case
covid.agg=merge(covid.agg,as.data.table(group_by(covid.agg, Country_Region) %>% filter(deaths>=10) %>% summarise(start.d.10=min(ObservationDate))),by="Country_Region",all=TRUE) #add date of 10th death
#covid.agg=as.data.frame(group_by(covid.agg, Country_Region) %>% mutate(confirmed.pc=confirmed/population,deaths.pc=deaths/population)) #per capita levels

# covid.agg=merge(covid.agg,as.data.frame(group_by(covid.agg, Country_Region) %>% summarise(start=min(ObservationDate))),by=c("Country_Region"),all=TRUE) #add date of first case

covid.agg$since_start=covid.agg$ObservationDate-covid.agg$start
covid.agg$since_case_100=covid.agg$ObservationDate-covid.agg$start.c.100
covid.agg$since_death_10=covid.agg$ObservationDate-covid.agg$start.d.10

covid.agg$ObservationDate=as.Date(covid.agg$ObservationDate)
covid.agg=arrange(covid.agg,ObservationDate)

#top countries are those in the top 10% on the most recent date
top.countries=as.vector(unique(subset(as.data.frame(covid.agg),confirmed>quantile(covid.agg$confirmed[covid.agg$ObservationDate==max(covid.agg$ObservationDate)],seq(0,1,.05))[19])[,c("Country_Region")]))


#top countries are those in the top 10% on the most recent date
# covid.top=subset(covid,Confirmed>quantile(covid$Confirmed[covid$ObservationDate==max(covid$ObservationDate)],seq(0,1,.05))[20])
# top.countries=unique(covid.top$Country_Region)


colnames(covid.agg)=c("Country_Region","Date","Confirmed Cases","Deaths","Recovered Cases","Population","start","start.c.100","start.d.10","Since 1st Case","Since 100th Case","Since 10th Death")

covid.agg.total=as.data.frame(group_by(covid.agg,Date) %>% summarise(`Confirmed Cases`=sum(`Confirmed Cases`,na.rm=T),Deaths=sum(Deaths,na.rm=T),`Recovered Cases`=sum(`Recovered Cases`,na.rm=T)))

print(mem_used()/1000000000)

#----------------#
#state level data#
#----------------#

print("getting state level data")

#read in base data from Kaggle
covid.kaggle <- fread("covid_19_data_base.csv",stringsAsFactors=F) 
colnames(covid.kaggle)=gsub("\\.","_",colnames(covid.kaggle))
covid.kaggle$ObservationDate=as.Date(covid.kaggle$ObservationDate)

covid.kaggle$Country_Region=gsub("Mainland China","China",covid.kaggle$Country_Region)
covid.kaggle$Country_Region=gsub("South Korea","Korea, South",covid.kaggle$Country_Region)
covid.kaggle$Country_Region=gsub("UK","United Kingdom",covid.kaggle$Country_Region)

#read in any new data from Hopkins Github page directly (state-level data uploaded seperately for each date - choose to download new data if today is a day or later than the most recent data we have stored) 

#There is a 1 day lag for when the data is uploaded by JHU. Data should be 1 day behind. If the data is 2 days behind or more, then download missing data

if ((today-max(covid.kaggle$ObservationDate,na.rm = T))>1){
  
  #if data is 3 days behind or more, download backlog of missing data
  if ((today-max(covid.kaggle$ObservationDate,na.rm = T))>2 ){
    
    print(paste0(paste0('There is a backlog of missing state-level data. Current data through '),max(covid.kaggle$ObservationDate,na.rm = T), ". Downloading backlog..."))
    
    assign(paste0("new_state_data"),fread(paste0(paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/",as.character(format(today-1,"%m-%d-%Y"))),".csv"))[,c("Province_State","Country_Region","Confirmed","Deaths","Recovered")] %>% mutate(ObservationDate=(as.Date(today-1))) )
    
    print(paste0('downloaded state data for yesterday, ', as.character(format(today-1,"%m-%d-%Y"))))
    
    for (i in (2:(today-max(covid.kaggle$ObservationDate,na.rm=T)-1))){
      
      assign(paste0("new_state_data_",i),fread(paste0(paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/",as.character(format(today-i,"%m-%d-%Y"))),".csv"))[,c("Province_State","Country_Region","Confirmed","Deaths","Recovered")] %>% mutate(ObservationDate=(as.Date(today-i))) )
      
      print(paste0('downloaded more state data for ', as.character(format(today-i,"%m-%d-%Y"))))
      new_state_data=rbind(new_state_data,get(paste0("new_state_data_",i)))
      
    }
   
    print(nrow(new_state_data))
    #new_state_data=new_state_data[!duplicated(new_state_data),]
    print(nrow(new_state_data))
    new_state_data[new_state_data$ObservationDate %in% covid.kaggle$Date,]=NA
  }
  
  #if there is only missing data from yesterday, download the data
  if ((today-max(covid.kaggle$ObservationDate,na.rm=T))==2){
    assign(paste0("new_state_data"),fread(paste0(paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/",as.character(format(today-1,"%m-%d-%Y"))),".csv")))
    print(paste0('Downloaded yesterdays state data for ', as.character(format(today-1,"%m-%d-%Y"))))
    new_state_data=new_state_data[,c("Province_State","Country_Region","Confirmed","Deaths","Recovered")]
    new_state_data$ObservationDate=as.Date(today-1)
    #new_state_data=rbind(new_state_data_yesterday,new_state_data)
    new_state_data[new_state_data$ObservationDate %in% covid.kaggle$Date,]=NA
    
  }
  
  colnames(new_state_data)=c("Province_State","Country_Region","Confirmed","Deaths","Recovered","ObservationDate")
  new_state_data$Province_State=as.character(new_state_data$Province_State)
  new_state_data$Country_Region=as.character(new_state_data$Country_Region)
  new_state_data=arrange(new_state_data,ObservationDate)
  print("downloaded new state-level data")

  covid.kaggle=merge(covid.kaggle,new_state_data,by=c("Province_State","Country_Region","ObservationDate","Confirmed","Deaths","Recovered"),all=TRUE)
  covid.kaggle=covid.kaggle[!duplicated(covid.kaggle),]
  
  covid.kaggle=as.data.table(group_by(covid.kaggle,ObservationDate,Province_State,Country_Region) %>% summarise(Confirmed = sum(Confirmed),Deaths=sum(Deaths),Recovered=sum(Recovered))) #add for instances in which there are many observations per date
  write.csv(covid.kaggle,"covid_19_data_base.csv",row.names = FALSE)
  
}

covid.kaggle=merge(covid.kaggle,pop,by="Country_Region",all=FALSE)
covid.kaggle=arrange(arrange(covid.kaggle,ObservationDate),Province_State)

covid.kaggle=as.data.table(group_by(covid.kaggle,ObservationDate,Province_State,Country_Region) %>% summarise(Confirmed = sum(Confirmed),Deaths=sum(Deaths),Recovered=sum(Recovered),population=max(population))) #aggregate
covid.kaggle=merge(covid.kaggle,as.data.table(group_by(covid.kaggle, Province_State) %>% summarise(start=min(ObservationDate))),by=c("Province_State"),all=FALSE) #add date of first case
covid.kaggle=merge(covid.kaggle,as.data.table(group_by(covid.kaggle, Province_State) %>% filter(Confirmed>=100) %>% summarise(start.c.100=min(ObservationDate))),by="Province_State",all=TRUE) # add date of 100th case
covid.kaggle=merge(covid.kaggle,as.data.table(group_by(covid.kaggle, Province_State) %>% filter(Deaths>=10) %>% summarise(start.d.10=min(ObservationDate))),by="Province_State",all=TRUE) #add date of 10th death

covid.kaggle=arrange(covid.kaggle,ObservationDate)

covid.kaggle$since_start=covid.kaggle$ObservationDate-covid.kaggle$start
covid.kaggle$since_case_100=covid.kaggle$ObservationDate-covid.kaggle$start.c.100
covid.kaggle$since_death_10=covid.kaggle$ObservationDate-covid.kaggle$start.d.10

covid.kaggle$ObservationDate=as.Date(covid.kaggle$ObservationDate)

covid.kaggle=as.data.table(group_by(covid.kaggle, Province_State) %>% mutate(case.max=max(Confirmed,na.rm=T))) # find max number of cases per country

colnames(covid.kaggle)=c("Province_State","Date","Country_Region","Confirmed Cases","Deaths","Recovered Cases","Population","start","start.c.100","start.d.10","Since 1st Case","Since 100th Case","Since 10th Death","Case_Max")
covid.kaggle=subset(covid.kaggle,Case_Max>=50) #limit states to those with at least 50 cases (~20th percentile)
covid.kaggle$Case_Max=NULL
state_pop=read.csv("state_pop_match.csv",stringsAsFactors = FALSE) 
colnames(state_pop)=gsub("\\.","_",colnames(state_pop))
covid.kaggle=merge(covid.kaggle,state_pop,by="Province_State",all.x=TRUE)
covid.kaggle=arrange(covid.kaggle,Date)

print(mem_used()/1000000000)

print('getting covid tracking project data')

#Add data on testing and hospitalizations from covidtracking.com and state population data from Census

covid.state=covid.kaggle[covid.kaggle$Country_Region=="US",]

# tests <- fread("states-daily.csv")
# tests=fread("https://covidtracking.com/api/states/daily.csv")
tests = fread("https://raw.githubusercontent.com/COVID19Tracking/covid-public-api/master/v1/states/daily.csv",stringsAsFactors = F)
tests$date=as.Date(as.character(tests$date),format="%Y%m%d")
colnames(tests)[c(1,2)]=c("Date","state_ab")
tests$state_ab=as.character(tests$state_ab)
colnames(tests)=gsub("death","deaths",colnames(tests))
colnames(tests)=gsub("hospitalized","cumulativeHospitalized",colnames(tests))

#us.tests=fread("https://covidtracking.com/api/us/daily.csv")
us.tests=fread("https://raw.githubusercontent.com/COVID19Tracking/covid-public-api/master/v1/us/daily.csv",stringsAsFactors = F)
print("Downloaded Testing data from Covid Tracking Project")
us.tests$date=as.Date(as.character(us.tests$date),format="%Y%m%d")
colnames(us.tests)[c(1)]=c("Date")
us.tests$state_ab="US (Total)"
colnames(us.tests)=gsub("death","deaths",colnames(us.tests))
colnames(us.tests)=gsub("hospitalized","cumulativeHospitalized",colnames(us.tests))
tests=merge(tests,us.tests[,c("Date","state_ab","positive","negative","pending","cumulativeHospitalized","deaths","total","totalTestResults")],by=c("Date","state_ab","positive","negative","pending","cumulativeHospitalized","deaths","total","totalTestResults"),all=TRUE)
tests$state_ab=as.character(tests$state_ab)

covid.state=merge(covid.state,tests[,c("Date","state_ab","positive","negative","pending","cumulativeHospitalized","deaths","total","totalTestResults","totalTestsPeopleViral")],by=c("Date","state_ab"),all.y=TRUE)

colnames(covid.state)=c("Date","state_ab","Province_State","Country_Region","Confirmed Cases","Deaths (JHU count)","Recovered Cases","Population","start","start.c.100","start.d.10","Since 1st Case","Since 100th Case","Since 10th Death","state","state_population","Positive Tests - People","Negative Tests","Pending Tests","Hospitalized Cases","Deaths","Total Tests + Pending","Total Tests - Encounters","Total Tests - People")
covid.state[covid.state$state_ab=="US (Total)","state"]="US (Total)"
covid.state[covid.state$state_ab=="US (Total)","state_population"]=state_pop[state_pop$state_ab=="US (Total)","state_population"]
# 
# covid.state=as.data.table(group_by(covid.state,state_ab) %>% mutate(`New Daily Positive Tests`=as.numeric(as.vector(c(NA,diff(`Positive Tests`,lag=1)))),
#                           `New Daily Tests - People`=as.numeric(as.vector(c(NA,diff(`Total Tests - People`,lag=1)))) ,
#                           `Positivity Rate`=`New Daily Positive Tests`/`New Daily Tests`,
#                           `Lag Positivity Rate`=data.table::shift(`Positivity Rate`,1)))
# 
# 
# covid.state$`Positivity Rate`=ifelse(covid.state$`Positivity Rate`>0.95,NA,covid.state$`Positivity Rate`)
# covid.state$`Positivity Rate`=ifelse(is.nan(covid.state$`Positivity Rate`),NA,covid.state$`Positivity Rate`)
# 
# covid.state=as.data.table(group_by(covid.state,state_ab) %>% mutate(`month Average Positivity Rate`=as.numeric(as.vector(zoo::rollapply(`Positivity Rate`,width=21,FUN=function(x) mean(x,na.rm=T),fill=NA,align="right")))))
#                                                                   
# covid.state$`Positivity Rate`=ifelse(is.na(covid.state$`Positivity Rate`),covid.state$`month Average Positivity Rate`,covid.state$`Positivity Rate`)
# 
# covid.state=as.data.table(group_by(covid.state,state_ab) %>% mutate(`Average Positivity Rate`=as.numeric(as.vector(zoo::rollapply(`Positivity Rate`,width=7,FUN=function(x) mean(x,na.rm=T),fill=NA,align="right")))))
# 
# covid.state=subset(covid.state,select=-c(`month Average Positivity Rate`,`Average Positivity Rate`,`Lag Positivity Rate`))

print ("getting NYT data")

# plots=list()
# for (i in 1:length(unique(covid.state$state))){
#   plots[[i]]=ggplot(subset(covid.state,state==unique(covid.state$state)[i]),aes(x=Date)) + geom_line(aes(y=`Positive Tests`/`Total Tests`)) 
# #+ geom_line(aes(y=`Confirmed Cases`,color='Confirmed Cases'))
# + ggtitle(paste0(unique(covid.state$state)[i]," - Positive Rate")) 
# + ylab("Total Cumulative")
# 
# }
# 
# grid.arrange(plot3,plot4,plot5,plot6,
#              plot7,plot8,plot9,plot10,
#              plot11,plot12,plot13,plot14,
#              plot15,plot16,plot17,plot18,
#           
#              ncol=2)


print(mem_used()/1000000000)

#county-level data from the New York Times

# if (weekdays(Sys.Date())=="Friday"){
# covid.county=fread("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
# write.csv(covid.county,'covid_county.csv',row.names = F)
# } else {
  #covid.county=fread("covid_county.csv",stringsAsFactors = F) #HERE
  covid.county=fread("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv",stringsAsFactors = F)
# }
print("Downloaded NYT County-Level Data")
covid.county$date=as.Date(covid.county$date)
covid.county$state=as.character(covid.county$state)
covid.county$county=as.character(covid.county$county)
covid.county$cases=as.numeric(covid.county$cases)
covid.county$deaths=as.numeric(covid.county$deaths)

covid.county=merge(covid.county,as.data.frame(group_by(covid.county, state,county) %>% filter(cases>=1) %>% summarise(start.c=min(date))),by=c("state","county"),all=TRUE) # add date of 1st case
covid.county=merge(covid.county,as.data.frame(group_by(covid.county, state,county) %>% filter(cases>=100) %>% summarise(start.c.100=min(date))),by=c("state","county"),all=TRUE) # add date of 100th case
covid.county=merge(covid.county,as.data.frame(group_by(covid.county, state,county) %>% filter(deaths>=1) %>% summarise(start.d=min(date))),by=c("state","county"),all=TRUE) # add date of 1st death

covid.county$since_start=covid.county$date-covid.county$start.c
covid.county$since_case_100=covid.county$date-covid.county$start.c.100
covid.county$since_1st_death=covid.county$date-covid.county$start.d

county_pop=fread("county_pop.csv",stringsAsFactors = F)
colnames(county_pop)[c(3,4)]=c("population_2010_census","population")
covid.county=merge(covid.county,county_pop,by=c("state","county"),all.x=T)

colnames(covid.county)=c("state","county","Date","fips","Cases","Deaths","start.c","start.c.100","start.d","Since 1st Case","Since 100th Case","Since 1st Death","population_2010_census","population")

covid.county$county.state=paste(covid.county$county,covid.county$state,sep=", ")
covid.county=arrange(covid.county,Date)



print(mem_used()/1000000000)