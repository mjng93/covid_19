

#source("covid.r")

library(shiny)
library(shinydashboard)

library(reshape2)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(zoo)
library(quantmod)
library(rmarkdown)
library(TTR)
library(gridExtra)
library(grid)
library(DT)
library(kableExtra)
library(tables)
library(knitr)
library(rsconnect)

library(ggthemes)
library(stargazer)
library(ggplot2)
library(DT)






sandbox5.UI <- function(id) {
  
  ns <- NS(id)
  
 
 
div(
  mainPanel(
           fluidRow(
                  column(2,
                  img(src = "covid.jpg", height = 72, width = 150,align="right")),
                  column(8,
                  h1("Covid-19 Data Tracker",align="center")),
                  column(2,
                  img(src = "covid.jpg", height = 72, width = 150,align="left"))
                  ),
                  
           column(12,
                  h2("Global",align='center'),
                  p("As of",
                    
                    as.character(format(max(covid.agg.total$Date,na.rm=T),"%B %d")),
                    
                    " there have been a worldwide cumulative total of:",align="center"),
                  p(
                    
                    fluidRow(
                      column(6,
                             h2(strong(format(tail(covid.agg.total[,"Confirmed Cases"],1),format="d",big.mark = ",")),align="center"),
                             h5("Confirmed Cases",align="center")
                      ),
                      
                      column(6,
                             h2(strong(format(tail(covid.agg.total[,"Deaths"],1),format="d",big.mark = ",")),align="center"),
                             h5("Deaths",align="center")
                      )
                      ),
                    
                    "Yesterday, on ",
                             as.character(format(max(covid.agg.total$Date,na.rm=T),"%B %d")),
                             ", confirmed cases worldwide grew by",
                             format(tail(diff(covid.agg.total[,"Confirmed Cases"],lag=1),1),format="d",big.mark = ","),
                             " (",
                             as.character(round(tail(as.numeric(as.vector(Delt(covid.agg.total[,"Confirmed Cases"],k=1)))*100,1),1)),
                             "% ), and Deaths grew by",
                             format(tail(diff(covid.agg.total[,"Deaths"],lag=1),1),format="d",big.mark = ","),
                             " (",
                             as.character(round(tail(as.numeric(as.vector(Delt(covid.agg.total[,"Deaths"],k=1)))*100,1),1)),
                             "% ). Over the past week, cases have increased by",
                             format(tail(diff(covid.agg.total[,"Confirmed Cases"],lag=7),1),format="d",big.mark = ","),
                             " (",
                             as.character(round(tail(as.numeric(as.vector(Delt(covid.agg.total[,"Confirmed Cases"],k=7)))*100,1),1)),
                             "% ) and deaths have increases by",
                    format(tail(diff(covid.agg.total[,"Deaths"],lag=7),1),format="d",big.mark = ","),
                    " (",
                    as.character(round(tail(as.numeric(as.vector(Delt(covid.agg.total[,"Deaths"],k=7)))*100,1),1)),
                    "% ). Exponential growth of the virus means that the number of cases and deaths are doubling very quickly. Based on the average daily growth rate over the last 3 days globally: ",
                  )
           ),
           
           br(),
           fluidRow(
           column(6,
                  h5("Cases doubling in",align="center"),
                  h2(strong(format(round(
                    
                    (log(2)/mean((tail(as.numeric(as.vector(Delt(covid.agg.total[,"Confirmed Cases"],k=1))),3)),na.rm=T))
                    
                    ,1),format="d",big.mark = ",")),align="center"),
                  h5("Days",align="center")
           ),
           
           column(6,
                  h5("Deaths doubling in",align="center"),
                  h2(strong(format(round(
                    
                    (log(2)/(mean(tail(as.numeric(as.vector(Delt(covid.agg.total[,"Deaths"],k=1))),3),na.rm = T)))
                    
                    ,1),format="d",big.mark = ",")),align="center"),
                  h5("Days",align="center")
           )
           ),
           br(),
           
      
      fluidRow(
        column(6,
               plotlyOutput(ns("countries.log.cases"))),
        column(6,
               plotlyOutput(ns("countries.log.deaths")))
      ),
   
    
      
      br(),
      
  
      br(),
      
      column(12,dataTableOutput(ns("countries.tbl"))),
      
      br(),
      br(),
      
      fluidRow(
        br(),
        h2("United States",align="center"),
        column(12,p("In the United States there are a cumualtive total of:",align="center")),
        br(),
        column(6,
               h2(strong(format(tail(covid.agg[covid.agg$Country.Region=="US","Confirmed Cases"],1),format="d",big.mark = ",")),align="center"),
               h5("Confirmed Cases",align="center")
        ),
        
        column(6,
               h2(strong(format(tail(covid.agg[covid.agg$Country.Region=="US","Deaths"],1),format="d",big.mark = ",")),align="center"),
               h5("Deaths",align="center")
        ),
        
        br(),
        fluidRow(
          column(6,
                 plotlyOutput(ns("states.log.cases"))),
          column(6,
                 plotlyOutput(ns("states.log.deaths")))
        ),
        
        br(),
        
        column(12,
               p("Yesterday, on ",
                 as.character(format(max(covid.agg$Date,na.rm=T),"%B %d")),
                 ", confirmed cases in the US grew by",
                 format(tail(diff(subset(covid.agg,Country.Region=="US")[,"Confirmed Cases"],lag=1),1),format="d",big.mark = ","),
                 " (",
                 as.character(round(tail(as.numeric(as.vector(Delt(subset(covid.agg,Country.Region=="US")[,"Confirmed Cases"],k=1)))*100,1),1)),
                 "% ), and Deaths grew by",
                 format(tail(diff(subset(covid.agg,Country.Region=="US")[,"Deaths"],lag=1),1),format="d",big.mark = ","),
                 " (",
                 as.character(round(tail(as.numeric(as.vector(Delt(subset(covid.agg,Country.Region=="US")[,"Deaths"],k=1)))*100,1),1)),
                 "% ). Over the past week, cases have increased by",
                 format(tail(diff(subset(covid.agg,Country.Region=="US")[,"Confirmed Cases"],lag=7),1),format="d",big.mark = ","),
                 " (",
                 as.character(round(tail(as.numeric(as.vector(Delt(subset(covid.agg,Country.Region=="US")[,"Confirmed Cases"],k=7)))*100,1),1)),
                 "% ) and deaths have increases by",
                 format(tail(diff(covid.agg[covid.agg$Country.Region=="US","Deaths"],lag=7),1),format="d",big.mark = ","),
                 " (",
                 as.character(round(tail(as.numeric(as.vector(Delt(covid.agg[covid.agg$Country.Region=="US","Deaths"],k=7)))*100,1),1)),
                 "% )."
                 
               )
               
        ) ,
        
        br(),
        br(),
        
        column(12,dataTableOutput(ns("states.tbl"))),
        
        br(),
        br(),
        
        
        fluidRow(
          column(12,
                 p("Exponential growth of the virus means that the number of cases and deaths are doubling very quickly. Based on the average daily growth rate over the last 3 days in the US: ",
                   )
          ),
          br(),
          column(6,
                 h5("Cases doubling in",align="center"),
                 h2(strong(format(round(
                   
                   (log(2)/mean((tail(as.numeric(as.vector(Delt(subset(covid.agg,Country.Region=="US")[,"Confirmed Cases"],k=1))),3)),na.rm=T))
                   
                   ,1),format="d",big.mark = ",")),align="center"),
                 h5("Days",align="center")
          ),
          
          column(6,
                 h5("Deaths doubling in",align="center"),
                 h2(strong(format(round(
                   
                   (log(2)/(mean(tail(as.numeric(as.vector(Delt(subset(covid.agg,Country.Region=="US")[,"Deaths"],k=1))),3),na.rm = T)))
                   
                   ,1),format="d",big.mark = ",")),align="center"),
                 h5("Days",align="center")
          )
        ),
          
          fluidRow(
            column(12,
                   p("Much of the response and understanding of the Covid-19 virus is dependent on widespread testing and tracing. The Covid Tracking Project provides data on the number of tests conducted in each US state, as well as hospitalizations that occur depending on how severe the sypmtoms are for a patient that tests positive. Yesterday, on ",
                     as.character(format(max(covid.state$Date,na.rm=T),"%B %d")),
                     " in the US there were:")
            ),
            br(),
            column(6,
                   h2(strong(format(tail(diff(covid.state[covid.state$state_ab=="US (Total)","Total Tests"],lag=1),1),format="d",big.mark = ",")),align="center"),
                   h5("New Covid Tests",align="center")
            ),
            
            column(6,
                   h2(strong(format(tail(diff(covid.state[covid.state$state_ab=="US (Total)","Hospitalized Cases"],lag=1),1),format="d",big.mark = ",")),align="center"),
                   h5("New Hospitalized Cases",align="center")
            )
          ),
          
        
        h4("County-Level Data",align="center"),
        
        column(12,plotlyOutput(ns("county.cases.bar"))),
        #column(12,dataTableOutput(ns("counties.tbl"))),
        
        br(),
        br(),
        
        column(12,
               h2("A Note on the data",align="center"),
               h4("Measurement"),
               p("The Data for this app is provided primarily through",
               span(a("Johns Hopkins University",target="_blank",href="https://github.com/CSSEGISandData/COVID-19")),", ",
               span(a("the Covid Tracking Project",target="_blank",href="https://covidtracking.com/")),", and ",
               span(a("The New York Times",target="_blank",href="https://github.com/nytimes/covid-19-data")),
                ". Though the data are from very reliable sources, there is widespread reporting that official statistics likely underestimate the scope of confirmed cases and deaths of Covid-19. Confirmed cases are limited by the degree to which testing is available. Testing has been",
               span(a("limited in the US",target="_blank",href="https://www.nytimes.com/2020/03/28/us/testing-coronavirus-pandemic.html")),
                "and many hopsitals are limiting tests to those who exhibit serious symptoms or are in at risk populations (the elderly or those with pre-existing conditions). Moreover, there are reports that countries like",
               span(a("China",target="_blank",href="https://www.cnbc.com/2020/04/01/coronavirus-china-hid-extent-of-outbreak-us-intelligence-reportedly-says.html")),
               "have underreported the number of confirmed cases, whereas",
               span(a("other reporting",target="_blank",href="https://www.wsj.com/articles/questions-about-accuracy-of-coronavirus-tests-sow-worry-11585836001?mod=hp_lead_pos3")),
               "shows that current Covid-19 testing in the US may have a relatively high rate of false negatives.",
               span(a("One study",target="_blank",href="https://www.nber.org/papers/w26906")),
               " estimates that up to 80% of all actual Covid-19 cases in China were not officially reported (though that figure has since fallen to ~11% as of the end of February). In addition, Covid-19 related deaths are also underreported due to for example, people dying without being able to reach a hospital or without being tested.",
               span(a("Reporting by the Wall Street Journal",target="_blank",href="https://www.wsj.com/articles/italys-coronavirus-death-toll-is-far-higher-than-reported-11585767179?emailToken=424264b710198413433f9f9fb63b9e7drhyIMT64MqVfXc721zdzosZnqZRLvMYgvLKi5johQiFSbXtrAcezOSmvmWDIAi6wWE81oIg8m73sntjKxRq3rFUEkGWmhSrNoUkOt9WGPCzX50fDRgKuWTyz6aWa5YIh&reflink=article_email_share"),
                    "for example indicates that Covid-19 deaths in Italy have likely been much higher than have been reported in official estimates. All of this is to say that official statistics likely represent a floor on the number of cases and deaths and should be taken in context.")),
               br(),
               h4("Exponential Growth and Log Transformations"),
               p("The nature of infectious diseases like Covid-19 mean that growth is often exponential. That is, cases grow non-linearly at a rapid rate. For that reason, many of the charts shown here are in log (base e) scale for ease of readability; transforming the data onto what is more easily inrepreted as a linear plane. In addition, the overview page computes coubling rates for some intuition on how quickly the spread of the virus is growing. The other pages on this cite provide various other data transformations of the data, including in raw levels. For a primer on log transformations, see this",
                 span(a("Forbes article",target="_blank",href="https://www.forbes.com/sites/naomirobbins/2012/01/19/when-should-i-use-logarithmic-scales-in-my-charts-and-graphs/#4b84b25d5e67")),
                 "or this",
                 span(a("Twitter thread",target="_blank",href="https://twitter.com/jburnmurdoch/status/1237748598051409921")),
                 ". Lastly, this app shows by default the raw number of total cases, though it also contains data transformations on a per capita basis. While per capita metrics are useful, given the nature of the disease, it's also important to look at the total raw number of cumulative cases since the disease spread is more likely to occur when many people have it in the same area. For this reason, density and geography are actually the most important metrics/adjustments to consider instead of just total population by state/county/region and is the reason why the raw number of cases are shown without adjustment (see for example,",
                 span(a("this thread",target="_blank",href="https://twitter.com/Nate_Cohn/status/1245717330316984324")),
                 "from NYT data journalist Nate Cohn or",
                 span(a("this explainer",target="_blank",href="https://www.ft.com/video/9a72a9d4-8db1-4615-8333-4b73ae3ddff8")),
                 " from Financial Times Journalist John Burn-Murdoch.)"),
               br(),
               h4("Projections"),
               p("One thing you will not find on this site is forecasts of the virus' spread and predictions on the number of cases and deaths. I am not an epidemiologist and building models of contagious disease is",
               span(a("very difficult",target="_blank",href="https://fivethirtyeight.com/features/why-its-so-freaking-hard-to-make-a-good-covid-19-model/")),
               " in general. For one thing, and as noted above, measurment is often unreliable. It's difficult to back out a fatality rate for example, if you don't have exact estimates on the denominantor (how many people have the disease) or the numerator (how many people have actually died from the disease, though this estimate is probably more accurate than the former). In addition, because of the exponential growth of the disease, small changes in initial conditions or assumptions can have large changes in the trajectory of the disease. Finally, government responses to the virus, such as stay at home orders and treatment, are endogenous and need to be incorporated into any model."),
               br(),
               
               p("Despite these difficulties, various projections have been made by professional epidemiologists about the spread of the disease. The White House released figures that estimate ",
               span(a("100,000 to 200,000 deaths",target="_blank",href="https://www.vox.com/science-and-health/2020/3/31/21202188/us-deaths-coronavirus-trump-white-house-presser-modeling-100000")),
               "in the US, even incorproating a strong government response. Their models supposedly heavily mirror the model from the",
               span(a("University of Washington",target="_blank",href="https://covid19.healthdata.org/projections")),
               ", which includes interactive visuals and info on how quickly the US hopsital system may be overrun based on current projections. Another widely circulated forecast comes from ",
               span(a("Imperial College",target="_blank",href="https://www.imperial.ac.uk/media/imperial-college/medicine/sph/ide/gida-fellowships/Imperial-College-COVID19-NPI-modelling-16-03-2020.pdf")),
               "which predicted relatively dire outcomes of 1-1.2 million deaths in the US without strict suppression measures. The report is credited with pushing the US and UK to take more",
              span(a("drastic measures",target="_blank",href="https://www.nytimes.com/2020/03/17/world/europe/coronavirus-imperial-college-johnson.html")),
              " to contain the virus.",
              span(a("Stanford",target="_blank",href="https://news.stanford.edu/2020/03/30/modeling-social-distancings-impact/")),
              " has also developed model simulations of the virus and the impact it will have on health systems with a",
              span(a("digital interactive",target="_blank",href="https://covid-measures.github.io/")),
              " that allow you to adjust assumptions about the virus' spread. The modelers ",
              span(a("note",target="_blank",href="https://twitter.com/morde/status/1242137323132743691?s=20")),
              " that the results are qualitatively similar to the Imperial College study, even though the empirical framework is totally different."
              
               )
               )
        
        
        
        
        
      )
   
      
  
  ,width = 12)
,tags$head(tags$style(type="text/css", ".container-fluid {  max-width: 12600px; /* or 950px */}")))
 
}
  
  
  
  sandbox.server5 <- function(input, output, session,data5,data5a,data5b,data5c){
    
    module_data=data5
    module_data.state=data5a
    module_data.tests=data5b
    module_data.county=data5c
  
    
    #plots

    
    output$countries.log.cases <- renderPlotly({
      # p2 <- ggplot(data=module_data[module_data$Country.Region %in% top.countries,],aes(x=`Since 100th Case`, y=log(`Confirmed Cases`), colour =  Country.Region)) + geom_line()  + labs(caption="Source: John Hopkins University.", title ="Confirmed Cases", subtitle = "Log Scale") + xlab("Days Since 100th Case") + ylab("Total Cumulative (Log Points)")   + scale_colour_manual(values = c('black','steelblue3','firebrick','chartreuse3','purple','azure4','green','cadetblue1','chocolate3','darkgoldenrod1','darkgreen','deeppink','blue',"grey","red")) + geom_hline(yintercept = 0) + xlim(0,40) + ylim(4,13)
      # p2
      plot_ly(covid.agg[covid.agg$Country.Region %in% top.countries,], x = ~`Since 100th Case`, y=~log(`Confirmed Cases`), color =  ~Country.Region, type = 'scatter', mode = 'lines',colors = c("steelblue3","red","chartreuse3","purple","darkgoldenrod2","aquamarine1","forestgreen","dodgerblue4","deeppink","azure4","burlywood4","chocolate1","olivedrab1","gray78","black")) %>%
        layout(title = "Total Confirmed Cases (Log Scale)",
               xaxis = list(title = "Days Since 100th Case",range=c(0,max(covid.agg$`Since 100th Case`,na.rm=T))),
               yaxis = list (title = "Total Cumulative (Log Points)",range=c(4,13)))
      
     
      
    })
    
    output$countries.cases <- renderPlotly({
# 
#     p1<- ggplot(data=module_data[module_data$Country.Region %in% top.countries,],aes(x=`Since 100th Case`, y=`Confirmed Cases`, colour =  Country.Region)) + geom_line()  + labs(caption="Source: John Hopkins University.", title ="Confirmed Cases", subtitle = "Exponential") + xlab("Days Since 100th Case") + ylab("Total Cumulative")   + scale_colour_manual(values = c('black','steelblue3','firebrick','chartreuse3','purple','azure4','green','cadetblue1','chocolate3','darkgoldenrod1','darkgreen','deeppink','blue')) + geom_hline(yintercept = 0)+  xlim(0,40) 
#     p1
      
      plot_ly(covid.agg[covid.agg$Country.Region %in% top.countries,], x = ~`Since 100th Case`, y=~`Confirmed Cases`, color =  ~Country.Region, type = 'scatter', mode = 'lines',colors = c("steelblue3","red","chartreuse3","purple","darkgoldenrod2","aquamarine1","forestgreen","dodgerblue4","deeppink","azure4","burlywood4","chocolate1","olivedrab1","gray78","black")) %>%
        layout(title = "Total Confirmed Cases",
               xaxis = list(title = "Days Since 100th Case",range=c(0,max(covid.agg$`Since 100th Case`,na.rm = T))),
               yaxis = list (title = "Total Cumulative",range=c(0,max(covid.agg$`Confirmed Cases`,na.rm = T))))
      
    })
    
    output$countries.deaths <- renderPlotly({
  
      covid.top.deaths=subset(covid,Deaths>quantile(covid$Deaths[covid$ObservationDate==max(covid$ObservationDate)],seq(0,1,.05))[20],na.rm=T)
      top.countries.deaths=unique(covid.top.deaths$Country.Region)
      
      
      plot_ly(covid.agg[covid.agg$Country.Region %in% top.countries.deaths,], x = ~`Since 10th Death`, y=~Deaths, color =  ~Country.Region, type = 'scatter', mode = 'lines',colors = c("steelblue3","red","chartreuse3","purple","darkgoldenrod2","aquamarine1","forestgreen","dodgerblue4","deeppink","azure4","burlywood4","chocolate1","olivedrab1","gray78","black")) %>%
        layout(title = "Deaths",
               xaxis = list(title = "Days Since 10th Death",range=c(0,max(covid.agg$`Since 10th Death`,na.rm = T))),
               yaxis = list (title = "Total Cumulative",range=c(0,max(covid.agg$`Deaths`,na.rm = T))))
      
    })
    
    output$countries.log.deaths <- renderPlotly({
      
      covid.top.deaths=subset(covid,Deaths>quantile(covid$Deaths[covid$ObservationDate==max(covid$ObservationDate)],seq(0,1,.05))[20],na.rm=T)
      top.countries.deaths=unique(covid.top.deaths$Country.Region)
      
      
      plot_ly(covid.agg[covid.agg$Country.Region %in% top.countries.deaths,], x = ~`Since 10th Death`, y=~log(Deaths), color =  ~Country.Region, type = 'scatter', mode = 'lines',colors = c("steelblue3","red","chartreuse3","purple","darkgoldenrod2","aquamarine1","forestgreen","dodgerblue4","deeppink","azure4","burlywood4","chocolate1","olivedrab1","gray78","black")) %>%
        layout(title = "Deaths (Log Scale)",
               xaxis = list(title = "Days Since 10th Death",range=c(0,max(covid.agg$`Since 10th Death`,na.rm = T))),
               yaxis = list (title = "Total Cumulative",range=c(2,10)))
      
    })
    
    output$countries.tbl <- renderDT({
      
      tbl.sum=arrange(covid.agg,Date)
      colnames(tbl.sum)=gsub("Confirmed Cases","Cases",colnames(tbl.sum))
      tbl.sum=as.data.frame(group_by(tbl.sum,Country.Region) %>% mutate(`Change in Cases`=c(NA,diff(`Cases`,lag=1)) ,
                                                                        # `% Change in Cases`=round(as.numeric(as.vector(Delt(`Cases`,k=1)))*100,2),
                                                                        `Change in Deaths`=c(NA,diff(`Deaths`,lag=1)) ,
                                                                        # `% Change in Deaths`=round(as.numeric(as.vector(Delt(`Deaths`,k=1)))*100,2)
              ))
      
      tbl.sum$`Cases Per Capita`=round((tbl.sum$`Cases`/tbl.sum$Population)*100,2)
      tbl.sum$`Deaths Per Capita`=round((tbl.sum$`Deaths`/tbl.sum$Population)*100,2)
      
      tbl.sum=subset(tbl.sum,Date==max(tbl.sum$Date,na.rm=T))[,c("Date","Country.Region","Cases","Cases Per Capita","Change in Cases","Deaths","Deaths Per Capita","Change in Deaths")]
      tbl.sum=tbl.sum[order(-tbl.sum$`Cases`),]
      tbl.sum=subset(tbl.sum,Deaths>=10 & `Cases`>=100)
      colnames(tbl.sum)[2]="Country"
      tbl.sum
    
    }, class = "display nowrap compact", 
    
    options = list(dom = 'ft'),rownames=FALSE)
    
    #----------#
    #State Data#
    #----------#
    
    covid.state.top=subset(covid.kaggle,`Confirmed Cases`>quantile(covid.kaggle$`Confirmed Cases`[covid.kaggle$Date==max(covid.kaggle$Date)],seq(0,1,.05),na.rm=T)[19] & Country.Region=="US")
    top.states=unique(covid.state.top$Province.State)
    
    covid.state.top.deaths=subset(covid.kaggle,Deaths>quantile(covid.kaggle$Deaths[covid.kaggle$Date==max(covid.kaggle$Date,na.rm=T)],seq(0,1,.05),na.rm=T)[19] & Country.Region=="US")
    top.states.deaths=unique(covid.state.top.deaths$Province.State)
    
    
    
    output$states.log.cases <- renderPlotly({
     
      plot_ly(covid.kaggle[covid.kaggle$Province.State %in% top.states,], x = ~`Since 100th Case`, y=~log(`Confirmed Cases`), color =  ~Province.State, type = 'scatter', mode = 'lines',colors = c("steelblue3","red","chartreuse3","purple","darkgoldenrod2","aquamarine1","forestgreen","dodgerblue4","deeppink","azure4","burlywood4","chocolate1","olivedrab1","gray78","black")) %>%
        layout(title = "Total Confirmed Cases by State (Log Scale)",
               xaxis = list(title = "Days Since 100th Case",range=c(0,max(covid.kaggle[covid.kaggle$Province.State %in% top.states,"Since 100th Case"],na.rm=T))),
               yaxis = list (title = "Total Cumulative (Log Points)",range=c(4,13)))
      
      
      
    })
    
    output$states.cases <- renderPlotly({
     
      plot_ly(covid.kaggle[covid.kaggle$Province.State %in% top.states,], x = ~`Since 100th Case`, y=~`Confirmed Cases`, color =  ~Province.State, type = 'scatter', mode = 'lines',colors = c("steelblue3","red","chartreuse3","purple","darkgoldenrod2","aquamarine1","forestgreen","dodgerblue4","deeppink","azure4","burlywood4","chocolate1","olivedrab1","gray78","black")) %>%
        layout(title = "Total Confirmed Cases by State",
               xaxis = list(title = "Days Since 100th Case",range=c(0,max(covid.kaggle$`Since 100th Case`,na.rm = T))),
               yaxis = list (title = "Total Cumulative",range=c(0,max(covid.kaggle[covid.kaggle$state_ab!="US (Total)","Confirmed Cases"],na.rm = T))))
      
    })
    
    output$states.deaths <- renderPlotly({
      
      
      plot_ly(covid.kaggle[covid.kaggle$Province.State %in% top.states,], x = ~`Since 10th Death`, y=~Deaths, color =  ~Province.State, type = 'scatter', mode = 'lines',colors = c("steelblue3","red","chartreuse3","purple","darkgoldenrod2","aquamarine1","forestgreen","dodgerblue4","deeppink","azure4","burlywood4","chocolate1","olivedrab1","gray78","black")) %>%
        layout(title = "Deaths by State",
               xaxis = list(title = "Days Since 10th Death",range=c(0,max(covid.kaggle$`Since 10th Death`,na.rm = T))),
               yaxis = list (title = "Total Cumulative",range=c(0,max(covid.kaggle[covid.kaggle$state_ab!="US (Total)","Deaths"],na.rm = T))))
      
    })
    
    output$states.log.deaths <- renderPlotly({
      
      
      plot_ly(covid.kaggle[covid.kaggle$Province.State %in% top.states,], x = ~`Since 10th Death`, y=~log(Deaths), color =  ~Province.State, type = 'scatter', mode = 'lines',colors = c("steelblue3","red","chartreuse3","purple","darkgoldenrod2","aquamarine1","forestgreen","dodgerblue4","deeppink","azure4","burlywood4","chocolate1","olivedrab1","gray78","black")) %>%
        layout(title = "Deaths by State (Log Scale)",
               xaxis = list(title = "Days Since 10th Death",range=c(0,max(covid.kaggle[covid.kaggle$Province.State %in% top.states,"Since 10th Death"],na.rm=T))),
               yaxis = list (title = "Total Cumulative (Log Points)",range=c(2,8)))
      
    })
    
    output$states.tbl <- renderDT({
      
      tbl.sum.state=arrange(covid.kaggle[covid.kaggle$Country.Region=="US",],Date)
      colnames(tbl.sum.state)=gsub("Confirmed Cases","Cases",colnames(tbl.sum.state))
      tbl.sum.state=as.data.frame(group_by(tbl.sum.state,Province.State) %>% mutate(`Change in Cases`=c(NA,diff(`Cases`,lag=1)) ,
                                                                        # `% Change in Cases`=round(as.numeric(as.vector(Delt(`Cases`,k=1)))*100,2),
                                                                        `Change in Deaths`=c(NA,diff(`Deaths`,lag=1)) ,
                                                                        # `% Change in Deaths`=round(as.numeric(as.vector(Delt(`Deaths`,k=1)))*100,2)
      ))
      
      
      #tbl.sum.state$`Change in Cases`=paste0(paste(tbl.sum.state$`Cases`,tbl.sum.state$`% Change in Cases`,sep=" ("),"%)")
      
      tbl.sum.state$`Cases Per Capita`=round((tbl.sum.state$`Cases`/tbl.sum.state$state_population)*100,2)
      tbl.sum.state$`Deaths Per Capita`=round((tbl.sum.state$`Deaths`/tbl.sum.state$state_population)*100,2)
      
      tbl.sum.state=subset(tbl.sum.state,Date==max(tbl.sum.state$Date,na.rm=T))[,c("Date","Province.State","Cases","Cases Per Capita","Change in Cases","Deaths","Deaths Per Capita","Change in Deaths")]
      
     
      
      tbl.sum.state=tbl.sum.state[order(-tbl.sum.state$`Cases`),]
      tbl.sum.state=subset(tbl.sum.state,Deaths>=10 & `Cases`>=100)
      colnames(tbl.sum.state)[2]="State"
      tbl.sum.state
      
    }, class = "display nowrap compact", 
    
    options = list(dom = 'ft'),rownames=FALSE)
    
    
    covid.county.top=arrange(covid.county,Date)
    covid.county.top=as.data.frame(group_by(covid.county.top,county.state) %>% mutate(count=length(Cases)))
    covid.county.top=subset(covid.county.top,count>7)
    covid.county.top=as.data.frame(group_by(covid.county.top,county.state) %>% mutate(`Change in Cases`=c(NA,diff(Cases,lag=1)) ,
                                                                               `% Change in Cases`=round(as.numeric(as.vector(Delt(Cases,k=1)))*100,2),
                                                                               `1-Week Change in Cases`=c(rep(NA,7),diff(Cases,lag=7)) ,
                                                                               `1-Week % Change in Cases`=round(as.numeric(as.vector(Delt(Cases,k=7)))*100,2),
                                                                               `Change in Deaths`=c(NA,diff(`Deaths`,lag=1)) ,
                                                                               `% Change in Deaths`=round(as.numeric(as.vector(Delt(`Deaths`,k=1)))*100,2),
                                                                               `1-Week Change in Deaths`=c(rep(NA,7),diff(`Deaths`,lag=7)) ,
                                                                               `1-Week % Change in Deaths`=round(as.numeric(as.vector(Delt(`Deaths`,k=7)))*100,2)
    ))
    
    
    covid.county.new.cases=subset(covid.county.top,`1-Week Change in Cases`>=quantile(covid.county.top$`1-Week Change in Cases`[covid.county.top$Date==max(covid.county.top$Date,na.rm=T)],seq(0,1,.01),na.rm=T)[100])
    top.county.weekly.cases=unique(covid.county.new.cases$county.state)
    
    
    covid.county.top=arrange(arrange(covid.county.top,Date),-`1-Week % Change in Cases`)
    
    output$county.cases.bar <- renderPlotly({
      
      plot.data=arrange(covid.county.top[covid.county.top$county.state %in% top.county.weekly.cases & covid.county.top$Date==max(covid.county.top$Date,na.rm=T),],-`1-Week Change in Cases`)
      
      plot_ly(plot.data, x = ~`1-Week Change in Cases`, y=~reorder(county.state,`1-Week Change in Cases`),  type = 'bar') %>%
        layout(title = "Fastest Growing Counties (Weekly Change in Cases)",
               xaxis = list(title = paste0("1-week change in cases: ",paste(format(max(covid.county.top$Date,na.rm=T)-7,"%b %d"),format(max(covid.county.top$Date,na.rm=T),"%b %d"),sep=" - "))),
               yaxis = list (title = "County"))
      
    })
    
    
    
  }
                  
                  