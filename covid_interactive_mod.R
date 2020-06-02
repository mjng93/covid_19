

source("covid.r")

library(shiny)
library(ggplot2)
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
library(plotly)
library(ggthemes)
library(stargazer)

sandbox.UI <- function(id) {
  
  ns <- NS(id)
  
  tabPanel("Country Data",
           #customHeader(title = "Data 'Sandbox' Manipulator"),
           column(12,
                  sidebarLayout(
                    sidebarPanel(h3("Country-Level Data"),
                                 radioButtons(ns("chart_type"),
                                              label = "Select Chart Type", 
                                              choices = c("Bar Chart" = "bar","Line Chart"="line"),
                                              selected = "line"),
                                 selectInput(ns('ystat'),
                                             label = 'Select statistic',
                                             choices=c("Confirmed Cases","Deaths","Recovered Cases"),
                                             selected="Deaths"
                                 ),
                                 radioButtons(ns("radio"),
                                              label = "Select Data Transformation", 
                                              choices = c("Levels" = "levels","Per Capita Levels" = "pc","Per Capita Daily Change"="pc.d","Average Per Capita Daily Change (Rolling, 7-days)"="pc.d.avg", "Log Levels" = "log", "Change (Daily)" = "diff", "Percent Change (Daily)" = "qoq","Average Daily Percent Change (Rolling, 7-days)"="chg.avg"), #"Percent Change (10-day)" = "mom"
                                              selected = "log"),
                                 
                                 selectInput(ns('xchoice'),
                                             label = 'Select time horizon',
                                             choices=c("Date","Since 1st Case","Since 100th Case","Since 10th Death"),
                                             selected="Since 10th Death"
                                 ),
                                 selectInput(ns("name"),
                                             label = "Select country:",
                                             choices = c(unique(covid.agg$Country.Region)),
                                             selected = c("US","Italy","Germany","France","Spain","China","Korea, South","Iran","United Kingdom"),
                                             multiple = TRUE
                                 ),
                                 
                                 downloadButton(ns("CovidData"), label = "Download Data")
                                 
                    ),
                    
                    
                    
                    #Main panel for displaying outputs ----
                    mainPanel(
                      tabsetPanel(type = "tabs",
                                  tabPanel("Plot", plotlyOutput(ns("plot"))),
                                  tabPanel("Summary", tableOutput(ns("summary"))),
                                  tabPanel("Correlations", tableOutput(ns("corr"))),
                                  tabPanel("Table", tableOutput(ns("table")))),
                    
                    
                    p("Data is sourced from Johns Hopkins Univerity CSSE as posted on",
                    span(a("Github",target="_blank",href="https://github.com/CSSEGISandData/COVID-19")),
                    # "and provided online through",
                    # span(a("Kaggle",target="_blank",href="https://www.kaggle.com/sudalairajkumar/novel-corona-virus-2019-dataset")),
                    ". All data is aggregated by Country/Region. Also inspired by the Financial Times ",
                    span(a("dataviz",target="_blank",href="https://www.ft.com/coronavirus-latest")),
                    " on Covid-19.",
                    "Population data is sourced from the ",
                    span(a("World Bank",target="_blank",href="https://data.worldbank.org/indicator/sp.pop.totl")),
                    ". Data is currently updated through",
                    span(as.character(format(as.Date(max(covid.agg$Date,na.rm=T)),"%B %d, %Y"))),". All code written by Michael Ng, available on",
                    span(a("Github",target="_blank",href="https://github.com/mjng93/covid_19")),"."
                    )
                    
                    )
                    
                  ) #,div(style = "width:5000px;height:10px")#style='padding:100px;'
           )
           
  )

  
  
}




sandbox.server <- function(input, output, session,data){
  
  module_data=data
  
  data_input <- reactive({
    if (input$radio=="levels"){
      module_data.levels=module_data[,c("Country.Region",input$xchoice,input$ystat)]
      df <- melt(module_data.levels,id.vars=c(input$xchoice,"Country.Region"))
      df <- df[df$Country.Region %in% input$name,]
      colnames(df)=c("xval","Country.Region","variable","value")
      df$value <- as.numeric(gsub(Inf,NA,df$value))
      df$value <- as.numeric(gsub(-Inf,NA,df$value))
      units="Levels"
    }
    
    if (input$radio=="pc"){
      module_data.pc=module_data[,c("Country.Region","Population",input$xchoice,input$ystat)]
      module_data.pc[,input$ystat]=(module_data.pc[,input$ystat]/module_data.pc[,"Population"])*100
      module_data.pc$Population=NULL
      df <- melt(module_data.pc,id.vars=c(input$xchoice,"Country.Region"))
      df <- df[df$Country.Region %in% input$name,]
      colnames(df)=c("xval","Country.Region","variable","value")
      df$value <- as.numeric(gsub(Inf,NA,df$value))
      df$value <- as.numeric(gsub(-Inf,NA,df$value))
      units="Percent"
    }
    
    if (input$radio=="log.pc"){
      module_data.log.pc=module_data[,c("Country.Region","Population",input$xchoice,input$ystat)]
      module_data.log.pc[,input$ystat]=log(1+(module_data.log.pc[,input$ystat]/module_data.log.pc[,"Population"])*100)
      module_data.log.pc$Population=NULL
      df <- melt(module_data.log.pc,id.vars=c(input$xchoice,"Country.Region"))
      df <- df[df$Country.Region %in% input$name,]
      colnames(df)=c("xval","Country.Region","variable","value")
      df$value <- as.numeric(gsub(Inf,NA,df$value))
      df$value <- as.numeric(gsub(-Inf,NA,df$value))
      units="Percent"
    }
    
    if (input$radio=="log"){
      module_data.log=module_data[,c("Country.Region",input$xchoice,input$ystat)]
      module_data.log[,input$ystat]=log(module_data.log[,input$ystat])
      df <- melt(module_data.log,id.vars=c(input$xchoice,"Country.Region"))
      df <- df[df$Country.Region %in% input$name,]
      colnames(df)=c("xval","Country.Region","variable","value")
      df$value <- as.numeric(gsub(Inf,NA,df$value))
      df$value <- as.numeric(gsub(-Inf,NA,df$value))
      units="Log Level"
    }
    
    if (input$radio=="pc.d"){
      module_data.pc.d=module_data[,c("Country.Region","Population",input$xchoice,input$ystat)]
      module_data.pc.d[,input$ystat]=(module_data.pc.d[,input$ystat]/module_data.pc.d[,"Population"])*100
      module_data.pc.d=as.data.frame(group_by(module_data.pc.d,Country.Region) %>% mutate(test=as.numeric(as.vector(c(NA,diff(get(input$ystat),lag=1))))))
      module_data.pc.d[,input$ystat]=module_data.pc.d[,c("test")]
      module_data.pc.d=subset(module_data.pc.d,select=-c(test,Population))
      df <- melt(module_data.pc.d,id.vars=c(input$xchoice,"Country.Region"))
      df <- df[df$Country.Region %in% input$name,]
      colnames(df)=c("xval","Country.Region","variable","value")
      df$value <- as.numeric(gsub(Inf,NA,df$value))
      df$value <- as.numeric(gsub(-Inf,NA,df$value))
      units="Percentage Points"
    }
    
    if (input$radio=="pc.d.avg"){
      module_data.pc.d.avg=module_data[,c("Country.Region","Population",input$xchoice,input$ystat)]
      module_data.pc.d.avg[,input$ystat]=(module_data.pc.d.avg[,input$ystat]/module_data.pc.d.avg[,"Population"])*100
      module_data.pc.d.avg=as.data.frame(group_by(module_data.pc.d.avg,Country.Region) %>% mutate(test=rollapply(as.numeric(as.vector(c(NA,diff(get(input$ystat),lag=1)))),width=7,mean,fill=NA,align="right")))
      
      module_data.pc.d.avg[,input$ystat]=module_data.pc.d.avg[,c("test")]
      module_data.pc.d.avg=subset(module_data.pc.d.avg,select=-c(test,Population))
      
      df <- melt(module_data.pc.d.avg,id.vars=c(input$xchoice,"Country.Region"))
      df <- df[df$Country.Region %in% input$name,]
      colnames(df)=c("xval","Country.Region","variable","value")
      df$value <- as.numeric(gsub(Inf,NA,df$value))
      df$value <- as.numeric(gsub(-Inf,NA,df$value))
      units="Percentage Points"
    }
    
    
    if (input$radio=="qoq"){
      module_data.qoq=module_data[,c("Country.Region",input$xchoice,input$ystat)]
      
      
      module_data.qoq=as.data.frame(group_by(module_data.qoq,Country.Region) %>% mutate(test=as.numeric(as.vector(Delt(get(input$ystat),k=1)))*100))
      
      module_data.qoq[,input$ystat]=module_data.qoq[,c("test")]
      module_data.qoq=subset(module_data.qoq,select=-c(test))
      
      
      df <- melt(module_data.qoq,id.vars=c(input$xchoice,"Country.Region"))
      df <- df[df$Country.Region %in% input$name,]
      colnames(df)=c("xval","Country.Region","variable","value")
      df$value <- as.numeric(gsub(Inf,NA,df$value))
      df$value <- as.numeric(gsub(-Inf,NA,df$value))
        units="Percent Change"
    }
    
    if (input$radio=="diff"){
      
      module_data.diff=module_data[,c("Country.Region",input$xchoice,input$ystat)]
     
    
      module_data.diff=as.data.frame(group_by(module_data.diff,Country.Region) %>% mutate(test=as.numeric(as.vector(c(NA,diff(get(input$ystat),lag=1))))))
      
      module_data.diff[,input$ystat]=module_data.diff[,c("test")]
      module_data.diff=subset(module_data.diff,select=-c(test))
    
      
      df <- melt(module_data.diff,id.vars=c(input$xchoice,"Country.Region"))
      df <- df[df$Country.Region %in% input$name,]
      colnames(df)=c("xval","Country.Region","variable","value")
      df$value <- as.numeric(gsub(Inf,NA,df$value))
      df$value <- as.numeric(gsub(-Inf,NA,df$value))
      units="New Cases"
      
    }
    
    if (input$radio=="mom"){
      module_data.mom=module_data[,c("Country.Region",input$xchoice,input$ystat)]
      
      module_data.mom=as.data.frame(group_by(module_data.mom,Country.Region) %>% mutate(test=as.numeric(as.vector(Delt(get(input$ystat),k=7)))*100))
      
      module_data.mom[,input$ystat]=module_data.mom[,c("test")]
      module_data.mom$test=NULL
      
      df <- melt(module_data.mom,id.vars=c(input$xchoice,"Country.Region"))
      df <- df[df$Country.Region %in% input$name,]
      colnames(df)=c("xval","Country.Region","variable","value")
      df$value <- as.numeric(gsub(Inf,NA,df$value))
      df$value <- as.numeric(gsub(-Inf,NA,df$value))
      print(tail(df))
      
      units="Percent Change"
    }
    
    if (input$radio=="chg.avg"){
      module_data.chg.avg=module_data[,c("Country.Region",input$xchoice,input$ystat)]
      
      module_data.chg.avg=as.data.frame(group_by(module_data.chg.avg,Country.Region) %>% mutate(test=rollapply(as.numeric(as.vector(Delt(get(input$ystat),k=1)))*100,width=7,mean,fill=NA,align="right")))
      
      module_data.chg.avg[,input$ystat]=module_data.chg.avg[,c("test")]
      module_data.chg.avg=subset(module_data.chg.avg,select=-c(test))
      
      df <- melt(module_data.chg.avg,id.vars=c(input$xchoice,"Country.Region"))
      df <- df[df$Country.Region %in% input$name,]
      colnames(df)=c("xval","Country.Region","variable","value")
      df$value <- as.numeric(gsub(Inf,NA,df$value))
      df$value <- as.numeric(gsub(-Inf,NA,df$value))
      
      units="Percent Change"
    }
    
    # if (input$xval=="Since 1st Case" | input$xval=="Since 100th Case" | input$xval=="Since 10th Death"){
    #   df=df[df$xval>=-1,]
    # }
    
    df
    
  })
  
  unit_input <- reactive({
    if (input$radio=="levels"){units="Total Cumulative"}
    if (input$radio=="pc"){units="Percent (1=1%)"}
    if (input$radio=="log.pc"){units="Log Points: computed as log(1 + Per Capita Level in decimals)"}
    if (input$radio=="log"){units="Log Points"}
    if (input$radio=="diff"){units="New Cases"}
    if (input$radio=="qoq"){units="Percent Change (1=1%)"}
    if (input$radio=="pc.d"){units="Percentage Points"}
    if (input$radio=="pc.d.avg"){units="Percentage Points"}
    if (input$radio=="chg.avg"){units="Average Daily Percent Change (1=1%)"}
    
    units
    
  })
  
  
  output$plot <- renderPlotly({
    
    plot.data=data_input()
    if (min(plot.data$xval,na.rm=T)<0){
      plot.data=plot.data[plot.data$xval>=-1,]
    }
    
    if (input$chart_type=="line"){
    plot_ly(plot.data, x = ~xval,y= ~value, color = ~Country.Region, type = 'scatter', mode = 'lines') %>%
      layout(title = input$ystat,
             xaxis = list(title = "Days"),
             yaxis = list (title = unit_input()))
    }
    
    else if (input$chart_type=="bar"){
      plot_ly(plot.data, x = ~xval,y= ~value, color = ~Country.Region, type = 'bar') %>%
        layout(title = input$ystat,
               xaxis = list(title = "Days"),
               yaxis = list (title = unit_input()))
    }
    
    #,text = paste('Value:', value,'<br>Date: ', as.Date(date,format='%b-%Y'),  '<br>Variable: ', variable)
    
    #print(df.ggplot)
    
    
  }#,height=400,width=1000
  )
  
  output$summary <- renderTable({
    
    df.sum=data_input()
    #df.sum=na.omit(df.sum)
    
    summary=data.frame(series=numeric(length(input$name)),l1=numeric(length(input$name)),l2=numeric(length(input$name)),l3=numeric(length(input$name)),min=numeric(length(input$name)),max=numeric(length(input$name)),sd=numeric(length(input$name)))
    
    for (i in 1:length(input$name)){
      summary[i,'series']=input$name[i]
      summary[i,'l1']=subset(df.sum,Country.Region==input$name[i])[nrow(subset(df.sum,Country.Region==input$name[i])),"value"]
      summary[i,'l2']=subset(df.sum,Country.Region==input$name[i])[nrow(subset(df.sum,Country.Region==input$name[i]))-1,"value"]
      summary[i,'l3']=subset(df.sum,Country.Region==input$name[i])[nrow(subset(df.sum,Country.Region==input$name[i]))-2,"value"]
      summary[i,'min']=min(subset(df.sum,Country.Region==input$name[i])[,"value"],na.rm=T)
      summary[i,'max']=max(subset(df.sum,Country.Region==input$name[i])[,"value"],na.rm=T)
      summary[i,'sd']=sd(subset(df.sum,Country.Region==input$name[i])[,"value"],na.rm=T)
     
    }
    colnames(summary)=c("Series",subset(df.sum,Country.Region==input$name[i])[nrow(subset(df.sum,Country.Region==input$name[i])),"xval"],subset(df.sum,Country.Region==input$name[i])[nrow(subset(df.sum,Country.Region==input$name[i]))-1,"xval"],subset(df.sum,Country.Region==input$name[i])[nrow(subset(df.sum,Country.Region==input$name[i]))-2,"xval"],"Min","Max","Stdev.")
    
    if (as.numeric(max(df.sum$xval,na.rm=T))>1800){
      colnames(summary)=c("Series",as.character(as.Date(subset(df.sum,Country.Region==input$name[i])[nrow(subset(df.sum,Country.Region==input$name[i])),"xval"])),as.character(as.Date(subset(df.sum,Country.Region==input$name[i])[nrow(subset(df.sum,Country.Region==input$name[i]))-1,"xval"])),as.character(as.Date(subset(df.sum,Country.Region==input$name[i])[nrow(subset(df.sum,Country.Region==input$name[i]))-2,"xval"])),"Min","Max","Stdev.")
    }
    
    #colnames(summary)[1:4]=c("Series",as.Date(df.sum[nrow(df.sum),"date"]),df.sum[nrow(df.sum)-1,"date"],df.sum[nrow(df.sum)-2,"date"])
    #colnames(summary)[5:ncol(summary)]=c("Three Month Avg","Six Month Avg","One Year Avg",'Series Avg.',"Min","Max","Stdev.","25th Percentile","75th Percentile")
    summary

   
    
  },caption="Summary Statistics",caption.placement = getOption("xtable.caption.placement", "top"))
  
  output$corr <- renderTable({
    
    df.corr=data_input()
    df.corr=dcast(df.corr,xval~variable+Country.Region,value.var="value",mean)
    df.corr1=cor(na.omit(as.matrix(df.corr[,-c(1)])))
    rownames(df.corr1)=colnames(df.corr)[-c(1)]
    df.corr1
    
  },caption="Correlation Matrix",caption.placement = getOption("xtable.caption.placement", "top"),rownames = TRUE)
  
  output$table <- renderTable({
    df1=data_input()
    #df1$GameID=as.character(df1$GameID)
    df1=dcast(df1,xval~variable+Country.Region,value.var='value',mean)
    if (max(df1$xval>1000,na.rm=T)){
      df1$xval=as.character(as.Date(df1$xval))
    }
    df1
  })
  
  output$CovidData <- downloadHandler(
    filename = function() {
      paste('covid_data', 'csv', sep='.')
    },
    content = function(file) {
      
      write.csv(data_input(), file, row.names = FALSE)
    }
  )
  

 
 
  
}