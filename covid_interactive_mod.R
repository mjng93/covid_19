

source("covid.R")

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
  
  tabPanel("Data Sandbox",
           #customHeader(title = "Data 'Sandbox' Manipulator"),
           column(12,
                  sidebarLayout(
                    sidebarPanel(h3("COVID-19 Data"),
                                 selectInput(ns('ystat'),
                                             label = 'Select statistic',
                                             choices=c("Confirmed Cases","Deaths","Recovered Cases"),
                                             selected="deaths"
                                 ),
                                 radioButtons(ns("radio"),
                                              label = "Select Data Transformation", 
                                              choices = c("Levels" = "levels", "Log Levels" = "log", "Change (Daily)" = "diff", "Percent Change (Daily)" = "qoq", "Percent Change (10-day)" = "mom","Average Daily Percent Change (Rolling, 7-days)"="chg.avg"),
                                              selected = "levels"),
                                 
                                 selectInput(ns('xchoice'),
                                             label = 'Select time horizon',
                                             choices=c("Date","Since 1st Case","Since 100th Case","Since 10th Death"),
                                             selected="Date"
                                 ),
                                 selectInput(ns("name"),
                                             label = "Select country:",
                                             choices = c(unique(covid.agg$Country.Region)),
                                             selected = c("US"),
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
                    span(a("Github",href="https://github.com/CSSEGISandData/COVID-19")),
                    "and provided online through",
                    span(a("Kaggle",href="https://www.kaggle.com/sudalairajkumar/novel-corona-virus-2019-dataset")),
                    ". All data is aggregated by Country/Region. Also inspired by the Financial Times ",
                    span(a("dataviz",href="https://www.ft.com/coronavirus-latest")),
                    " on Covid-19.","Data is currently updated through",
                    span(as.character(format(as.Date(max(covid.agg$Date,na.rm=T)),"%B %d, %Y"))),".")
                    
                    )
                    
                  ) #,div(style = "width:5000px;height:10px")#style='padding:100px;'
           )
           
  )
}




sandbox.server <- function(input, output, session,data,data2){
  
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
      module_data.mom=subset(module_data.mom,select=-c(test))
      
      df <- melt(module_data.mom,id.vars=c(input$xchoice,"Country.Region"))
      df <- df[df$Country.Region %in% input$name,]
      colnames(df)=c("xval","Country.Region","variable","value")
      df$value <- as.numeric(gsub(Inf,NA,df$value))
      df$value <- as.numeric(gsub(-Inf,NA,df$value))
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
  
  
  output$plot <- renderPlotly({
    
    # plot <-   ggplot(data_input()) +              
    #   geom_line(aes(x = input$xchoice, y = value, colour = Country.Region)) + scale_colour_discrete(name = NULL) + labs(x = NULL, y = input$radio, title = "COVID Data",cap=c()   + theme(legend.position = "bottom", legend.margin = margin(t = -.1, unit='cm')) + theme(panel.grid.minor.x=element_blank(), panel.grid.major.x=element_blank()) + theme(plot.margin=unit(c(.1,.1,.1,.1),"cm"))
    # 
    # print(plot)
    
    plot.data=data_input()
    if (min(plot.data$xval,na.rm=T)<0){
      plot.data=plot.data[plot.data$xval>=-1,]
    }
    
    
    plot_ly(plot.data, x = ~xval,y= ~value, color = ~Country.Region, type = 'scatter', mode = 'lines') %>%
      layout(title = "Covid Data",
             xaxis = list(title = "Days"),
             yaxis = list (title = units))
    
    
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
      paste('stats', 'csv', sep='.')
    },
    content = function(file) {
      
      write.csv(data_input(), file, row.names = FALSE)
    }
  )
  
 
 
  
}