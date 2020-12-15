

#source("covid.r")

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

sandbox3.UI <- function(id) {
  
  ns <- NS(id)
  
  tabPanel("State-Level Testing Data",
           #customHeader(title = "Data 'Sandbox' Manipulator"),
           column(12,
                  sidebarLayout(
                    sidebarPanel(h3("State-Level Testing Data"),
                                 
                                 radioButtons(ns("chart_type"),
                                              label = "Select Chart Type", 
                                              choices = c("Bar Chart" = "bar","Line Chart"="line"),
                                              selected = "line"),
                                 selectInput(ns('ystat'),
                                             label = 'Select statistic',
                                             choices=c("Positive Tests","Negative Tests","Pending Tests","Hospitalized Cases","Deaths","Total Tests - Encounters","Total Tests - People"),
                                             selected="Total Tests - Encounters"
                                 ),
                                 radioButtons(ns("radio"),
                                              label = "Select Data Transformation", 
                                              choices = c("Levels" = "levels","Rolling Average (7-day)"="levels.avg","Per Capita Levels" = "pc","Per Capita Daily Change"="pc.d","Average Per Capita Daily Change (Rolling, 7-days)"="pc.d.avg", "Log Levels" = "log", "Change (Daily)" = "diff", "Percent Change (Daily)" = "qoq","Average Daily Percent Change (Rolling, 7-days)"="chg.avg"), #"Percent Change (10-day)" = "mom"
                                              selected = "pc.d.avg"),
                                 
                                 selectInput(ns('xchoice'),
                                             label = 'Select time horizon',
                                             choices=c("Date","Since 1st Case","Since 100th Case","Since 10th Death"),
                                             selected="Date"
                                 ),
                                 selectInput(ns("name"),
                                             label = "Select State:",
                                             choices = c(unique(covid.state$state)),
                                             selected = c("US (Total)"),
                                             multiple = TRUE
                                 ),
                                 
                                 downloadButton(ns("CovidTestData"), label = "Download Data")
                                 
                    ),
                    
                    
                    
                    #Main panel for displaying outputs ----
                    mainPanel(
                      tabsetPanel(type = "tabs",
                                  tabPanel("Plot", plotlyOutput(ns("plot"))),
                                  tabPanel("Summary", tableOutput(ns("summary"))),
                                  tabPanel("Correlations", tableOutput(ns("corr"))),
                                  tabPanel("Table", tableOutput(ns("table")))),
                      
                      
                      p("Data is sourced from the COVID Tracking Project as posted ",
                        span(a("online",target="_blank",href="https://covidtracking.com/")),
                        ". Site currently run by Alexis Madrigal at the Atlantic. ",
                        "State population data is sourced from the ",
                        span(a("Census Bureu",target="_blank",href="https://www.census.gov/data/tables/time-series/demo/popest/2010s-state-total.html")),
                        ". Note that these data may not match with data from Johns Hopkins exactly, as they may be collected from different sources. Data is currently updated through",
                        span(as.character(format(as.Date(max(covid.state$Date,na.rm=T)),"%B %d, %Y"))),"All code written by Michael Ng, available on",
                        span(a("Github",target="_blank",href="https://github.com/mjng93/covid_19")),"."
                      )
                      
                    )
                    
                  ) #,div(style = "width:5000px;height:10px")#style='padding:100px;'
           )
           
  )
  
  
  
}




sandbox.server3 <- function(input, output, session,data3){
  
  module_data=as.data.frame(data3)
  
  data_input_tests <- reactive({
    if (input$radio=="levels"){
      module_data.levels=module_data[,c("state",input$xchoice,input$ystat)]
      df <- reshape2::melt(module_data.levels,id.vars=c(input$xchoice,"state"))
      df <- df[df$state %in% input$name,]
      colnames(df)=c("xval","state","variable","value")
      df$value <- as.numeric(gsub(Inf,NA,df$value))
      df$value <- as.numeric(gsub(-Inf,NA,df$value))
      units="Levels"
    }
    
    if (input$radio=="levels.avg"){
      module_data.levels.avg=module_data[,c("state",input$xchoice,input$ystat)]
      module_data.levels.avg[,input$ystat]=as.numeric(as.vector(zoo::rollapply(module_data.levels.avg[,input$ystat],width=7,mean,fill=NA,align="right")))
      df <- reshape2::melt(module_data.levels.avg,id.vars=c(input$xchoice,"state"))
      df <- df[df$state %in% input$name,]
      colnames(df)=c("xval","state","variable","value")
      df$value <- as.numeric(gsub(Inf,NA,df$value))
      df$value <- as.numeric(gsub(-Inf,NA,df$value))
      units="Levels"
    }
    
    if (input$radio=="pc"){
      module_data.pc=module_data[,c("state","state_population",input$xchoice,input$ystat)]
      module_data.pc[,input$ystat]=(module_data.pc[,input$ystat]/module_data.pc[,"state_population"])*100
      module_data.pc$state_population=NULL
      df <- reshape2::melt(module_data.pc,id.vars=c(input$xchoice,"state"))
      df <- df[df$state %in% input$name,]
      colnames(df)=c("xval","state","variable","value")
      df$value <- as.numeric(gsub(Inf,NA,df$value))
      df$value <- as.numeric(gsub(-Inf,NA,df$value))
      units="Percent"
    }
    
    if (input$radio=="log.pc"){
      module_data.log.pc=module_data[,c("state","state_population",input$xchoice,input$ystat)]
      module_data.log.pc[,input$ystat]=log(1+(module_data.log.pc[,input$ystat]/module_data.log.pc[,"state_population"])*100)
      module_data.log.pc$state_population=NULL
      df <- reshape2::melt(module_data.log.pc,id.vars=c(input$xchoice,"state"))
      df <- df[df$state %in% input$name,]
      colnames(df)=c("xval","state","variable","value")
      df$value <- as.numeric(gsub(Inf,NA,df$value))
      df$value <- as.numeric(gsub(-Inf,NA,df$value))
      units="Percent"
    }
    
    if (input$radio=="total.share"){
      module_data.total.share=module_data[,c("state","Total Tests - Encounters",input$xchoice,input$ystat)]
      module_data.total.share[,input$ystat]=(module_data.total.share[,input$ystat]/module_data.total.share[,"Total Tests - Encounters"])*100
      module_data.total.share[,c("Total Tests - Encounters")]=NULL
      df <- reshape2::melt(module_data.total.share,id.vars=c(input$xchoice,"state"))
      df <- df[df$state %in% input$name,]
      colnames(df)=c("xval","state","variable","value")
      df$value <- as.numeric(gsub(Inf,NA,df$value))
      df$value <- as.numeric(gsub(-Inf,NA,df$value))
      units="Percent"
    }
    
    if (input$radio=="total.share.new"){
      
      module_data.total.share.new=module_data[,c("state","Total Tests - Encounters",input$xchoice,input$ystat)]
      
      module_data.total.share.new=as.data.frame(group_by(module_data.total.share.new,state) %>% mutate(totals=as.numeric(as.vector(c(NA,diff(`Total Tests - Encounters`,lag=1))))))
      
      
      module_data.total.share.new[,"Total Tests - Encounters"]=module_data.total.share.new[,c("totals")]
      module_data.total.share.new=subset(module_data.total.share.new,select=-c(totals))
      module_data.total.share.new[,input$ystat]=(module_data.total.share.new[,input$ystat]/module_data.total.share.new[,"Total Tests - Encounters"])*100
      module_data.total.share.new[,c("Total Tests - Encounters")]=NULL
      df <- reshape2::melt(module_data.total.share.new,id.vars=c(input$xchoice,"state"))
      df <- df[df$state %in% input$name,]
      colnames(df)=c("xval","state","variable","value")
      df$value <- as.numeric(gsub(Inf,NA,df$value))
      df$value <- as.numeric(gsub(-Inf,NA,df$value))
      units="Percent"
    }
    
    if (input$radio=="total.share.new.avg"){
      
      module_data.total.share.new=module_data[,c("state","Total Tests - Encounters",input$xchoice,input$ystat)]
      
      module_data.total.share.new=as.data.frame(group_by(module_data.total.share.new,state) %>% mutate(totals=as.numeric(as.vector(c(NA,diff(`Total Tests - Encounters`,lag=1))))))
      
      
      module_data.total.share.new[,"Total Tests - Encounters"]=module_data.total.share.new[,c("totals")]
      module_data.total.share.new=subset(module_data.total.share.new,select=-c(totals))
      module_data.total.share.new[,input$ystat]=(module_data.total.share.new[,input$ystat]/module_data.total.share.new[,"Total Tests - Encounters"])*100
      
      module_data.total.share.new[,input$ystat]=as.numeric(as.vector(zoo::rollapply(module_data.total.share.new[,input$ystat],width=7,mean,fill=NA,align="right")))
      
      module_data.total.share.new[,c("Total Tests - Encounters")]=NULL
      df <- reshape2::melt(module_data.total.share.new,id.vars=c(input$xchoice,"state"))
      df <- df[df$state %in% input$name,]
      colnames(df)=c("xval","state","variable","value")
      df$value <- as.numeric(gsub(Inf,NA,df$value))
      df$value <- as.numeric(gsub(-Inf,NA,df$value))
      units="Percent"
    }
    
    if (input$radio=="log"){
      module_data.log=module_data[,c("state",input$xchoice,input$ystat)]
      module_data.log[,input$ystat]=log(module_data.log[,input$ystat])
      df <- reshape2::melt(module_data.log,id.vars=c(input$xchoice,"state"))
      df <- df[df$state %in% input$name,]
      colnames(df)=c("xval","state","variable","value")
      df$value <- as.numeric(gsub(Inf,NA,df$value))
      df$value <- as.numeric(gsub(-Inf,NA,df$value))
      units="Log Level"
    }
    
    if (input$radio=="pc.d"){
      module_data.pc.d=module_data[,c("state","state_population",input$xchoice,input$ystat)]
      module_data.pc.d[,input$ystat]=(module_data.pc.d[,input$ystat]/module_data.pc.d[,"state_population"])*100
      module_data.pc.d=as.data.frame(group_by(module_data.pc.d,state) %>% mutate(test=as.numeric(as.vector(c(NA,diff(get(input$ystat),lag=1))))))
      module_data.pc.d[,input$ystat]=module_data.pc.d[,c("test")]
      module_data.pc.d=subset(module_data.pc.d,select=-c(test,state_population))
      df <- reshape2::melt(module_data.pc.d,id.vars=c(input$xchoice,"state"))
      df <- df[df$state %in% input$name,]
      colnames(df)=c("xval","state","variable","value")
      df$value <- as.numeric(gsub(Inf,NA,df$value))
      df$value <- as.numeric(gsub(-Inf,NA,df$value))
      units="Percentage Points"
    }
    
    if (input$radio=="pc.d.avg"){
      module_data.pc.d.avg=module_data[,c("state","state_population",input$xchoice,input$ystat)]
      module_data.pc.d.avg[,input$ystat]=(module_data.pc.d.avg[,input$ystat]/module_data.pc.d.avg[,"state_population"])*100
      module_data.pc.d.avg=as.data.frame(group_by(module_data.pc.d.avg,state) %>% mutate(test=rollapply(as.numeric(as.vector(c(NA,diff(get(input$ystat),lag=1)))),width=7,mean,fill=NA,align="right")))
      
      module_data.pc.d.avg[,input$ystat]=module_data.pc.d.avg[,c("test")]
      module_data.pc.d.avg=subset(module_data.pc.d.avg,select=-c(test,state_population))
      
      df <- reshape2::melt(module_data.pc.d.avg,id.vars=c(input$xchoice,"state"))
      df <- df[df$state %in% input$name,]
      colnames(df)=c("xval","state","variable","value")
      df$value <- as.numeric(gsub(Inf,NA,df$value))
      df$value <- as.numeric(gsub(-Inf,NA,df$value))
      units="Percentage Points"
    }
    
    
    if (input$radio=="qoq"){
      module_data.qoq=module_data[,c("state",input$xchoice,input$ystat)]
      
      module_data.qoq=as.data.frame(group_by(module_data.qoq,state) %>% mutate(test=as.numeric(as.vector(Delt(get(input$ystat),k=1)))*100))
      
      module_data.qoq[,input$ystat]=module_data.qoq[,c("test")]
      module_data.qoq=subset(module_data.qoq,select=-c(test))
      
      df <- reshape2::melt(module_data.qoq,id.vars=c(input$xchoice,"state"))
      df <- df[df$state %in% input$name,]
      colnames(df)=c("xval","state","variable","value")
      df$value <- as.numeric(gsub(Inf,NA,df$value))
      df$value <- as.numeric(gsub(-Inf,NA,df$value))
      units="Percent Change"
    }
    
    if (input$radio=="diff"){
      
      module_data.diff=module_data[,c("state",input$xchoice,input$ystat)]
      
      
      module_data.diff=as.data.frame(group_by(module_data.diff,state) %>% mutate(test=as.numeric(as.vector(c(NA,diff(get(input$ystat),lag=1))))))
      
      module_data.diff[,input$ystat]=module_data.diff[,c("test")]
      module_data.diff=subset(module_data.diff,select=-c(test))
      
      
      df <- reshape2::melt(module_data.diff,id.vars=c(input$xchoice,"state"))
      df <- df[df$state %in% input$name,]
      colnames(df)=c("xval","state","variable","value")
      df$value <- as.numeric(gsub(Inf,NA,df$value))
      df$value <- as.numeric(gsub(-Inf,NA,df$value))
      units="New Cases"
      
    }
    
    if (input$radio=="mom"){
      module_data.mom=module_data[,c("state",input$xchoice,input$ystat)]
      
      module_data.mom=as.data.frame(group_by(module_data.mom,state) %>% mutate(test=as.numeric(as.vector(Delt(get(input$ystat),k=7)))*100))
      
      module_data.mom[,input$ystat]=module_data.mom[,c("test")]
      module_data.mom$test=NULL
      
      df <- reshape2::melt(module_data.mom,id.vars=c(input$xchoice,"state"))
      df <- df[df$state %in% input$name,]
      colnames(df)=c("xval","state","variable","value")
      df$value <- as.numeric(gsub(Inf,NA,df$value))
      df$value <- as.numeric(gsub(-Inf,NA,df$value))
      print(tail(df))
      
      units="Percent Change"
    }
    
    if (input$radio=="chg.avg"){
      module_data.chg.avg=module_data[,c("state",input$xchoice,input$ystat)]
      
      module_data.chg.avg=as.data.frame(group_by(module_data.chg.avg,state) %>% mutate(test=rollapply(as.numeric(as.vector(Delt(get(input$ystat),k=1)))*100,width=7,mean,fill=NA,align="right")))
      
      module_data.chg.avg[,input$ystat]=module_data.chg.avg[,c("test")]
      module_data.chg.avg=subset(module_data.chg.avg,select=-c(test))
      
      df <- reshape2::melt(module_data.chg.avg,id.vars=c(input$xchoice,"state"))
      df <- df[df$state %in% input$name,]
      colnames(df)=c("xval","state","variable","value")
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
    if (input$radio=="total.share"){units="Percent (1=1%)"}
    if (input$radio=="total.share.new"){units="Percent (1=1%)"}
    if (input$radio=="log.pc"){units="Log Points: computed as log(1 + Per Capita Level in decimals)"}
    if (input$radio=="log"){units="Log Points"}
    if (input$radio=="diff"){units="New Tests"}
    if (input$radio=="qoq"){units="Percent Change (1=1%)"}
    if (input$radio=="pc.d"){units="Percentage Points"}
    if (input$radio=="pc.d.avg"){units="Percentage Points"}
    if (input$radio=="chg.avg"){units="Average Daily Percent Change (1=1%)"}
    
    units
    
  })
  
  output$plot <- renderPlotly({
    
    plot.data=data_input_tests()
    
    if (min(plot.data$xval,na.rm=T)<0){
      plot.data=plot.data[plot.data$xval>=-1,]
    }
    
    if (input$chart_type=="bar"){
    plot_ly(plot.data, x = ~xval,y= ~value, color = ~state, type = 'bar') %>%
      layout(title = input$ystat,
             xaxis = list(title = "Days"),
             yaxis = list (title = unit_input()))
    }
    
    else if (input$chart_type=="line"){
      plot_ly(plot.data, x = ~xval,y= ~value, color = ~state, type = 'scatter', mode = 'lines') %>%
        layout(title = input$ystat,
               xaxis = list(title = "Days"),
               yaxis = list (title = unit_input()))
    }
    
    #,text = paste('Value:', value,'<br>Date: ', as.Date(date,format='%b-%Y'),  '<br>Variable: ', variable)
    
    #print(df.ggplot)
    
    
  }#,height=400,width=1000
  )
  
  output$summary <- renderTable({
    
    df.sum=data_input_tests()
    #df.sum=na.omit(df.sum)
    
    summary=data.frame(series=numeric(length(input$name)),l1=numeric(length(input$name)),l2=numeric(length(input$name)),l3=numeric(length(input$name)),min=numeric(length(input$name)),max=numeric(length(input$name)),sd=numeric(length(input$name)))
    
    for (i in 1:length(input$name)){
      summary[i,'series']=input$name[i]
      summary[i,'l1']=subset(df.sum,state==input$name[i])[nrow(subset(df.sum,state==input$name[i])),"value"]
      summary[i,'l2']=subset(df.sum,state==input$name[i])[nrow(subset(df.sum,state==input$name[i]))-1,"value"]
      summary[i,'l3']=subset(df.sum,state==input$name[i])[nrow(subset(df.sum,state==input$name[i]))-2,"value"]
      summary[i,'min']=min(subset(df.sum,state==input$name[i])[,"value"],na.rm=T)
      summary[i,'max']=max(subset(df.sum,state==input$name[i])[,"value"],na.rm=T)
      summary[i,'sd']=sd(subset(df.sum,state==input$name[i])[,"value"],na.rm=T)
      
    }
    colnames(summary)=c("Series",subset(df.sum,state==input$name[i])[nrow(subset(df.sum,state==input$name[i])),"xval"],subset(df.sum,state==input$name[i])[nrow(subset(df.sum,state==input$name[i]))-1,"xval"],subset(df.sum,state==input$name[i])[nrow(subset(df.sum,state==input$name[i]))-2,"xval"],"Min","Max","Stdev.")
    
    if (as.numeric(max(df.sum$xval,na.rm=T))>1800){
      colnames(summary)=c("Series",as.character(as.Date(subset(df.sum,state==input$name[i])[nrow(subset(df.sum,state==input$name[i])),"xval"])),as.character(as.Date(subset(df.sum,state==input$name[i])[nrow(subset(df.sum,state==input$name[i]))-1,"xval"])),as.character(as.Date(subset(df.sum,state==input$name[i])[nrow(subset(df.sum,state==input$name[i]))-2,"xval"])),"Min","Max","Stdev.")
    }
    
    #colnames(summary)[1:4]=c("Series",as.Date(df.sum[nrow(df.sum),"date"]),df.sum[nrow(df.sum)-1,"date"],df.sum[nrow(df.sum)-2,"date"])
    #colnames(summary)[5:ncol(summary)]=c("Three Month Avg","Six Month Avg","One Year Avg",'Series Avg.',"Min","Max","Stdev.","25th Percentile","75th Percentile")
    summary
    
    
    
  },caption="Summary Statistics",caption.placement = getOption("xtable.caption.placement", "top"))
  
  output$corr <- renderTable({
    
    df.corr=data_input_tests()
    df.corr=dcast(df.corr,xval~variable+state,value.var="value",mean)
    df.corr1=cor(na.omit(as.matrix(df.corr[,-c(1)])))
    rownames(df.corr1)=colnames(df.corr)[-c(1)]
    df.corr1
    
  },caption="Correlation Matrix",caption.placement = getOption("xtable.caption.placement", "top"),rownames = TRUE)
  
  output$table <- renderTable({
    df1=data_input_tests()
    #df1$GameID=as.character(df1$GameID)
    df1=dcast(df1,xval~variable+state,value.var='value',mean)
    if (max(df1$xval>1000,na.rm=T)){
      df1$xval=as.character(as.Date(df1$xval))
    }
    df1
  })
  
  output$CovidTestData <- downloadHandler(
    filename = function() {
      paste('covid_data_tests', 'csv', sep='.')
    },
    content = function(file) {
      
      write.csv(data_input_tests(), file, row.names = FALSE)
    }
  )
  
  
  
  
  
}