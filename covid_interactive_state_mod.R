

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

sandbox2.UI <- function(id) {
  
  ns <- NS(id)
  
 
  
  #second tab
  
  tabPanel("State and Province Data",
           #customHeader(title = "Data 'Sandbox' Manipulator"),
           column(12,
                  sidebarLayout(
                    sidebarPanel(h3("State/Province-Level Data"),
                                 radioButtons(ns("chart_type"),
                                              label = "Select Chart Type", 
                                              choices = c("Bar Chart" = "bar","Line Chart"="line"),
                                              selected = "line"),
                                 selectInput(ns('ystat2'),
                                             label = 'Select statistic',
                                             choices=c("Confirmed Cases","Deaths","Recovered Cases"),
                                             selected="Confirmed Cases"
                                 ),
                                 radioButtons(ns("radio2"),
                                              label = "Select Data Transformation", 
                                              choices = c("Levels" = "levels", "Log Levels" = "log", "Change (Daily)" = "diff", "Percent Change (Daily)" = "qoq","Average Daily Percent Change (Rolling, 7-days)"="chg.avg"), #"Percent Change (10-day)" = "mom"
                                              selected = "log"),
                                 
                                 selectInput(ns('xchoice2'),
                                             label = 'Select time horizon',
                                             choices=c("Date","Since 1st Case","Since 100th Case","Since 10th Death"),
                                             selected="Since 100th Case"
                                 ),
                                 selectInput(ns("name2"),
                                             label = "Select State or Province:",
                                             choices = c(unique(covid.kaggle$Province.State)),
                                             selected = c("New York","Hubei","Illinois","California","Washington","Pennsylvania"),
                                             multiple = TRUE
                                 ),
                                 
                                 downloadButton(ns("CovidData.state"), label = "Download Data")
                                 
                    ),
                    
                    
                    
                    #Main panel for displaying outputs ----
                    mainPanel(
                      tabsetPanel(type = "tabs",
                                  tabPanel("Plot", plotlyOutput(ns("plot2"))),
                                  tabPanel("Summary", tableOutput(ns("summary2"))),
                                  tabPanel("Correlations", tableOutput(ns("corr2"))),
                                  tabPanel("Table", tableOutput(ns("table2")))),
                      
                      
                      p("Data is sourced from Johns Hopkins Univerity CSSE as posted on",
                        span(a("Github",href="https://github.com/CSSEGISandData/COVID-19")),
                        "and provided online through",
                        span(a("Kaggle",href="https://www.kaggle.com/sudalairajkumar/novel-corona-virus-2019-dataset")),
                        ". Also inspired by the Financial Times ",
                        span(a("dataviz",href="https://www.ft.com/coronavirus-latest")),
                        " on Covid-19.","Data is currently updated through",
                        span(as.character(format(as.Date(max(covid.kaggle$Date,na.rm=T)),"%B %d, %Y"))),"All code written by Michael Ng, available on",
                    span(a("Github",href="https://github.com/mjng93/covid_19")),".")
                      
                    )
                    
                  ) #,div(style = "width:5000px;height:10px")#style='padding:100px;'
           )
           
  )
  
  
}




sandbox.server2 <- function(input, output, session,data2){
 
  
  #------------#
  #Second chart#
  #------------#
  
  module_data2=data2
  
  data_input2 <- reactive({
    if (input$radio2=="levels"){
      module_data2.levels=module_data2[,c("Province.State",input$xchoice2,input$ystat2)]
      df2 <- melt(module_data2.levels,id.vars=c(input$xchoice2,"Province.State"))
      df2 <- df2[df2$Province.State %in% input$name2,]
      colnames(df2)=c("xval","Province.State","variable","value")
      df2$value <- as.numeric(gsub(Inf,NA,df2$value))
      df2$value <- as.numeric(gsub(-Inf,NA,df2$value))
      units="Levels"
    }
    
    if (input$radio2=="log"){
      module_data2.log=module_data2[,c("Province.State",input$xchoice2,input$ystat2)]
      module_data2.log[,input$ystat2]=log(module_data2.log[,input$ystat2])
      df2 <- melt(module_data2.log,id.vars=c(input$xchoice2,"Province.State"))
      df2 <- df2[df2$Province.State %in% input$name2,]
      colnames(df2)=c("xval","Province.State","variable","value")
      df2$value <- as.numeric(gsub(Inf,NA,df2$value))
      df2$value <- as.numeric(gsub(-Inf,NA,df2$value))
      units="Log Level"
    }
    
    
    if (input$radio2=="qoq"){
      module_data2.qoq=module_data2[,c("Province.State",input$xchoice2,input$ystat2)]
      
      
      module_data2.qoq=as.data.frame(group_by(module_data2.qoq,Province.State) %>% mutate(test=as.numeric(as.vector(Delt(get(input$ystat2),k=1)))*100))
      
      module_data2.qoq[,input$ystat2]=module_data2.qoq[,c("test")]
      module_data2.qoq=subset(module_data2.qoq,select=-c(test))
      
      
      df2 <- melt(module_data2.qoq,id.vars=c(input$xchoice2,"Province.State"))
      df2 <- df2[df2$Province.State %in% input$name2,]
      colnames(df2)=c("xval","Province.State","variable","value")
      df2$value <- as.numeric(gsub(Inf,NA,df2$value))
      df2$value <- as.numeric(gsub(-Inf,NA,df2$value))
      units="Percent Change"
    }
    
    if (input$radio2=="diff"){
      
      module_data2.diff=module_data2[,c("Province.State",input$xchoice2,input$ystat2)]
      
      
      module_data2.diff=as.data.frame(group_by(module_data2.diff,Province.State) %>% mutate(test=as.numeric(as.vector(c(NA,diff(get(input$ystat2),lag=1))))))
      
      module_data2.diff[,input$ystat2]=module_data2.diff[,c("test")]
      module_data2.diff=subset(module_data2.diff,select=-c(test))
      
      
      df2 <- melt(module_data2.diff,id.vars=c(input$xchoice2,"Province.State"))
      df2 <- df2[df2$Province.State %in% input$name2,]
      colnames(df2)=c("xval","Province.State","variable","value")
      df2$value <- as.numeric(gsub(Inf,NA,df2$value))
      df2$value <- as.numeric(gsub(-Inf,NA,df2$value))
      units="New Cases"
      
    }
    
    if (input$radio2=="mom"){
      module_data2.mom=module_data2[,c("Province.State",input$xchoice2,input$ystat2)]
      
      module_data2.mom=as.data.frame(group_by(module_data2.mom,Province.State) %>% mutate(test=as.numeric(as.vector(Delt(get(input$ystat2),k=7)))*100))
      
      module_data2.mom[,input$ystat2]=module_data2.mom[,c("test")]
      module_data2.mom$test=NULL
      
      df2 <- melt(module_data2.mom,id.vars=c(input$xchoice2,"Province.State"))
      df2 <- df2[df2$Province.State %in% input$name2,]
      colnames(df2)=c("xval","Province.State","variable","value")
      df2$value <- as.numeric(gsub(Inf,NA,df2$value))
      df2$value <- as.numeric(gsub(-Inf,NA,df2$value))
      print(tail(df2))
      
      units="Percent Change"
    }
    
    if (input$radio2=="chg.avg"){
      module_data2.chg.avg=module_data2[,c("Province.State",input$xchoice2,input$ystat2)]
      
      module_data2.chg.avg=as.data.frame(group_by(module_data2.chg.avg,Province.State) %>% mutate(test=rollapply(as.numeric(as.vector(Delt(get(input$ystat2),k=1)))*100,width=7,mean,fill=NA,align="right")))
      
      module_data2.chg.avg[,input$ystat2]=module_data2.chg.avg[,c("test")]
      module_data2.chg.avg=subset(module_data2.chg.avg,select=-c(test))
      
      df2 <- melt(module_data2.chg.avg,id.vars=c(input$xchoice2,"Province.State"))
      df2 <- df2[df2$Province.State %in% input$name2,]
      colnames(df2)=c("xval","Province.State","variable","value")
      df2$value <- as.numeric(gsub(Inf,NA,df2$value))
      df2$value <- as.numeric(gsub(-Inf,NA,df2$value))
      
      units="Percent Change"
    }
    
    # if (input$xval=="Since 1st Case" | input$xval=="Since 100th Case" | input$xval=="Since 10th Death"){
    #   df2=df2[df2$xval>=-1,]
    # }
    
    df2
    
  })
  
  
  output$plot2 <- renderPlotly({
    
    # plot <-   ggplot(data_input()) +              
    #   geom_line(aes(x = input$xchoice2, y = value, colour = Province.State)) + scale_colour_discrete(name2 = NULL) + labs(x = NULL, y = input$radio2, title = "COVID Data",cap=c()   + theme(legend.position = "bottom", legend.margin = margin(t = -.1, unit='cm')) + theme(panel.grid.minor.x=element_blank(), panel.grid.major.x=element_blank()) + theme(plot.margin=unit(c(.1,.1,.1,.1),"cm"))
    # 
    # print(plot)
    
    plot.data2=data_input2()
    if (min(plot.data2$xval,na.rm=T)<0){
      plot.data2=plot.data2[plot.data2$xval>=-1,]
    }
    
    if (input$chart_type=="line"){
    plot_ly(plot.data2, x = ~xval,y= ~value, color = ~Province.State, type = 'scatter', mode = 'lines') %>%
      layout(title = input$ystat,
             xaxis = list(title = "Days"),
             yaxis = list (title = units))
    }
    
    else if (input$chart_type=="bar"){
      plot_ly(plot.data2, x = ~xval,y= ~value, color = ~Province.State, type = 'bar') %>%
        layout(title = input$ystat,
               xaxis = list(title = "Days"),
               yaxis = list (title = units))
    }
    
    
    #,text = paste('Value:', value,'<br>Date: ', as.Date(date,format='%b-%Y'),  '<br>Variable: ', variable)
    
    #print(df.ggplot)
    
    
  }#,height=400,width=1000
  )
  
  output$summary2 <- renderTable({
    
    df2.sum=data_input2()
    #df2.sum=na.omit(df2.sum)
    
    summary=data.frame(series=numeric(length(input$name2)),l1=numeric(length(input$name2)),l2=numeric(length(input$name2)),l3=numeric(length(input$name2)),min=numeric(length(input$name2)),max=numeric(length(input$name2)),sd=numeric(length(input$name2)))
    
    for (i in 1:length(input$name2)){
      summary[i,'series']=input$name2[i]
      summary[i,'l1']=subset(df2.sum,Province.State==input$name2[i])[nrow(subset(df2.sum,Province.State==input$name2[i])),"value"]
      summary[i,'l2']=subset(df2.sum,Province.State==input$name2[i])[nrow(subset(df2.sum,Province.State==input$name2[i]))-1,"value"]
      summary[i,'l3']=subset(df2.sum,Province.State==input$name2[i])[nrow(subset(df2.sum,Province.State==input$name2[i]))-2,"value"]
      summary[i,'min']=min(subset(df2.sum,Province.State==input$name2[i])[,"value"],na.rm=T)
      summary[i,'max']=max(subset(df2.sum,Province.State==input$name2[i])[,"value"],na.rm=T)
      summary[i,'sd']=sd(subset(df2.sum,Province.State==input$name2[i])[,"value"],na.rm=T)
      
    }
    colnames(summary)=c("Series",subset(df2.sum,Province.State==input$name2[i])[nrow(subset(df2.sum,Province.State==input$name2[i])),"xval"],subset(df2.sum,Province.State==input$name2[i])[nrow(subset(df2.sum,Province.State==input$name2[i]))-1,"xval"],subset(df2.sum,Province.State==input$name2[i])[nrow(subset(df2.sum,Province.State==input$name2[i]))-2,"xval"],"Min","Max","Stdev.")
    
    if (as.numeric(max(df2.sum$xval,na.rm=T))>1800){
      colnames(summary)=c("Series",as.character(as.Date(subset(df2.sum,Province.State==input$name2[i])[nrow(subset(df2.sum,Province.State==input$name2[i])),"xval"])),as.character(as.Date(subset(df2.sum,Province.State==input$name2[i])[nrow(subset(df2.sum,Province.State==input$name2[i]))-1,"xval"])),as.character(as.Date(subset(df2.sum,Province.State==input$name2[i])[nrow(subset(df2.sum,Province.State==input$name2[i]))-2,"xval"])),"Min","Max","Stdev.")
    }
    
    
    summary
    
    
    
  },caption="Summary Statistics",caption.placement = getOption("xtable.caption.placement", "top"))
  
  output$corr2 <- renderTable({
    
    df2.corr=data_input2()
    df2.corr=dcast(df2.corr,xval~variable+Province.State,value.var="value",mean)
    df2.corr1=cor(na.omit(as.matrix(df2.corr[,-c(1)])))
    rownames(df2.corr1)=colnames(df2.corr)[-c(1)]
    df2.corr1
    
  },caption="Correlation Matrix",caption.placement = getOption("xtable.caption.placement", "top"),rownames = TRUE)
  
  output$table2 <- renderTable({
    df21=data_input2()
    #df21$GameID=as.character(df21$GameID)
    df21=dcast(df21,xval~variable+Province.State,value.var='value',mean)
    if (max(df21$xval>1000,na.rm=T)){
      df21$xval=as.character(as.Date(df21$xval))
    }
    df21
  })
  
  output$CovidData.state <- downloadHandler(
    filename = function() {
      paste('covid_data_state', 'csv', sep='.')
    },
    content = function(file) {
      
      write.csv(data_input2(), file, row.names = FALSE)
    }
  )
  
  
  
}