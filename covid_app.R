library(shiny)
library(ggplot2)
library(reshape2)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(zoo)
library(quantmod)
library(TTR)
library(gridExtra)
library(grid)
library(DT)
library(kableExtra)
library(tables)
library(knitr)
library(rsconnect)
library(shinythemes)
library(shinydashboard)


source("covid_interactive_mod.R", local = TRUE) 
source("covid_interactive_state_mod.R", local = TRUE) 
source("covid_interactive_tests_mod.R", local = TRUE) 
source("covid_interactive_county_mod.R", local = TRUE) 
source("homepage.R", local = TRUE) 

ui <- shinyUI(
  fluidPage(theme=shinytheme('yeti'),
           
            
            tags$head(includeScript("google_analytics_covid1.js")),
            
            
            navbarPage("Covid-19",
                       tabPanel(title="Overview",
                                  sandbox5.UI(id="sandbox5")
                      ),
                       navbarMenu("Country Level Data",
                                  sandbox.UI(id="sandbox")
                       ),
                       navbarMenu("State/Province Level Data",
                                  sandbox2.UI(id="sandbox2")
                       ),
                       navbarMenu("Testing Data",
                                  sandbox3.UI(id="sandbox3")
                       )
                       ,
                       navbarMenu("US County Level Data",
                                  sandbox4.UI(id="sandbox4")
                       )
            )
  )
)


server <- function(input, output, session){
  
  callModule(sandbox.server,id="sandbox",data=covid.agg)
  callModule(sandbox.server2,id="sandbox2",data2=covid.kaggle)
  callModule(sandbox.server3,id="sandbox3",data3=covid.state)
  callModule(sandbox.server4,id="sandbox4",data4=covid.county)
  callModule(sandbox.server5,id="sandbox5",data5=covid.agg,data5a=covid.kaggle,data5b=covid.state,data5c=covid.county)
  
}

shinyApp(ui = ui, server = server)
