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

ui <- shinyUI(
  fluidPage(theme=shinytheme('spacelab'),
            
            
            navbarPage("Covid-19",
                       navbarMenu("Research",
                                  sandbox.UI(id="sandbox")
                       )
            )
  )
)


server <- function(input, output, session){
  
  callModule(sandbox.server,id="sandbox",data=covid.agg)
  
}

shinyApp(ui = ui, server = server)
