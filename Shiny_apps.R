######## setup #########

rm(list=ls())

library(shiny)
library(dplyr)
library(shinydashboard)
library(ggplot2)
library(PortfolioAnalytics)
library(quantmod)
library(PerformanceAnalytics)
library(zoo)
library(ROI)
library(ggplot2)
library(reshape2)
library(scales)
library(magrittr)

#---- Load data -------------------------------------------

source("Load_data.R")

#---- UI --------------------------------------------------

ui<-dashboardPage(#skin= "red",
    dashboardHeader(title= "Sales Dashboard"),
    
    dashboardSidebar(
      sidebarMenu(
        menuItem(strong("Data visualisation"),    
                 tabName = "Data_visualisation",
                 icon = icon("dashboard"))
      )
    ),
    
  dashboardBody(
    tabItems(
      tabItem(tabName = "Data_visualisation",
       column(
          width = 10,
          box(uiOutput("stock")),
          box(verbatimTextOutput("summary"))
        ),
       column(
          width = 10,
          box(plotOutput("hist_plot")),
          box(plotOutput("time_plot"))
       ),
        column(
         width = 10,
          box(plotOutput("regression_plot")),
          box(plotOutput("density_plot"))
        )
      ),
      tabItem(tabName = "Data_visualisation")
    )
  )
)

#------Server ----------------------------------------------

server <- function(input,output){

  output$stock <- renderUI({
    selectInput(
    inputId = "stock",
    label = "Stocks :",
    choices = returns.data %>%
          select(-1) %>% # remove time index from stock choices
          colnames())})
  
  # render Histogram 
  output$hist_plot <-renderPlot({
    returns.data %>% 
      ggplot(.,aes_string(x=input$stock))+
      stat_bin(bins=30)+
      geom_histogram()
  })
  
  # render Density
   output$density_plot <-renderPlot({
    returns.data %>% 
      ggplot(.,aes_string(x=input$stock))+
      geom_density()
})

 # render Time Series :
   output$time_plot <- renderPlot({
     merge.zoo(prices.data$Index,prices.data[[input$stock]]) %>% 
       ggplot(.,aes(x=Index,y=prices.data[[input$stock]])) +
       xlab("Time")+ ylab("Price")+
       geom_line()
   })
   
   #render regression against the benchmark
   output$regression_plot <- renderPlot({
     prices.data %>%
       ggplot(., aes_string(x=prices.data$SP500, y=input$stock)) +
       geom_point()+
       geom_smooth(method=lm, se=FALSE)+ xlab("S&P500")+ title("regression against the benchmark")
   })
   
 #Summary of returns :
     output$summary <-renderPrint(summary(returns.data[[input$stock]]))
  
}

#---- Shiny  App -------

shinyApp(ui, server)
