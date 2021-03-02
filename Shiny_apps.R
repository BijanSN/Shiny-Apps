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

# source("Function1.R")

#---- Load data -------------------------------------------

# 3 random stocks : Nvidia, Apple and Visa
getSymbols(c("NVDA", "AAPL", "V"))
# Get adjusted prices
prices.data <- merge.zoo(NVDA[,6], AAPL[,6], V[,6])

# Calculate returns
returns.data <- CalculateReturns(prices.data)
returns.data <- na.omit(returns.data)
returns.data <- fortify.zoo(returns.data)

returns.data %<>% 
  rename(
    NVDA = NVDA.Adjusted,
    AAPL = AAPL.Adjusted,
    V = V.Adjusted
  )

# or :
# data(EuStockMarkets)
# 
# Dax= EuStockMarkets[,"DAX"]
# Smi= EuStockMarkets[,"SMI"]
# Cac= EuStockMarkets[,"CAC"]

#---- UI --------------------------------------------------

ui<-dashboardPage(skin= "red",
  dashboardHeader(title= "Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Stocks", tabName = "stocks", icon = icon("signal"))
    )
  ),
  dashboardBody(
    tabItems(
      #Stocks Page
      tabItem("stocks",
        box(plotOutput("hist_plot"),width=8),
        box(
          selectInput("stocks","Stocks :",
                         c("NVDA","AAPL",
                           "V")),width = 4
        ),
        box(plotOutput("density_plot"),width=8)
      )
    )
  )
)

#------Server ----------------------------------------------

server <- function(input,output){

  # render Histogram 
  output$hist_plot <-renderPlot({
    hist(returns.data[[input$stocks]])
    # plot(returns.data[[input$stocks]])
  })
  
  # render Density
  output$density_plot <-renderPlot({
    plot(density(as.numeric(returns.data[[input$stocks]]), adjust = 1.0),col='blue')
  })
}

#---- Shiny  App -------

shinyApp(ui, server)


