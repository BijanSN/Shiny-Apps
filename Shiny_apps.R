######## setup #########

rm(list=ls())

library(shiny)
library(dplyr)
library(shinydashboard)
library(ggplot2)

#---- UI ----

ui<-dashboardPage(skin = "red",
  dashboardHeader(title= "Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Iris", tabName = "iris", icon = icon("tree")),
      menuItem("Cars", tabName = "cars", icon = icon("car"))
    )
  ),
  dashboardBody(
    tabItems(
      #Iris Page
      tabItem("iris",
        box(plotOutput("corrolation_plot"),width=8),
        box(
          selectInput("features","Features :",
                      c("Sepal.Width","Petal.Length",
                        "Petal.Width")),width = 4
        )
      ),
      #Cars Page
      tabItem("cars",
              fluidPage(
                h1("Cars")
              )
      )
    )
  )
)

#------Server -----

server <- function(input,output){
  output$corrolation_plot <- renderPlot({
    plot(iris$Sepal.Length,iris[[input$features]],
    xlab="Sepal length", ylab ="Features")
  })
}

#---- Shiny  App -------

shinyApp(ui, server)





