library(shiny)
library(plotly)
library(tidyverse)
library(shiny)
library(plotly)
library(tidyverse)
library(ggplot2)
library(shinydashboard)
library(shinyWidgets)
library(regclass)
library(ggpubr)
library(quantmod)
library(DT)
library(data.table)
library(fresh)
library(readr)
library(tsibble)
library(fpp3)
library(tsibble)
library(dplyr)
library(lubridate)
library(plotly)
library(shinythemes)
library(fpp3)

# cleaning the data
mr_beast_data <- read_csv("MrBeast.csv")
mr_beast_data <- read.csv("MrBeast.csv",skip = 1)
names(mr_beast_data) <- c("Date","Count")

mr_beast_data$Date <- ym(mr_beast_data$Date)
mr_beast_data$Count <- as.numeric(gsub("[<]", "", mr_beast_data$Count))
mr_beast_data

MRBEAST <- mr_beast_data %>%
  mutate(Date = yearmonth(Date)) %>%
  as_tsibble(index = "Date")

MRBEAST_fc <- MRBEAST[-c(1:144),]



# lets ride

ui <- navbarPage(
  title = "MrBeast Subscribtion Prediction",
  theme = shinytheme("darkly"),
  tabPanel(
    title = "Instructions",
    fluidRow(column(9, "Welcome to my time series analysis and forecasting app predicitng MrBeast's subscribers "),
             column(9, " - Wihtin this analysis, I am going to predict the overall subscriber count of the famous youtuber MrBeast."),
             column(9," - His peak was in Decemeber 2021, and did not start gaining subscribers until September 2018."),
             column(9, " - Once of his best viedos was when Jimmy recreated the Netflix show Squid Games, which resulted him gaining millions of followers."),
             column(9," - He also a gaming channel and philantropy channel which also rack in views and followers"),
             column(9,"In the navigation bar, listed is the..."),
             column(12," - gg_season"),
             column(12, " - Decomposistion"),
             column(12," - Simple Model"),
             column(12, " - Exponential Smoothing"),
             column(12," - ARIMA"),
             column(9," - In the Simple Model, the best fit for the model is the Seasonal Naive"),
             column(9," - From that visualization, it fits and predicts the subscribers for the youtuber MrBeast the best.")),
    
    actionButton("x","Youtube Link", onclick ="window.open('https://www.youtube.com/channel/UCX6OQ3DkcsbYNE6H8uQQuVA')", type= "Primary")
    

  ),
  tabPanel(
    title = "Over the Years",
    plotlyOutput("plot1"),
    mainPanel("The visualizationa above is the amount of Subsribers MrBeast has gained over the recent years.")
  ),
  tabPanel(
    title = "Seasonility",
    plotlyOutput("plot2")
  ),
  tabPanel(
    title = "Decompisition",
    plotOutput("plot3")
  ),
  tabPanel(
    title = "Simple Models",
    plotOutput("plot4")
  ),
  
  tabPanel(title = "Exponential Smoothing",
           plotOutput("plot5"),
           sidebarLayout(sidebarPanel(
             
             radioButtons(
               inputId = "selected",
               label = "Select plot",
               choices = c("Holts", "Holts/Winter"),
               selected = "Holts")),
             
             mainPanel("Above are the two Exponential Smoothing models, use the radio button to switch between models"))),


  
  tabPanel(title = "ARIMA",
    plotOutput("plot6"),
    sidebarLayout(sidebarPanel(
      
      radioButtons(
        inputId = "select",
        label = "Select plot",
        choices = c("Box-cox", "Auto", "Manual"),
        selected = "Box-cox")),
      
      mainPanel("Above are the 3 plots that that best represent my ARIMA model using Autoregession (lag), Differencing, and the Moving Average"))),
    
  )


dashboardSidebar()


server <- function(input, output, session) {
 
  output$plot1 <- renderPlotly({
      autoplot(MRBEAST, Count)
  })
  
  output$plot2 <- renderPlotly({
    gg_season(MRBEAST_fc,Count)
  })
  
  output$plot3 <- renderPlot({
    MRBEAST %>%
      model(
      classical_decomposition(Count, type = "multiplicative")) %>%
      components() %>%
      autoplot()
  })
  
output$plot4 <- renderPlot({
    train <- MRBEAST %>%
      filter_index("2015 Jan" ~ "2022 Oct")
    Count_fit <- train %>%
      model(
        Mean = MEAN(Count),
        `Naïve` = NAIVE(Count),
        `Seasonal naïve` = SNAIVE(Count),
        drift = RW(Count ~ drift()))
    Count_fc <- Count_fit %>% 
      forecast(h = 24)
    Count_fc %>%
      autoplot(train, level = NULL) +
      autolayer(
        filter_index(MRBEAST, "2015 Jan" ~ .),colour = "black") +
      labs( y = "Subs (millions)",
            title = "Forecasts monthly for MrBeast Subsribers") +
      guides(colour = guide_legend(title = "Forecast"))
  })
  
  output$plot5 <- renderPlot({    
    if (input$selected == "Holts") {
      fit <- MRBEAST %>%
        model(ETS(Count ~ error("M") + trend("N") + season("N")))
      fc <- fit %>%
        forecast(h=12) 
      
      fc %>%
        autoplot(MRBEAST)
      
      MRBEAST %>%
        model(
          `Holt's method` = ETS(Count ~ error("A") + trend("A") + season("N")),
          `Damped Holt's method` = ETS(Count ~ error("A") + trend("Ad", phi = 0.9) + season("N"))) %>%
        forecast(h = 12) %>%
        autoplot(MRBEAST, level = NULL) +
        labs(title = "MRBEAST subscribers", y = "Millions") +
        guides(colour = guide_legend(title = "Forecast"))
      
      
  }  else if (input$selected == "Holts/Winter") {
      
    fit <- MRBEAST %>%
      model(
        additive = ETS(Count ~ error("A") + trend("A") + season("A")),
        multiplicative = ETS(Count ~ error("M") + trend("A") + season("M")))
    
    fc <- fit %>% 
      forecast(h = "3 years")
    
    fc %>%
      autoplot(MRBEAST, level = NULL) +
      labs(title="MrBeast Subscribers", y="subs (millions)") +
      guides(colour = guide_legend(title = "Forecast"))
    
  }
})
  
  output$plot6 <- renderPlot({
    if (input$select == "Box-cox") {
      lambda <- MRBEAST %>%
        features(Count, features = guerrero) %>%
        pull(lambda_guerrero)
    
      autoplot(MRBEAST,box_cox(Count, lambda))
      
  }  else if (input$select == "Auto") {
      
      fit2 <-  MRBEAST %>%
        model(ARIMA(Count)) 
      
      fit2 %>%
        forecast(h= "2 years") %>%
        autoplot()
      
  } else if (input$select == "Manual") {
      
      fit3 <- MRBEAST %>%
        model(ARIMA(Count ~ pdq(0,1,2) + PDQ(1,1,0))) 
      fit3 %>%
        forecast(h= "2 years") %>%
        autoplot()
      }
  })  

}


shinyApp(ui, server)





