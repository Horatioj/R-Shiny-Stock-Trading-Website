# Load packages ----
library(shinydashboard)
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(lubridate)
library(shinydashboardPlus)
library(quantmod)
library(PerformanceAnalytics)
library(reshape2)
library(scales)
library(DT)
options(scipen = 999)

# Source helpers ----
source("strategies.R")
# strategies
strategies <- c("SMA", "ARBR", "BBANDS", "CCI", "DMI", "MACD", "OBV", "PVT", "ROC", 
                "RSI", "SAR", "SMI", "WPR", "DMISMI","DMIMACD", "SMIRSI", "BBRSI", "MACDCCI",
                "MACDSMI", "MACDRSI", "MACDSAR", "OBVMACD", "OBVSMA", "PVTSMA", "PVTMACD", "PVTSTC")

# User interface ----
## 1. header -------------------------------
header <- 
  dashboardHeader( title = HTML("Portfolio Performance"), 
                   disable = FALSE, 
                   titleWidth  = 350,
                   dropdownMenu( type = 'message',
                                       #customSentence = customSentence,
                                       messageItem(
                                         from = "horatioj16@outlook.com",#'Feedback and suggestions',
                                         message =  "",#paste0("horatioj16@outlook.com" ),
                                         icon = icon("envelope"),
                                         href = "mailto:horatioj16@outlook.com"
                                       ),
                                       icon = icon('comment')
                   ),
                   dropdownMenu( type = 'message',
                                       #customSentence = customSentence_share,
                                       icon = icon("share-alt"),
                                       messageItem(
                                         from = 'Twitter',
                                         message = "",
                                         icon = icon("twitter"),
                                         href = "https://twitter.com/intent/tweet?url=http%3A%2F%2Fum.edu.mo&text=The%20Investart%20Trading%20System"
                                       ),
                                       messageItem(
                                         from = 'Facebook',
                                         message = "",
                                         icon = icon("facebook"),
                                         href = "https://www.facebook.com/sharer/sharer.php?u=http%3A%2F%2Fum.edu.mo"
                                       ),
                                       messageItem(
                                         from = 'Google+',
                                         message = "",
                                         icon = icon("google-plus"),
                                         href = "https://plus.google.com/share?url=http%3A%2F%2um.edu.mo"
                                       ),
                                       messageItem(
                                         from = 'Sina Weibo',
                                         message = "",
                                         icon = icon("weibo"),
                                         href = "http://service.weibo.com/share/share.php?url=http://example.com&appkey=&title=The%20Investart%20Trading%20System%20http%3A%2F%2Fum.edu.mo&pic=&ralateUid=&language=zh_cn"
                                       ),
                                       messageItem(
                                         from = 'Pinterest',
                                         message = "",
                                         icon = icon("pinterest-p"),
                                         href = "http://pinterest.com/pin/create/button/?url=http%3A%2F%2Fum.edu.mo&media=&description=The%20Investart%20Trading%20System"
                                       ),
                                       messageItem(
                                         from = 'LinkedIn',
                                         message = "",
                                         icon = icon("linkedin"),
                                         href = "http://www.linkedin.com/shareArticle?mini=true&url=http%3A%2F%2Fum.edu.motitle=The%20Investart%20Trading%20System"
                                       ),
                                       messageItem(
                                         from = 'Tumblr',
                                         message = "",
                                         icon = icon("tumblr"),
                                         href = "http://www.tumblr.com/share?v=3&u=http%3A%2F%2Fum.edu.mo&t=The%20Investart%20Trading%20System"
                                       )
                   )
                   
  )

header$children[[2]]$children[[2]] <- header$children[[2]]$children[[1]]
header$children[[2]]$children[[1]] <- tags$a(href='http://www.um.edu.mo',
                                             tags$img(src='https://github.com/Horatioj/stock_trading_strategy_in_R/raw/main/logo.png')) #, height = '40', width='40', align = 'left') #,height='67',width='228.6', align = 'left'


## 2. sidebar -------------------------------------------
sidebar <- 
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "Stock Information",
        tabName = "description",
        width = 220,
        selectizeInput("market", label = "Select Market", choices = c("China", "United States"), 
                       options = list(placeholder = 'Type to select a market', onInitialize = I('function() { this.setValue(""); }'))),
        conditionalPanel('input.market == "China"', 
                         selectInput('stock', 'Stock',  choices = china_market),
                         br(),
                         dateRangeInput("dates1", "Date range", start = "2013-01-01", end = as.character(Sys.Date())),
                         selectInput('stra1', 'Strategy',  choices = strategies, selected = "MACD"),
                         div( 
                           offset = 1,
                           downloadButton(outputId = "downloadData1","Download CSV"))
        ),
        
        conditionalPanel('input.market == "United States"', 
                         selectInput('stock1', 'Stock',  choices = us_market),
                         br(),
                         dateRangeInput("dates2", "Date range", start = "2013-01-01", end = as.character(Sys.Date())),
                         selectInput('stra2', 'Strategy',  choices = strategies, selected = "MACD"),
                         downloadButton(outputId = "downloadData2","Download CSV"))
        
      ),
      menuItem("Indicator", tabName = "indicator")
    )
  )

## 3. body ----------------------------------------------
body <-
dashboardBody(
  tabsetPanel(
    tabPanel("Graph",
           fluidRow(
             plotly::plotlyOutput("his_plot")),
             plotOutput("regraph"),
             plotly::plotlyOutput("retplot")),
    tabPanel("Statistic",
             fluidRow(
               column(8,
                      dataTableOutput("retable")),
               column(4,
                      dataTableOutput("retable2")
               )
             )
    )
  )
)
# body <-
#   dashboardBody(
#     tabItems(
#       tabItem("description",
#               fluidRow(
#                 column(8,
#                        # plotOutput("his_plot")),
#                        plotly::plotlyOutput("his_plot")),
#                 column(4,
#                        dataTableOutput("retable2"))
#               ),
#           
#               br(),
#               
#               fluidRow(
#                 column(12,
#                        plotOutput("regraph")),
#                 
#                 br(),
#                 
#                 column(4,
#                        dataTableOutput("retable"))
#               )
#       ),
#       tabItem("indicator")
#     )
#   )
# body <-
#   dashboardBody(
#     tabItems(
#       tabItem(tabName = "indicator",
#               h2("Indicator")),
#       tabItem("description",
#               fluidRow(
#                 box(width = 8,
#                     collapsible = TRUE,
#                     plotly::plotlyOutput("his_plot")),
#                 box(width = 4,
#                     dataTableOutput("retable2")),
#                 br(),
#               ),
# 
# 
#               fluidRow(
#                 column(12,
#                        plotOutput("regraph")
#                 ),
# 
#                 br(),
# 
#                 column(4,
#                        dataTableOutput("retable")
#                 )
#               )
#       )
#     )
#   )
# https://blog.csdn.net/kMD8d5R/article/details/88859130

##
ui <- 
  dashboardPage(skin = "red", header, sidebar, body, controlbar = dashboardControlbar(skinSelector()))
