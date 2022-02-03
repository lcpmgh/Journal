# Visualization of Journal information

##########===== ui =====##########
# load packages
library(DT)
library(shiny)
library(dplyr)
library(magrittr)
library(data.table)
library(shinyWidgets)
library(showtext)

ui <- fluidPage(
  h3("期刊信息查询系统",align="center"),
  tags$hr(), 
  sidebarLayout(
    sidebarPanel(width=3, uiOutput("ui_sidebar")),
    mainPanel(uiOutput("ui_main"))
  )
)
