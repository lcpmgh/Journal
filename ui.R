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
  ),
  tags$hr(),
  tags$div(align = "center", 
           tags$p("--- Designed by PMGH ---"),
           actionLink(inputId = "", label = "Github", icon = icon("github"), onclick ="window.open('https://github.com/lcpmgh/Journal')"),
           tags$p("  ", style = "display:inline;white-space:pre"),
           tags$p("Email: lcpmgh@gmail.com", style="display:inline;white-space:pre"),  
           tags$div(align = "center",
                    # tags$img(src="http://www.beian.gov.cn/portal/download"),
                    # tags$a("冀ICP备2022003075号", target="_blank", href="https://beian.miit.gov.cn", style="color:#06c; display:inline;"),
                    # tags$p("  ", style = "display:inline;white-space:pre"),
                    tags$a("冀ICP备2022003075号", target="_blank", href="http://beian.miit.gov.cn", style="color:#06c; display:inline;")
           )
  )
)
