# Visualization of Journal information

##########===== ui =====##########
# load packages
library(DT)
library(shiny)
library(stringr)
library(magrittr)
library(data.table)
library(shinyWidgets)
library(showtext)

ui <- fluidPage(
  tags$head(tags$title("journal")),
  tags$head(tags$link(rel = "shortcut icon", href = "pmgh.ico")),
  titlePanel("学术期刊质量查询系统"),
  tabsetPanel(id = "mainPanel",
              type = "pills",
              tabPanel(title = "英文LetPub版",
                       sidebarLayout(
                         sidebarPanel(width=3, uiOutput("ui_sidebar1")),
                         mainPanel(uiOutput("ui_main1"))
                       )),
              tabPanel(title = "英文JCR版",
                       sidebarLayout(
                         sidebarPanel(width = 3, uiOutput("ui_sidebar2")),
                         mainPanel(uiOutput("ui_main2"))
                       )),
              tabPanel(title = "中文CNKI版",
                       sidebarLayout(
                         sidebarPanel(width = 3, uiOutput("ui_sidebar3")),
                         mainPanel(uiOutput("ui_main3"))
                       )),
              tabPanel(title = "已选项目",
                       uiOutput("ui_main_selected")
                       )), 
  
  # h3("期刊信息查询系统",align="center"),
  # tags$hr(), 
  # sidebarLayout(
  #   sidebarPanel(width=3, uiOutput("ui_sidebar")),
  #   mainPanel(uiOutput("ui_main"))
  # ),
  tags$hr(),
  tags$div(align = "center", 
           tags$p("\ua9 2021-2022, LIANG Chen, Institute of Mountain Hazards and Environment, CAS. All rights reserved.", style="height:8px"),
           actionLink(inputId = "", label = "Github", icon = icon("github"), onclick ="window.open('https://github.com/lcpmgh/Journal')"),
           tags$p("  ", style = "display:inline;white-space:pre"),
           tags$p("Email: lcpmgh@gmail.com", style="display:inline;white-space:pre"),  
           tags$div(align = "center",
                    tags$a("冀ICP备2022003075号", target="_blank", href="https://beian.miit.gov.cn", style="color:#06c; display:inline;"),
                    tags$p("  ", style = "display:inline;white-space:pre"),
                    tags$img(src="gaba.png"),
                    tags$a("川公网安备51010702002736", target="_blank", href="http://www.beian.gov.cn/portal/registerSystemInfo?recordcode=51010702002736", style="color:#06c; display:inline;")
           )
  )
)
