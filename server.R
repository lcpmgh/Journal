##########===== server =====##########
server <- function(input, output) {
  #### 0. Preparation ####
  
  showtext_auto()
  # font_add("msyh","/opt/shiny-server/samples/sample-apps/journalinfo/msyh.ttc")
  # font_add("msyh","/lome/lc/journalinfo/msyh.ttc")
  font_add("msyh", "./msyh.ttc")
  
  #### 1. Load dataset ####
  # jdata <- fread('https://raw.githubusercontent.com/lcpmgh/Journal/master/Journal.csv', stringsAsFactors=F)
  # jdata <- fread('/home/lc/journalinfo/Journal.csv', stringsAsFactors=F, encoding = "UTF-8")
  jdata <- fread('./journal2018.csv', stringsAsFactors=F, encoding = "UTF-8")
  jdata2022 <- fread('./journal2022.csv', stringsAsFactors=F, encoding = "UTF-8")
  jdatacate0 <- jdata2022$category
  jdatacate <- fread("./journal2022category.csv", header = T, sep = ",")$cate
  Items <- names(jdata)
  Items2 <- names(jdata2022)
  
  #### 2. Sidebar1 ####
  output$ui_sidebar1 <- renderUI({
    tagList(
      pickerInput(inputId = "inp_0",
                  label = "选择要显示的条目", 
                  choices = Items,
                  selected = Items,
                  options = list(`selected-text-format` = "count > 3", `actions-box` = TRUE),
                  multiple = TRUE),
      h4(strong("筛选项目")),
      checkboxGroupButtons(inputId = "inp_1",
                           label = "1. 收录情况",
                           choices = c("SCI", "SCIE", "SCI/SCIE", "无数据"),
                           status = "primary",
                           checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove",lib = "glyphicon")), size = 'xs'),
      checkboxGroupButtons(inputId = "inp_2",
                           label = "2. 中科院分区",
                           choices = c("1区", "2区", "3区", "4区", "未录"),
                           status = "primary",
                           checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove",lib = "glyphicon")), size = 'xs'),
      pickerInput(inputId = "inp_3",
                  label = "3. 分类", 
                  choices = sort(unique(jdata$Category)),
                  options = list(`selected-text-format` = "count > 3", `actions-box` = TRUE), multiple = TRUE),
      pickerInput(inputId = "inp_4",
                  label = "4. 学科", 
                  choices = sort(unique(jdata$Discipline)),
                  options = list(`selected-text-format` = "count > 3", `actions-box` = TRUE), multiple = TRUE),
      checkboxGroupButtons(inputId = "inp_5",
                           label = "5. Top期刊",
                           choices = c("是", "否", "无数据"),
                           status = "primary",
                           checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove",lib = "glyphicon")), size = 'xs'),
      checkboxGroupButtons(inputId = "inp_6",
                           label = "6. 综述期刊",
                           choices = c("是", "否", "无数据"),
                           status = "primary",
                           checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove",lib = "glyphicon")), size = 'xs'),
      pickerInput(inputId = "inp_7",
                  label = "7. 出版周期", 
                  choices = sort(unique(jdata$PublicationCycle)),
                  options = list(`selected-text-format`="count > 3",`actions-box`=TRUE), multiple = TRUE),
      pickerInput(inputId = "inp_8",
                  label = "8. 出版地区", 
                  choices = sort(unique(jdata$Region)),
                  options = list(`selected-text-format`="count > 3",`actions-box`=TRUE), multiple = TRUE)
    )
  })
  
  #### 3. Main1 ####
  output$ui_main1    <- renderUI({
    tagList(
      h3("2018年度期刊信息表"),
      tags$hr(),
      DT::dataTableOutput("table")
      )})
  output$table <- DT::renderDataTable({
    if(length(input$inp_0) != 0) input_0 <- input$inp_0 else input_0 <- Items
    if(length(input$inp_1) != 0) input_1 <- input$inp_1 else input_1 <- unique(jdata$IsSCI)
    if(length(input$inp_2) != 0) input_2 <- input$inp_2 else input_2 <- unique(jdata$CASRanking)
    if(length(input$inp_3) != 0) input_3 <- input$inp_3 else input_3 <- unique(jdata$Category)
    if(length(input$inp_4) != 0) input_4 <- input$inp_4 else input_4 <- unique(jdata$Discipline)
    if(length(input$inp_5) != 0) input_5 <- input$inp_5 else input_5 <- unique(jdata$IsTop)
    if(length(input$inp_6) != 0) input_6 <- input$inp_6 else input_6 <- unique(jdata$IsReview)
    if(length(input$inp_7) != 0) input_7 <- input$inp_7 else input_7 <- unique(jdata$PublicationCycle)
    if(length(input$inp_8) != 0) input_8 <- input$inp_8 else input_8 <- unique(jdata$Region)
    DT::datatable(jdata[IsSCI %in% input_1 &
                          CASRanking %in% input_2 &
                          Category %in% input_3 &
                          Discipline %in% input_4 &
                          IsTop %in% input_5 &
                          IsReview %in% input_6 &
                          PublicationCycle %in% input_7 &
                          Region %in% input_8, .SD, .SDcols = input_0], 
                  options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE))
    
  })

  ########################
  #### 4. Sidebar2 ####
  output$ui_sidebar2 <- renderUI({
  tagList(
    pickerInput(inputId = "inp2_0",
                label = "选择要显示的条目", 
                choices = Items2,
                selected = Items2,
                options = list(`selected-text-format` = "count > 3", `actions-box` = TRUE),
                multiple = TRUE),
    h4(strong("筛选项目")),
    checkboxGroupButtons(inputId = "inp2_1",
                         label = "1. 收录情况",
                         choices = c("SCIE", "SSCI", "AHCI", "ESCI"),
                         status = "primary",
                         checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove",lib = "glyphicon")), size = 'xs'),
    checkboxGroupButtons(inputId = "inp2_2",
                         label = "2. JCR分区",
                         choices = c("Q1", "Q2", "Q3", "Q4", "N"),
                         status = "primary",
                         checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove",lib = "glyphicon")), size = 'xs'),
    pickerInput(inputId = "inp2_3",
                label = "3. 分类", 
                choices = sort(jdatacate),
                options = list(`selected-text-format` = "count > 3", `actions-box` = TRUE), multiple = TRUE)
  )
  })
  
  #### 5. Main2 ####
  output$ui_main2    <- renderUI({
    tagList(
      h3("2022年度JCR期刊信息表"),
      tags$hr(),
      DT::dataTableOutput("table2")
    )})
  output$table2 <- DT::renderDataTable({
    if(length(input$inp2_0) != 0) input2_0 <- input$inp2_0 else input2_0 <- Items2
    if(length(input$inp2_1) != 0) input2_1 <- input$inp2_1 else input2_1 <- c("SCIE", "SSCI", "AHCI", "ESCI")
    if(length(input$inp2_2) != 0) input2_2 <- input$inp2_2 else input2_2 <- c("Q1", "Q2", "Q3", "Q4", "N")
    if(length(input$inp2_3) != 0) input2_3 <- input$inp2_3 else input2_3 <- jdatacate
    
    if(length(input2_1) < 4){
    sig1 <- seq(T, nrow(jdata2022))
      for(i in input2_1){
        sig1 <- sig1&str_detect(jdata2022$collection, i)
      }
    } else{
    sig1 <- seq(T, nrow(jdata2022))
    }
    
    if(length(input2_3) < length(jdatacate)){
    sig2 <- seq(T, nrow(jdata2022))
      for(i in input2_3){
        sig2 <- sig2&str_detect(jdata2022$category, i)
      }
    } else{
    sig2 <- seq(T, nrow(jdata2022))
    }
    
    showdt <- jdata2022[sig1&sig2,]  %>% .[rank %in% input2_2,
                                     .SD, .SDcols = input2_0]

    DT::datatable(showdt, 
                  options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE))
    
  })
  
}
