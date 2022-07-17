##########===== server =====##########
server <- function(input, output) {
  #### 0. Preparation ####
  
  showtext_auto()
  font_add("msyh", "./msyh.ttc")
  
  #### 1. Load dataset ####
  jd18 <- fread('./journal2018.csv', stringsAsFactors=F, encoding = "UTF-8")
  jd22 <- fread('./journal2022.csv', stringsAsFactors=F, encoding = "UTF-8")
  jdch <- fread('./journalchinese.csv', stringsAsFactors=F, encoding = "UTF-8")
  
  cate22 <- fread("./journal2022category.csv", header = T, sep = ",")$cate
  
  catech1 <- jdch$category1 %>% unique() %>% str_split("；") %>% unlist() %>% unique() %>% .[order(.)]
  catech2 <- jdch$category2 %>% unique() %>% str_split("；") %>% unlist() %>% unique() %>% .[order(.)]
  evalch   <- jdch$evaluation %>% str_split("/") %>% unlist() %>% unique() %>% .[order(.)] %>% .[-1] %>% append("-")
  freqch   <- jdch$frequency %>% unique() %>% .[order(.)] %>% .[-1] %>% append("-")
  
  item18 <- names(jd18)
  item22 <- names(jd22)
  itemch <- names(jdch) %>% .[-1]
  ########################  
  #### 2. Sidebar1 ####
  output$ui_sidebar1 <- renderUI({
    tagList(
      pickerInput(inputId = "inp1_item",
                  label = "展示条目", 
                  choices = item18,
                  selected = item18,
                  options = list(`selected-text-format` = "count > 3", `actions-box` = TRUE),
                  multiple = TRUE),
      h4(strong("属性筛选")),
      checkboxGroupButtons(inputId = "inp1_coll",
                           label = "1.收录情况",
                           choices = c("SCI", "SCIE", "SCI/SCIE", "无数据"),
                           status = "primary",
                           checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove",lib = "glyphicon")), size = 'xs'),
      checkboxGroupButtons(inputId = "inp1_divi",
                           label = "2.中科院分区",
                           choices = c("1区", "2区", "3区", "4区", "未录"),
                           status = "primary",
                           checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove",lib = "glyphicon")), size = 'xs'),
      pickerInput(inputId = "inp1_cate",
                  label = "3.分类", 
                  choices = sort(unique(jd18$Category)),
                  options = list(`selected-text-format` = "count > 3", `actions-box` = TRUE), multiple = TRUE),
      pickerInput(inputId = "inp1_disc",
                  label = "4.学科", 
                  choices = sort(unique(jd18$Discipline)),
                  options = list(`selected-text-format` = "count > 3", `actions-box` = TRUE), multiple = TRUE),
      checkboxGroupButtons(inputId = "inp1_topj",
                           label = "5.Top期刊",
                           choices = c("是", "否", "无数据"),
                           status = "primary",
                           checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove",lib = "glyphicon")), size = 'xs'),
      checkboxGroupButtons(inputId = "inp1_revi",
                           label = "6.综述期刊",
                           choices = c("是", "否", "无数据"),
                           status = "primary",
                           checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove",lib = "glyphicon")), size = 'xs'),
      pickerInput(inputId = "inp1_freq",
                  label = "7.出版周期", 
                  choices = sort(unique(jd18$PublicationCycle)),
                  options = list(`selected-text-format`="count > 3",`actions-box`=TRUE), multiple = TRUE),
      pickerInput(inputId = "inp1_loac",
                  label = "8.出版地区", 
                  choices = sort(unique(jd18$Region)),
                  options = list(`selected-text-format`="count > 3",`actions-box`=TRUE), multiple = TRUE)
    )
  })
  
  #### 3. Main1 ####
  output$ui_main1    <- renderUI({
    tagList(
      h3("英文期刊信息（LetPub 2018年数据）"),
      tags$hr(),
      DT::dataTableOutput("table")
    )})
  output$table <- DT::renderDataTable({
    if(length(input$inp1_item) != 0) ip1_item <- input$inp1_item else ip1_item <- item18
    if(length(input$inp1_coll) != 0) ip1_coll <- input$inp1_coll else ip1_coll <- unique(jd18$IsSCI)
    if(length(input$inp1_divi) != 0) ip1_divi <- input$inp1_divi else ip1_divi <- unique(jd18$CASRanking)
    if(length(input$inp1_cate) != 0) ip1_cate <- input$inp1_cate else ip1_cate <- unique(jd18$Category)
    if(length(input$inp1_disc) != 0) ip1_disc <- input$inp1_disc else ip1_disc <- unique(jd18$Discipline)
    if(length(input$inp1_topj) != 0) ip1_topj <- input$inp1_topj else ip1_topj <- unique(jd18$IsTop)
    if(length(input$inp1_revi) != 0) ip1_revi <- input$inp1_revi else ip1_revi <- unique(jd18$IsReview)
    if(length(input$inp1_freq) != 0) ip1_freq <- input$inp1_freq else ip1_freq <- unique(jd18$PublicationCycle)
    if(length(input$inp1_loac) != 0) ip1_loac <- input$inp1_loac else ip1_loac <- unique(jd18$Region)
    DT::datatable(jd18[IsSCI %in% ip1_coll &
                         CASRanking %in% ip1_divi &
                         Category %in% ip1_cate &
                         Discipline %in% ip1_disc &
                         IsTop %in% ip1_topj &
                         IsReview %in% ip1_revi &
                         PublicationCycle %in% ip1_freq &
                         Region %in% ip1_loac, .SD, .SDcols = ip1_item], 
                  options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE))
    
  })
  
  ########################
  #### 4. Sidebar2 ####
  output$ui_sidebar2 <- renderUI({
    tagList(
      pickerInput(inputId = "inp2_item",
                  label = "展示条目", 
                  choices = item22,
                  selected = item22,
                  options = list(`selected-text-format` = "count > 3", `actions-box` = TRUE),
                  multiple = TRUE),
      h4(strong("属性筛选")),
      checkboxGroupButtons(inputId = "inp2_coll",
                           label = "1.收录情况",
                           choices = c("SCIE", "SSCI", "AHCI", "ESCI"),
                           status = "primary",
                           checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove",lib = "glyphicon")), size = 'xs'),
      checkboxGroupButtons(inputId = "inp2_quar",
                           label = "2.JCR分区",
                           choices = c("Q1", "Q2", "Q3", "Q4", "N"),
                           status = "primary",
                           checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove",lib = "glyphicon")), size = 'xs'),
      pickerInput(inputId = "inp2_cate",
                  label = "3.分类", 
                  choices = sort(cate22),
                  options = list(`selected-text-format` = "count > 3", `actions-box` = TRUE), multiple = TRUE)
    )
  })
  
  #### 5. Main2 ####
  output$ui_main2    <- renderUI({
    tagList(
      h3("英文期刊信息（JCR 2022年数据）"),
      tags$hr(),
      DT::dataTableOutput("table2")
    )})
  output$table2 <- DT::renderDataTable({
    if(length(input$inp2_item) != 0) ip2_item <- input$inp2_item else ip2_item <- item22
    if(length(input$inp2_coll) != 0) ip2_coll <- input$inp2_coll else ip2_coll <- c("SCIE", "SSCI", "AHCI", "ESCI")
    if(length(input$inp2_quar) != 0) ip2_quar <- input$inp2_quar else ip2_quar <- c("Q1", "Q2", "Q3", "Q4", "N")
    if(length(input$inp2_cate) != 0) ip2_cate <- input$inp2_cate else ip2_cate <- cate22
    
    if(length(ip2_coll) < 4){
      sig1 <- seq(T, nrow(jd22))
      for(i in ip2_coll){
        sig1 <- sig1&str_detect(jd22$collection, i)
      }
    } else{
      sig1 <- seq(T, nrow(jd22))
    }
    
    if(length(ip2_cate) < length(cate22)){
      sig2 <- seq(T, nrow(jd22))
      for(i in ip2_cate){
        sig2 <- sig2&str_detect(jd22$category, i)
      }
    } else{
      sig2 <- seq(T, nrow(jd22))
    }
    
    showdt <- jd22[sig1&sig2,]  %>% .[rank %in% ip2_quar, .SD, .SDcols = ip2_item]
    DT::datatable(showdt, options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE))
  })
  
  ########################
  #### 6. Sidebar3 ####
  output$ui_sidebar3 <- renderUI({
    tagList(
      pickerInput(inputId = "inp3_item",
                  label = "展示条目", 
                  choices = itemch,
                  selected = itemch[c(3,6,7,8,10,13,21)],
                  options = list(`selected-text-format` = "count > 3", `actions-box` = TRUE),
                  multiple = TRUE),
      h4(strong("属性筛选")),
      # radioButtons(inputId = "AorO",
      #              label = "多项属性关系", 
      #              choices = list("OR" = "o", "AND" = "a"),
      #              inline = T),
      pickerInput(inputId = "inp3_cate1",
                  label = "1.一级分类", 
                  choices = catech1,
                  options = list(`selected-text-format` = "count > 3", `actions-box` = TRUE), multiple = TRUE),
      pickerInput(inputId = "inp3_cate2",
                  label = "2.二级分类", 
                  choices = catech2,
                  options = list(`selected-text-format` = "count > 3", `actions-box` = TRUE), multiple = TRUE),
      pickerInput(inputId = "inp3_eval",
                  label = "3.收录情况", 
                  choices = evalch,
                  options = list(`selected-text-format` = "count > 3", `actions-box` = TRUE), multiple = TRUE),
      pickerInput(inputId = "inp3_freq",
                  label = "4.出版周期", 
                  choices = freqch,
                  options = list(`selected-text-format` = "count > 3", `actions-box` = TRUE), multiple = TRUE),
      checkboxGroupButtons(inputId = "inp3_lang",
                           label = "5.期刊语言",
                           choices = c("中文", "英文", "日文", "韩文"),
                           status = "primary",
                           checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove",lib = "glyphicon")), size = 'xs'),
      checkboxGroupButtons(inputId = "inp3_stat",
                           label = "6.出版状态",
                           choices = c("发行", "停刊", "合并"),
                           status = "primary",
                           checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove",lib = "glyphicon")), size = 'xs'),
    )
  })
  #### 7. Main3 ####
  output$ui_main3    <- renderUI({
    tagList(
      h3("中文期刊信息（CNKI 2022年数据）"),
      tags$hr(),
      DT::dataTableOutput("table3")
    )})
  output$table3 <- DT::renderDataTable({
    if(length(input$inp3_item) != 0)  ip3_item  <- input$inp3_item  else ip3_item  <- itemch
    if(length(input$inp3_cate1) != 0) ip3_cate1 <- input$inp3_cate1 else ip3_cate1 <- catech1
    if(length(input$inp3_cate2) != 0) ip3_cate2 <- input$inp3_cate2 else ip3_cate2 <- catech2
    if(length(input$inp3_eval) != 0)  ip3_eval  <- input$inp3_eval  else ip3_eval  <- evalch
    if(length(input$inp3_freq) != 0)  ip3_freq  <- input$inp3_freq  else ip3_freq  <- freqch
    if(length(input$inp3_lang) != 0)  ip3_lang  <- input$inp3_lang  else ip3_lang  <- c("中文", "英文", "日文", "韩文")
    if(length(input$inp3_stat) != 0)  ip3_stat  <- input$inp3_stat  else ip3_stat  <- c("发行", "停刊", "合并")
    
    sig_cate1 <- sig_cate2 <- sig_eval <- sig_freq<- sig_lang <- sig_stat <- rep(T, nrow(jdch))
    
    if(length(ip3_cate1) < length(catech1)){
      sig_cate1 <- rep(F, nrow(jdch))
      for(i in ip3_cate1) sig_cate1 <- sig_cate1|str_detect(jdch$category1, i)
    } 
    
    if(length(ip3_cate2) < length(catech2)){
      sig_cate2 <- rep(F, nrow(jdch))
      for(i in ip3_cate2) sig_cate2 <- sig_cate2|str_detect(jdch$category2, i)
    } 
    
    if(length(ip3_eval)  < length(evalch)){
      sig_eval <- rep(F, nrow(jdch))
      for(i in ip3_eval)  sig_eval  <- sig_eval|str_detect(jdch$evaluation, i)
    } 
    
    if(length(ip3_freq)  < length(freqch))  sig_freq <- jdch$frequency %in% ip3_freq
    
    if(length(ip3_lang) < 4){
      sig_lang <- rep(F, nrow(jdch))
      for(i in ip3_lang) sig_lang <- sig_lang|str_detect(jdch$language, i)
    } 
    
    if(length(ip3_stat) < 3) sig_stat <- jdch$status %in% ip3_stat
    
    sig <- sig_cate1&sig_cate2&sig_eval&sig_freq&sig_lang&sig_stat
    
    showdt <- jdch[sig, .SD, .SDcols = ip3_item]
    # names(showdt) <- c("ISSN", "CN", "刊名", "译名", "曾用刊名",
    #                    "一级分类", "二级分类", "复合IF", "综合IF", "出版量",
    #                    "下载量", "引用量", "收录", "主办单位", "出版地",
    #                    "出版周期", "语言", "开本", "邮发代号", "创刊时间",
    #                    "当前状态", "知网链接")
    DT::datatable(showdt, options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE))
  })
}
