# Visualization of Journal information

# packages
library(DT)
library(shiny)
library(stringr)
library(magrittr)
library(data.table)
library(shinyWidgets)

########## ui ##########
ui <- fluidPage(
  tags$script(HTML("
  $(document).on('keydown', '[id$=\"_search_content\"]', function(e) {
    if (e.key === 'Enter') {
      e.preventDefault();
  
      Shiny.setInputValue(
        this.id,
        $(this).val(),
        {priority: 'event'}
      );
    }
  });
  ")),
  tags$head(tags$title("Journal")),
  tags$head(tags$link(rel = "shortcut icon", href = "journal.ico")),
  titlePanel("学术期刊收录信息"),
  tabsetPanel(
    id = "mainPanel",
    type = "pills",
    tabPanel(
      title = "LetPub",
      sidebarLayout(
        sidebarPanel(width=3, uiOutput("sidebar_letpub")),
        mainPanel(uiOutput("main_letpub"))
      )
    ),
    tabPanel(
      title = "JCR",
      sidebarLayout(
        sidebarPanel(width = 3, uiOutput("sidebar_jcr")),
        mainPanel(uiOutput("main_jcr"))
      )
    ),
    tabPanel(
      title = "新锐分区",
      sidebarLayout(
        sidebarPanel(width = 3, uiOutput("sidebar_xr")),
        mainPanel(uiOutput("main_xr"))
      )
    ),
    tabPanel(
      title = "CNKI",
      sidebarLayout(
        sidebarPanel(width = 3, uiOutput("sidebar_cnki")),
        mainPanel(uiOutput("main_cnki"))
      )
    ),
    tabPanel(
      title = "已选项目",
      uiOutput("ui_main_selected")
    )
  ),
  
  # 网页页脚 ...................................................................
  hr(),
  div(
    tags$style(HTML("
        #footer-container { text-align:center; font-size:13px; color:#666666; line-height:0.8; margin:12px 0px; } 
        #footer-container a { color:#0066cc; text-decoration:none; margin: 0 2px; } 
        #footer-container a:hover { text-decoration:none; } 
        .footer-links, .footer-records { display:inline-flex; flex-wrap:wrap; justify-content:center; margin: 0 2px; gap: 10px; }
      ")),
    div(
      id = "footer-container",
      tags$p(sprintf("© 2021–%s, Lcpmgh. All rights reserved.", format(Sys.Date(), "%Y"))),
      div(
        class = "footer-links",
        tags$a(icon("github"), " lcpmgh", href = "https://github.com/lcpmgh", target="_blank"),
        tags$a(icon("envelope"), " lcpmgh@gmail.com", href = "mailto:lcpmgh@gmail.com"),
        tags$a(icon("home"), " lcpmgh.com", href = "http://lcpmgh.com/", target="_blank")
      ),
      div(
        class = "footer-records",
        tags$a("冀ICP备2022003075号", href = "https://beian.miit.gov.cn", target="_blank"),
        tags$a("川公网安备51010702002736", href = "http://www.beian.gov.cn/portal/registerSystemInfo?recordcode=51010702002736", target="_blank")
      )
    )
  )
  # ............................................................................
)

########## server ##########
server <- function(input, output, session) {
  #### 01. Load dataset ####
  # 原始数据
  jdat_letpub <- fread('./journal_letpub_2018.csv', stringsAsFactors=F, encoding = "UTF-8")
  jdat_jcr    <- fread('./journal_jcr_2025.csv', stringsAsFactors=F, encoding = "UTF-8") %>% setorder(-JCR_yr, -JIF, na.last=T)
  jdat_cnki   <- fread('./journal_cnki_2022.csv', stringsAsFactors=F, encoding = "UTF-8")
  jdat_xr     <- fread('./journal_xr_2026.csv', stringsAsFactors=F, encoding = "UTF-8") %>% setorder(Category, Serial_number)
  
  # 数据处理
  cate_jcr    <- jdat_jcr$Category %>% unique() %>% sort()
  cate_xr     <- jdat_xr$Category %>% unique() %>% sort()
  cate_cnki_1 <- jdat_cnki$category1  %>% unique() %>% str_split("；") %>% unlist() %>% unique() %>% .[order(.)]
  cate_cnki_2 <- jdat_cnki$category2  %>% unique() %>% str_split("；") %>% unlist() %>% unique() %>% .[order(.)]
  eval_cnki   <- jdat_cnki$evaluation %>% str_split("/") %>% unlist() %>% unique() %>% .[order(.)] %>% .[-1] %>% append("-")
  freq_cnki   <- jdat_cnki$frequency  %>% unique() %>% .[order(.)] %>% .[-1] %>% append("-")
  
  item_letpub <- names(jdat_letpub)
  item_jcr    <- names(jdat_jcr)
  item_xr     <- names(jdat_xr)
  item_cnki   <- names(jdat_cnki) %>% .[-1]
  
  year_jcr <- jdat_jcr$JCR_yr %>% unique()  %>% sort(decreasing=T)
  tedi_jcr <- jdat_jcr$Edition %>% unique() %>% sort()
  edit_jcr <- tedi_jcr[!str_detect(tedi_jcr,"/")]
  jciq_jcr <- jdat_jcr$JCI_Quartile %>% unique() %>% .[str_length(.)>0] %>% sort()
  jifq_jcr <- jdat_jcr$JIF_Quartile %>% unique() %>% .[str_length(.)>0] %>% sort()
  aisq_jcr <- jdat_jcr$AIS_Quartile %>% unique() %>% .[str_length(.)>0] %>% sort()
  
  rank_xr  <- jdat_xr$Journal_ranking %>% unique() %>% sort()
  names(rank_xr) <- paste0(rank_xr, "区")
  
  ########################  
  #### 02. sidebar：letpub ####
  output$sidebar_letpub <- renderUI({
    tagList(
      pickerInput(
        inputId  = "inp1_item",
        label    = "展示条目", 
        choices  = item_letpub,
        selected = item_letpub,
        options  = list(`selected-text-format` = "count > 3", `actions-box` = TRUE),
        multiple = TRUE
      ),
      h4(strong("属性筛选")),
      checkboxGroupButtons(
        inputId   = "inp1_coll",
        label     = "1.收录情况",
        choices   = c("SCI", "SCIE", "SCI/SCIE", "无数据"),
        status    = "primary",
        size      = 'xs',
        checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove",lib = "glyphicon"))
      ),
      checkboxGroupButtons(
        inputId   = "inp1_divi",
        label     = "2.中科院分区",
        choices   = c("1区", "2区", "3区", "4区", "未录"),
        status    = "primary",
        size      = 'xs',
        checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove",lib = "glyphicon"))
      ),
      pickerInput(
        inputId  = "inp1_cate",
        label    = "3.分类", 
        choices  = sort(unique(jdat_letpub$Category)),
        options  = list(`selected-text-format` = "count > 3", `actions-box` = TRUE), 
        multiple = TRUE
      ),
      pickerInput(
        inputId  = "inp1_disc",
        label    = "4.学科", 
        choices  = sort(unique(jdat_letpub$Discipline)),
        options  = list(`selected-text-format` = "count > 3", `actions-box` = TRUE), 
        multiple = TRUE
      ),
      checkboxGroupButtons(
        inputId   = "inp1_topj",
        label     = "5.Top期刊",
        choices   = c("是", "否", "无数据"),
        status    = "primary",
        size      = 'xs',
        checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove",lib = "glyphicon"))
      ),
      checkboxGroupButtons(
        inputId   = "inp1_revi",
        label     = "6.综述期刊",
        choices   = c("是", "否", "无数据"),
        status    = "primary",
        size      = 'xs',
        checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove",lib = "glyphicon"))
      ),
      pickerInput(
        inputId  = "inp1_freq",
        label    = "7.出版周期", 
        choices  = sort(unique(jdat_letpub$PublicationCycle)),
        options  = list(`selected-text-format`="count > 3",`actions-box`=TRUE), 
        multiple = TRUE
      ),
      pickerInput(
        inputId  = "inp1_loac",
        label    = "8.出版地区", 
        choices  = sort(unique(jdat_letpub$Region)),
        options  = list(`selected-text-format`="count > 3",`actions-box`=TRUE), 
        multiple = TRUE
      )
    )
  })
  
  #### 03. main：   letpub ####
  output$main_letpub    <- renderUI({
    tagList(
      h3("LetPub 2018年数据"),
      tags$hr(),
      DT::dataTableOutput("table_letpub")
    )})
  output$table_letpub   <- DT::renderDataTable({
    if(length(input$inp1_item) != 0) ip1_item <- input$inp1_item else ip1_item <- item_letpub
    if(length(input$inp1_coll) != 0) ip1_coll <- input$inp1_coll else ip1_coll <- unique(jdat_letpub$IsSCI)
    if(length(input$inp1_divi) != 0) ip1_divi <- input$inp1_divi else ip1_divi <- unique(jdat_letpub$CASRanking)
    if(length(input$inp1_cate) != 0) ip1_cate <- input$inp1_cate else ip1_cate <- unique(jdat_letpub$Category)
    if(length(input$inp1_disc) != 0) ip1_disc <- input$inp1_disc else ip1_disc <- unique(jdat_letpub$Discipline)
    if(length(input$inp1_topj) != 0) ip1_topj <- input$inp1_topj else ip1_topj <- unique(jdat_letpub$IsTop)
    if(length(input$inp1_revi) != 0) ip1_revi <- input$inp1_revi else ip1_revi <- unique(jdat_letpub$IsReview)
    if(length(input$inp1_freq) != 0) ip1_freq <- input$inp1_freq else ip1_freq <- unique(jdat_letpub$PublicationCycle)
    if(length(input$inp1_loac) != 0) ip1_loac <- input$inp1_loac else ip1_loac <- unique(jdat_letpub$Region)
    showdt <- jdat_letpub[IsSCI %in% ip1_coll &
                             CASRanking %in% ip1_divi &
                             Category %in% ip1_cate &
                             Discipline %in% ip1_disc &
                             IsTop %in% ip1_topj &
                             IsReview %in% ip1_revi &
                             PublicationCycle %in% ip1_freq &
                             Region %in% ip1_loac,]
    session$userData$showdt_letpub <- showdt
    DT::datatable(showdt[, .SD, .SDcols = ip1_item], 
                  escape = FALSE, 
                  options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE),
                  selection = list(mode = "multiple", target = "row"))
  })
  
  ########################
  #### 04. sidebar：jdat_jcr ####
  output$sidebar_jcr <- renderUI({
    tagList(
      pickerInput(
        inputId  = "inp2_item",
        label    = "展示列项目", 
        choices  = item_jcr,
        selected = item_jcr[c(1,2,5,8,17,18)],
        options  = pickerOptions(container="body", liveSearch=TRUE, actionsBox=TRUE, `selected-text-format`="count > 3", `actions-box`=TRUE),
        multiple = TRUE
      ),
      h4(strong("属性筛选")),
      pickerInput(
        inputId  = "inp2_cate",
        label    = "期刊分类", 
        choices  = cate_jcr,
        options  = pickerOptions(container="body", liveSearch=TRUE, actionsBox=TRUE, `selected-text-format`="count > 3", `actions-box`=TRUE),
        multiple = TRUE
      ),
      pickerInput(
        inputId  = "inp2_yr",
        label    = "年份", 
        choices  = year_jcr,
        selected = year_jcr[1],
        options  = pickerOptions(container="body", actionsBox=TRUE, `selected-text-format`="count > 3", `actions-box`=TRUE),
        multiple = TRUE
      ),
      checkboxGroupButtons(
        inputId   = "inp2_ed",
        label     = "引文索引数据库", 
        choices   = edit_jcr,
        selected  = "SCIE",
        status    = "primary",
        size      = 'xs',
        checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove",lib = "glyphicon"))
      ),
      checkboxGroupButtons(
        inputId   = "inp2_jci",
        label     = "JCI分区", 
        choices   = jciq_jcr,
        selected  = jciq_jcr,
        status    = "primary",
        size      = 'xs',
        checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove",lib = "glyphicon"))
      ),
      checkboxGroupButtons(
        inputId   = "inp2_jif",
        label     = "JIF分区", 
        choices   = jifq_jcr,
        selected  = jifq_jcr,
        status    = "primary",
        size      = 'xs',
        checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove",lib = "glyphicon"))
      ),
      checkboxGroupButtons(
        inputId   = "inp2_ais",
        label     = "AIS分区", 
        choices   = aisq_jcr,
        selected  = aisq_jcr,
        status    = "primary",
        size      = 'xs',
        checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove",lib = "glyphicon"))
      ),
      h4(strong("查找期刊")),
      # pickerInput(
      #   inputId  = "inp2_search_col",
      #   label    = "搜索列", 
      #   choices  = year_jcr,
      #   selected = year_jcr[1],
      #   multiple = F
      # ),
      
      searchInput(
        inputId     = "inp2_search_content",
        label       = NULL,
        placeholder = "查找...",
        btnSearch = icon("magnifying-glass"),
        btnReset = icon("xmark")
      ),
      div(
        style = "display: flex; justify-content: center; width: 100%; ",
        prettySwitch(
          inputId = "inp2_search_precise",
          label   = "精确查找",
          status  = "primary",
          fill    = TRUE
        )
      )
    )
  })
  
  #### 05. main：   dat_jcr ####
  output$main_jcr    <- renderUI({
    tagList(
      h3("JCR 2019-2025年数据"),
      tags$hr(),
      DT::dataTableOutput("table_jcr")
    )})
  output$table_jcr   <- DT::renderDataTable({
    item <- input$inp2_item
    yr <- input$inp2_yr
    jc <- input$inp2_jci
    ji <- input$inp2_jif
    ai <- input$inp2_ais
    ca <- input$inp2_cate
    ed <- input$inp2_ed
    
    if(length(ca)==0) ca <- cate_jcr
    scarch_p <- input$inp2_search_precise
    
    ed_or   <- jdat_jcr$Edition %>% unique()
    ed_or_s <- ed_or[sapply(strsplit(ed_or, "/"), \(i) any(ed %in% i))]   #匿名函数/(i)等价于 function(i)
    showdt  <- jdat_jcr[Edition %in% ed_or_s, ] %>% 
      .[JCR_yr %in% yr & JCI_Quartile %in% jc & JIF_Quartile %in% ji & AIS_Quartile %in% ai & Category %in% ca, .SD, .SDcols=item] 
    
    # 查找期刊
    if (nzchar(input$inp2_search_content)) {
      if(scarch_p){
        showdt <- showdt[trimws(tolower(Journal_name)) == trimws(tolower(input$inp2_search_content))]
      } else {
        showdt <- showdt[grepl(tolower(input$inp2_search_content), tolower(Journal_name), fixed=TRUE)]
      }
    }
   
    # 输出表格
    session$userData$showdt_jcr <- showdt
    DT::datatable(showdt, 
                  escape = FALSE, 
                  options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE),
                  selection = list(mode = "multiple", target = "row"))
  })
  
  ########################
  #### 06. sidebar：jdat_xr ####
  output$sidebar_xr    <- renderUI({
    tagList(
      pickerInput(
        inputId  = "inp3_item",
        label    = "展示列项目", 
        choices  = item_xr,
        selected = item_xr[c(1,2,3,6)],
        options  = pickerOptions(container="body", liveSearch=TRUE, actionsBox=TRUE, `selected-text-format`="count > 3", `actions-box`=TRUE),
        multiple = TRUE
      ),
      h4(strong("属性筛选")),
      pickerInput(
        inputId  = "inp3_cate",
        label    = "期刊分类", 
        choices  = cate_xr,
        options  = pickerOptions(container="body", liveSearch=TRUE, actionsBox=TRUE, `selected-text-format`="count > 3", `actions-box`=TRUE),
        multiple = TRUE
      ),
      checkboxGroupButtons(
        inputId   = "inp3_rank",
        label     = "分区", 
        choices   = rank_xr,
        selected  = rank_xr[1],
        status    = "primary",
        size      = 'xs',
        checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove",lib = "glyphicon"))
      ),
      h4(strong("查找期刊")),
      searchInput(
        inputId     = "inp3_search_content",
        label       = NULL,
        placeholder = "查找...",
        btnSearch = icon("magnifying-glass"),
        btnReset = icon("xmark")
      ),
      div(
        style = "display: flex; justify-content: center; width: 100%; ",
        prettySwitch(
          inputId = "inp3_search_precise",
          label   = "精确查找",
          status  = "primary",
          fill    = TRUE
        )
      )
    )
  })
  #### 07. main：   jdat_xr ####
  output$main_xr       <- renderUI({
    tagList(
      h3("新锐分区 2026年数据"),
      tags$hr(),
      DT::dataTableOutput("table_xr")
    )})
  output$table_xr      <- DT::renderDataTable({
    item <- input$inp3_item
    ca <- input$inp3_cate
    ra <- input$inp3_rank
    if(length(ca)==0) ca <- cate_xr
    scarch_p <- input$inp3_search_precise
    
    showdt  <- jdat_xr[Category %in% ca & Journal_ranking %in% ra, .SD, .SDcols=item] 
    
    # 查找期刊
    if (nzchar(input$inp3_search_content)) {
      if(scarch_p){
        showdt <- showdt[trimws(tolower(Journal_name)) == trimws(tolower(input$inp3_search_content))]
      } else {
        showdt <- showdt[grepl(tolower(input$inp3_search_content), tolower(Journal_name), fixed=TRUE)]
      }
    }
    
    # 输出表格
    session$userData$showdt_xr <- showdt
    DT::datatable(showdt, 
                  escape = FALSE, 
                  options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE),
                  selection = list(mode = "multiple", target = "row"))
  })
  
  ########################  
  #### 08. sidebar：jdat_cnki ####
  output$sidebar_cnki <- renderUI({
    tagList(
      pickerInput(
        inputId  = "inp4_item",
        label    = "展示条目", 
        choices  = item_cnki,
        selected = item_cnki[c(3,6,7,8,10,13,16)],
        options  = list(`selected-text-format` = "count > 3", `actions-box` = TRUE),
        multiple = TRUE
      ),
      h4(strong("属性筛选")),
      pickerInput(
        inputId  = "inp4_cate1",
        label    = "1.一级分类", 
        choices  = cate_cnki_1,
        options  = list(`selected-text-format` = "count > 3", `actions-box` = TRUE),
        multiple = TRUE
      ),
      pickerInput(
        inputId  = "inp4_cate2",
        label    = "2.二级分类", 
        choices  = cate_cnki_2,
        options  = list(`selected-text-format` = "count > 3", `actions-box` = TRUE),
        multiple = TRUE
      ),
      pickerInput(
        inputId  = "inp4_eval",
        label    = "3.收录情况", 
        choices  = eval_cnki,
        options  = list(`selected-text-format` = "count > 3", `actions-box` = TRUE), 
        multiple = TRUE
      ),
      pickerInput(
        inputId  = "inp4_freq",
        label    = "4.出版周期", 
        choices  = freq_cnki,
        options  = list(`selected-text-format` = "count > 3", `actions-box` = TRUE),
        multiple = TRUE
      ),
      checkboxGroupButtons(
        inputId   = "inp4_lang",
        label     = "5.期刊语言",
        choices   = c("中文", "英文", "日文", "韩文"),
        status    = "primary",
        size      = 'xs',
        checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove",lib = "glyphicon"))
      ),
      checkboxGroupButtons(
        inputId   = "inp4_stat",
        label     = "6.出版状态",
        choices   = c("发行", "停刊", "合并"),
        status    = "primary",
        size      = 'xs',
        checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove",lib = "glyphicon"))
      ),
    )
  })
  #### 09. main：   jdat_cnki ####
  output$main_cnki    <- renderUI({
    tagList(
      h3("CNKI 2022年数据"),
      tags$hr(),
      DT::dataTableOutput("table_cnki")
    )})
  output$table_cnki   <- DT::renderDataTable({
    if(length(input$inp4_item) != 0)  ip4_item  <- input$inp4_item  else ip4_item  <- item_cnki
    if(length(input$inp4_cate1) != 0) ip4_cate1 <- input$inp4_cate1 else ip4_cate1 <- cate_cnki_1
    if(length(input$inp4_cate2) != 0) ip4_cate2 <- input$inp4_cate2 else ip4_cate2 <- cate_cnki_2
    if(length(input$inp4_eval) != 0)  ip4_eval  <- input$inp4_eval  else ip4_eval  <- eval_cnki
    if(length(input$inp4_freq) != 0)  ip4_freq  <- input$inp4_freq  else ip4_freq  <- freq_cnki
    if(length(input$inp4_lang) != 0)  ip4_lang  <- input$inp4_lang  else ip4_lang  <- c("中文", "英文", "日文", "韩文")
    if(length(input$inp4_stat) != 0)  ip4_stat  <- input$inp4_stat  else ip4_stat  <- c("发行", "停刊", "合并")
    sig_cate1 <- sig_cate2 <- sig_eval <- sig_freq<- sig_lang <- sig_stat <- rep(T, nrow(jdat_cnki))
    if(length(ip4_cate1) < length(cate_cnki_1)){
      sig_cate1 <- rep(F, nrow(jdat_cnki))
      for(i in ip4_cate1) sig_cate1 <- sig_cate1|str_detect(jdat_cnki$category1, i)
    } 
    
    if(length(ip4_cate2) < length(cate_cnki_2)){
      sig_cate2 <- rep(F, nrow(jdat_cnki))
      for(i in ip4_cate2) sig_cate2 <- sig_cate2|str_detect(jdat_cnki$category2, i)
    } 
    
    if(length(ip4_eval)  < length(eval_cnki)){
      sig_eval <- rep(F, nrow(jdat_cnki))
      for(i in ip4_eval)  sig_eval  <- sig_eval|str_detect(jdat_cnki$evaluation, i)
    } 
    
    if(length(ip4_freq)  < length(freq_cnki))  sig_freq <- jdat_cnki$frequency %in% ip4_freq
    
    if(length(ip4_lang) < 4){
      sig_lang <- rep(F, nrow(jdat_cnki))
      for(i in ip4_lang) sig_lang <- sig_lang|str_detect(jdat_cnki$language, i)
    } 
    
    if(length(ip4_stat) < 3) sig_stat <- jdat_cnki$status %in% ip4_stat
    
    sig <- sig_cate1&sig_cate2&sig_eval&sig_freq&sig_lang&sig_stat
    
    showdt <- jdat_cnki[sig,]
    # names(showdt) <- c("ISSN", "CN", "刊名", "译名", "曾用刊名",
    #                    "一级分类", "二级分类", "复合IF", "综合IF", "出版量",
    #                    "下载量", "引用量", "收录", "主办单位", "出版地",
    #                    "出版周期", "语言", "开本", "邮发代号", "创刊时间",
    #                    "当前状态", "知网链接")
    session$userData$showdt_cnki <- showdt
    DT::datatable(showdt[, .SD, .SDcols = ip4_item], 
                  escape = FALSE, 
                  options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE), 
                  selection=list(mode = "multiple", target = "row"))
  })
  
  ########################
  #### 10. Main：   对比 ####
  output$ui_main_selected    <- renderUI({
    tagList(
      h3("已选期刊："),
      hr(),
      h4("LetPub页："),
      DT::dataTableOutput("jdat_letpub"),
      hr(),
      h4("JCR页："),
      DT::dataTableOutput("jdat_jcr"),
      hr(),
      h4("新锐分区页："),
      DT::dataTableOutput("jdat_xr"),
      hr(),
      h4("CNKI页："),
      DT::dataTableOutput("jdat_cnki")
    )
  })
  row_latpub <- reactive({input$table_letpub_rows_selected})
  row_jcr    <- reactive({input$table_jcr_rows_selected})
  row_xr     <- reactive({input$table_xr_rows_selected})
  row_cnki   <- reactive({input$table_cnki_rows_selected})
  output$jdat_letpub <- DT::renderDataTable({
    datatable(
      session$userData$showdt_letpub[row_latpub(),], 
      escape = FALSE,
      options = list(
        pageLength = 100, 
        autoWidth = TRUE, 
        scrollX = TRUE, 
        searching = FALSE, 
        lengthChange = FALSE
      )
    )
  })
  output$jdat_jcr    <- DT::renderDataTable({
    datatable(
      session$userData$showdt_jcr[row_jcr(),], 
      escape = FALSE, 
      options = list(
        pageLength = 100, 
        autoWidth = TRUE, 
        scrollX = TRUE, 
        searching = FALSE, 
        lengthChange = FALSE
      )
    )
  })
  output$jdat_xr     <- DT::renderDataTable({
    datatable(
      session$userData$showdt_xr[row_xr(),], 
      escape = FALSE, 
      options = list(
        pageLength = 100, 
        autoWidth = TRUE, 
        scrollX = TRUE, 
        searching = FALSE, 
        lengthChange = FALSE
      )
    )
  })
  output$jdat_cnki   <- DT::renderDataTable({
    datatable(
      session$userData$showdt_cnki[row_cnki(), -1], 
      escape = FALSE, 
      options = list(
        pageLength = 100, 
        autoWidth = TRUE, 
        scrollX = TRUE, 
        searching = FALSE, 
        lengthChange = FALSE
      )
    )
  })
}

########## app ##########
shinyApp(ui, server)