# Visualization of Journal information

# packages
library(DT)
library(shiny)
library(stringr)
library(magrittr)
library(data.table)
library(shinyWidgets)
library(echarty)

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
      title = "LetPub",
      sidebarLayout(
        sidebarPanel(width=3, uiOutput("sidebar_letpub")),
        mainPanel(uiOutput("main_letpub"))
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
      uiOutput("main_select")
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
  #### 1. Load dataset ####
  # 原始数据
  jdat_jcr    <- fread('./journal_jcr_2025.csv', stringsAsFactors=F, encoding = "UTF-8") %>% setorder(-JCR_yr, -JIF, na.last=T)
  jdat_cnki   <- fread('./journal_cnki_2022.csv', stringsAsFactors=F, encoding = "UTF-8")
  jdat_letpub <- fread('./journal_letpub_2018.csv', stringsAsFactors=F, encoding = "UTF-8")
  jdat_xr     <- fread('./journal_xr_2026.csv', stringsAsFactors=F, encoding = "UTF-8") %>% setorder(Category, Serial_number)
  
  # 数据处理
  cate_jcr    <- jdat_jcr$Category %>% unique() %>% sort()
  cate_xr     <- jdat_xr$Category %>% unique() %>% sort()
  cate_cnki_1 <- jdat_cnki$category1  %>% unique() %>% str_split("；") %>% unlist() %>% unique() %>% .[order(.)]
  cate_cnki_2 <- jdat_cnki$category2  %>% unique() %>% str_split("；") %>% unlist() %>% unique() %>% .[order(.)]
  eval_cnki   <- jdat_cnki$evaluation %>% str_split("/") %>% unlist() %>% unique() %>% .[order(.)] %>% .[-1] %>% append("-")
  freq_cnki   <- jdat_cnki$frequency  %>% unique() %>% .[order(.)] %>% .[-1] %>% append("-")
  item_jcr    <- names(jdat_jcr)
  item_xr     <- names(jdat_xr)
  item_letpub <- names(jdat_letpub)
  item_cnki   <- names(jdat_cnki) %>% .[-1]
  year_jcr    <- jdat_jcr$JCR_yr %>% unique()  %>% sort(decreasing=T)
  tedi_jcr    <- jdat_jcr$Edition %>% unique() %>% sort()
  edit_jcr    <- tedi_jcr[!str_detect(tedi_jcr,"/")]
  jciq_jcr    <- jdat_jcr$JCI_Quartile %>% unique() %>% .[str_length(.)>0] %>% sort()
  jifq_jcr    <- jdat_jcr$JIF_Quartile %>% unique() %>% .[str_length(.)>0] %>% sort()
  aisq_jcr    <- jdat_jcr$AIS_Quartile %>% unique() %>% .[str_length(.)>0] %>% sort()
  rank_xr     <- jdat_xr$Journal_ranking %>% unique() %>% sort() %>% set_names(paste0(.,"区"))
  trend_item  <- c("JIF", "Total_Articles", "OA_percent", "Citable_Items", "Total_Citations", "JCI")
  
  ########################
  #### 2. jcr ####
  # sidebar
  output$sidebar_jcr <- renderUI({
    tagList(
      pickerInput(
        inputId  = "jcr_item",
        label    = "展示列项目", 
        choices  = item_jcr,
        selected = item_jcr[c(1,2,5,8,17,18)],
        options  = pickerOptions(container="body", liveSearch=T, actionsBox=T, `selected-text-format`="count>3", `actions-box`=T),
        multiple = T
      ),
      h4(strong("属性筛选")),
      pickerInput(
        inputId  = "jcr_cate",
        label    = "期刊分类", 
        choices  = cate_jcr,
        options  = pickerOptions(container="body", liveSearch=T, actionsBox=T, `selected-text-format`="count>3", `actions-box`=T),
        multiple = T
      ),
      pickerInput(
        inputId  = "jcr_yr",
        label    = "年份", 
        choices  = year_jcr,
        selected = year_jcr,
        options  = pickerOptions(container="body", actionsBox=T, `selected-text-format`="count>3", `actions-box`=T),
        multiple = T
      ),
      checkboxGroupButtons(
        inputId   = "jcr_ed",
        label     = "引文索引数据库", 
        choices   = edit_jcr,
        selected  = edit_jcr,
        status    = "primary",
        size      = 'xs',
        checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
      ),
      checkboxGroupButtons(
        inputId   = "jcr_jci",
        label     = "JCI分区", 
        choices   = jciq_jcr,
        selected  = jciq_jcr,
        status    = "primary",
        size      = 'xs',
        checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
      ),
      checkboxGroupButtons(
        inputId   = "jcr_jif",
        label     = "JIF分区", 
        choices   = jifq_jcr,
        selected  = jifq_jcr,
        status    = "primary",
        size      = 'xs',
        checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
      ),
      checkboxGroupButtons(
        inputId   = "jcr_ais",
        label     = "AIS分区", 
        choices   = aisq_jcr,
        selected  = aisq_jcr,
        status    = "primary",
        size      = 'xs',
        checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove",lib = "glyphicon"))
      ),
      h4(strong("查找期刊")),
      searchInput(
        inputId     = "jcr_search_content",
        label       = NULL,
        placeholder = "查找...",
        btnSearch   = icon("magnifying-glass"),
        btnReset    = icon("xmark")
      ),
      div(
        style = "display: flex; justify-content: center; width: 100%;",
        prettySwitch(
          inputId = "jcr_search_precise",
          label   = "精确查找",
          status  = "primary",
          fill    = T
        )
      ),
      hr(),
      div(
        style = "display: flex; align-items: center;justify-content: space-between;",
        h4(strong("查看趋势")),
        # actionButton(inputId="locat_sel", "定位", icon("location-crosshairs")),
        actionButton(inputId="clear_sel", "清除选中", icon("eraser"), class = "btn-primary")
      ),
      div(
        style = "display: flex; align-items: center; gap:1px;  width:100%;",
        div(
          style = "flex:8; min-width:0;",
          pickerInput(
            inputId  = "jcr_trend_item",
            label    = "选择指标", 
            choices  = trend_item,
            selected = trend_item[1],
            multiple = F
          )
        ),
        div(
          style = "flex:2; display: flex; align-items: center; gap:1px; margin-top:10px",
          shiny::actionButton(
            inputId = "item_pre",
            label   = NULL,
            icon    = icon("angle-left")
          ),
          shiny::actionButton(
            inputId = "itme_nex",
            label   = NULL,
            icon    = icon("angle-right")
          )
        )
      ),
      uiOutput("jcr_p_info"),
      ecs.output("jcr_trend_p", height = "300px")
    )
  })
  # main
  output$main_jcr    <- renderUI({
    tagList(
      h3("JCR 2019-2025年数据"),
      DT::dataTableOutput("table_jcr")
    )})
  output$table_jcr   <- DT::renderDataTable({
    item <- input$jcr_item
    yr <- input$jcr_yr
    jc <- input$jcr_jci
    ji <- input$jcr_jif
    ai <- input$jcr_ais
    ca <- input$jcr_cate
    ed <- input$jcr_ed
    
    if(length(ca)==0) ca <- cate_jcr
    scarch_p <- input$jcr_search_precise
    
    ed_or   <- jdat_jcr$Edition %>% unique()
    ed_or_s <- ed_or[sapply(strsplit(ed_or, "/"), \(i) any(ed %in% i))]   #匿名函数/(i)等价于 function(i)
    showdt  <- jdat_jcr[Edition %in% ed_or_s, ] %>% 
      .[JCR_yr %in% yr & JCI_Quartile %in% jc & JIF_Quartile %in% ji & AIS_Quartile %in% ai & Category %in% ca,]
    
    # 查找期刊
    if(nzchar(input$jcr_search_content)){
      if(scarch_p){
        showdt <- showdt[trimws(tolower(Journal_name)) == trimws(tolower(input$jcr_search_content))]
      } else {
        showdt <- showdt[grepl(tolower(input$jcr_search_content), tolower(Journal_name), fixed=TRUE)]
      }
    }
    
    # 输出表格
    session$userData$showdt_jcr_g <- showdt$group      #返回输出表格的group
    showdt <- showdt[, .SD, .SDcols=item]              #筛选列，在此之前提取group，使其不受筛选影响
    session$userData$showdt_jcr   <- showdt            #返回输出表格
    
    # 表格
    DT::datatable(
      showdt,
      escape = FALSE, 
      options = list(
        pageLength = 30, 
        autoWidth = TRUE, 
        scrollX = TRUE, 
        scrollY = "1200px",
        scrollCollapse = TRUE,  
        lengthChange = FALSE,
        searching = FALSE
      ),
      selection = list(mode = "multiple", target = "row")
    )
  })
  # sidebar上的趋势图的信息
  row_jcr            <- reactive({input$table_jcr_rows_selected})
  output$jcr_p_info  <- renderUI({
    if(length(row_jcr())<1){
      t_name <- NULL
      t_cate <- NULL
      t_grou <- NULL
      t_item <- NULL
    } else{
      row_id   <- tail(row_jcr(), 1)                           #展示最后一次点击对应的期刊
      group_id <- session$userData$showdt_jcr_g[row_id]
      tdat     <- jdat_jcr[group==group_id,]
      t_name   <- tdat$Journal_name[1]
      t_cate   <- tdat$Category[1]
      t_grou   <- group_id
      t_item   <- input$jcr_trend_item
    }
    tagList(
      p("已选期刊信息：", style = "font-weight:bold;font-size:14px;margin:2px 0;"),
      p("期刊：", tags$span(t_name,style="font-weight:bold;"), style = "font-size:12px;margin:2px 0;"),
      p("分类：", tags$span(t_cate,style="font-weight:bold;"), style = "font-size:12px;margin:2px 0;"),
      # p("编号：", tags$span(t_grou,style="font-weight:bold;"), style = "font-size:12px;margin:2px 0;"),
      p("指标：", tags$span(t_item,style="font-weight:bold;"), style = "font-size:12px;margin:2px 0;"),
      p("趋势图：", style = "font-size:12px;margin:2px 0;")
    )
  })
  # sidebar上的趋势图
  output$jcr_trend_p <- ecs.render({
    # 如果没选则画提示信息
    if(length(row_jcr())<1){
      p <- ec.init(
        graphic = list(
          type  = "text",
          left  = "center",
          top   = "middle",
          style = list(
            text = "点击表格\n展示期刊信息",
            fill = "red",
            fontSize = 15,
            lineHeight = 20,
            fontWeight = "bold",
            textAlign = "center",
            textVerticalAlign = "middle"
          )
        ),
        xAxis   = list(show = FALSE),
        yAxis   = list(show = FALSE),
        tooltip = list(show = FALSE)
      )
      return(p)
    }
    
    # 如果不是，正常画图
    col_show <- input$jcr_trend_item
    row_id   <- tail(row_jcr(), 1)                           #展示最后一次点击对应的期刊
    group_id <- session$userData$showdt_jcr_g[row_id]
    tdat1    <- jdat_jcr[group==group_id,]
    tdat2    <- tdat1 %>% .[,max(.SD, na.rm=T), by="JCR_yr", .SDcols=col_show] %>% set_colnames(c("v1", "v2")) %>% setorder(v1)
    jname    <- tdat1$Journal_name[1]
    ymin     <- tdat2$v2 %>% min()
    ymax     <- tdat2$v2 %>% max()
    yrange   <- ymax-ymin
    yupper   <- ((ymax+0.10*yrange)) %>% ceiling()
    ylower   <- ((ymin-0.10*yrange)) %>% floor() %>% max(0)
    plotdt   <- tdat2 %>% .[data.table(v1=2019:2025), on="v1"]   #让每一年都有横坐标
    p <- ec.init(
      color = "#56B4E9",
      title = list(text=jname, textStyle=list(fontSize=11), top=0),
      series = list(
        list(
          type = "line", 
          name = col_show, 
          data = plotdt$v2, 
          label = list(normal = list(show=TRUE, position="top", textStyle=list(fontSize=10)))
        )
      ),
      xAxis = list(
        type = "category", 
        data = plotdt$v1,
        name = "Year",
        boundaryGap = TRUE,          #离散x轴，两端是否gap
        nameLocation = "middle",
        nameTextStyle = list(fontSize = 10, padding = c(10, 0, 0, 0)), 
        axisLine = list(show = TRUE, lineStyle = list(color = "black")),
        axisTick = list(show = T, alignWithLabel =T),
        axisLabel = list(fontSize = 10 , padding = c(0, 0, 0, 0)
        )
      ),
      yAxis = list(
        type = "value", 
        name = col_show,
        min = ylower,
        max = yupper,
        nameTextStyle = list(fontSize = 10, padding = c(0, 0, 0, 0)),
        nameLocation = "middle",
        nameRotate = 90,
        nameGap = 0,
        axisLine = list(show = TRUE, lineStyle = list(color = "black")),
        axisLabel = list(fontSize = 10 , padding = c(0, 0, 0, 0)
        )
      ),
      tooltip = list(
        trigger = "axis",
        formatter = JS("
          function(params) {
            let result = params[0].name + '年<br>';
            params.forEach(function(item) {
              if (item.value !== null && item.value !== undefined && item.value !== '-') {
                result += item.marker + ' ' + item.seriesName + ': ' + item.value + '<br>';
              }
            });
            return result;
          }
        ")  
      ),
      legend = list(show=F),
      grid = list(top="9%", right="1%", bottom="1%", left="1%")
      # grid = list(right="1%", bottom="1%", left="1%")
    )
    return(p)
  })
  # 清空表格的选中行
  proxy <- dataTableProxy("table_jcr")                                   #创建datatable代理
  observe({proxy %>% selectRows(NULL)}) %>% bindEvent(input$clear_sel)   #清除选择
  # trend的前后按钮
  observe({
    trend_item_selected      <- input$jcr_trend_item
    trend_item_selected_rank <- which(trend_item==trend_item_selected)
    new_rank  <- max(1, trend_item_selected_rank-1)
    updatePickerInput(session, inputId="jcr_trend_item", choices=trend_item, selected=trend_item[new_rank]) #必须要有choices，否则失效
  }) %>% bindEvent(input$item_pre)
  observe({
    trend_item_selected      <- input$jcr_trend_item
    trend_item_selected_rank <- which(trend_item==trend_item_selected)
    new_rank  <- min(length(trend_item), trend_item_selected_rank+1)
    updatePickerInput(session, inputId="jcr_trend_item", choices=trend_item, selected=trend_item[new_rank]) #必须要有choices，否则失效
  }) %>% bindEvent(input$itme_nex)
  
  ########################
  #### 3. xr ####
  # sidebar
  output$sidebar_xr    <- renderUI({
    tagList(
      pickerInput(
        inputId  = "xr_item",
        label    = "展示列项目", 
        choices  = item_xr,
        selected = item_xr[c(1,2,3,6)],
        options  = pickerOptions(container="body", liveSearch=T, actionsBox=T, `selected-text-format`="count>3", `actions-box`=T),
        multiple = T
      ),
      h4(strong("属性筛选")),
      pickerInput(
        inputId  = "xr_cate",
        label    = "期刊分类", 
        choices  = cate_xr,
        options  = pickerOptions(container="body", liveSearch=T, actionsBox=T, `selected-text-format`="count>3", `actions-box`=T),
        multiple = T
      ),
      checkboxGroupButtons(
        inputId   = "xr_rank",
        label     = "分区", 
        choices   = rank_xr,
        selected  = rank_xr,
        status    = "primary",
        size      = 'xs',
        checkIcon = list(yes = icon("ok", lib="glyphicon"), no = icon("remove", lib="glyphicon"))
      ),
      h4(strong("查找期刊")),
      searchInput(
        inputId     = "xr_search_content",
        label       = NULL,
        placeholder = "查找...",
        btnSearch   = icon("magnifying-glass"),
        btnReset    = icon("xmark")
      ),
      div(
        style = "display: flex; justify-content: center; width: 100%; ",
        prettySwitch(
          inputId = "xr_search_precise",
          label   = "精确查找",
          status  = "primary",
          fill    = TRUE
        )
      )
    )
  })
  # main
  output$main_xr       <- renderUI({
    tagList(
      h3("新锐分区 2026年数据"),
      tags$hr(),
      DT::dataTableOutput("table_xr")
    )})
  output$table_xr      <- DT::renderDataTable({
    item <- input$xr_item
    ca <- input$xr_cate
    ra <- input$xr_rank
    if(length(ca)==0) ca <- cate_xr
    scarch_p <- input$xr_search_precise
    showdt   <- jdat_xr[Category %in% ca & Journal_ranking %in% ra, .SD, .SDcols=item] 
    
    # 查找期刊
    if(nzchar(input$xr_search_content)){
      if(scarch_p){
        showdt <- showdt[trimws(tolower(Journal_name)) == trimws(tolower(input$xr_search_content))]
      } else {
        showdt <- showdt[grepl(tolower(input$xr_search_content), tolower(Journal_name), fixed=TRUE)]
      }
    }
    
    # 输出表格
    session$userData$showdt_xr <- showdt
    DT::datatable(
      showdt, 
      escape = FALSE, 
      options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE),
      selection = list(mode = "multiple", target = "row")
    )
  })
  
  ########################  
  #### 4. letpub ####
  # sidebar
  output$sidebar_letpub <- renderUI({
    tagList(
      pickerInput(
        inputId  = "letpub_item",
        label    = "展示条目", 
        choices  = item_letpub,
        selected = item_letpub,
        options  = list(`selected-text-format` = "count>3", `actions-box` = TRUE),
        multiple = TRUE
      ),
      h4(strong("属性筛选")),
      checkboxGroupButtons(
        inputId   = "letpub_coll",
        label     = "1.收录情况",
        choices   = c("SCI", "SCIE", "SCI/SCIE", "无数据"),
        status    = "primary",
        size      = 'xs',
        checkIcon = list(yes=icon("ok", lib="glyphicon"), no=icon("remove", lib="glyphicon"))
      ),
      checkboxGroupButtons(
        inputId   = "letpub_divi",
        label     = "2.中科院分区",
        choices   = c("1区", "2区", "3区", "4区", "未录"),
        status    = "primary",
        size      = 'xs',
        checkIcon = list(yes=icon("ok", lib="glyphicon"), no=icon("remove", lib="glyphicon"))
      ),
      pickerInput(
        inputId  = "letpub_cate",
        label    = "3.分类", 
        choices  = sort(unique(jdat_letpub$Category)),
        options  = list(`selected-text-format` = "count > 3", `actions-box` = TRUE), 
        multiple = TRUE
      ),
      pickerInput(
        inputId  = "letpub_disc",
        label    = "4.学科", 
        choices  = sort(unique(jdat_letpub$Discipline)),
        options  = list(`selected-text-format` = "count>3", `actions-box` = TRUE), 
        multiple = TRUE
      ),
      checkboxGroupButtons(
        inputId   = "letpub_topj",
        label     = "5.Top期刊",
        choices   = c("是", "否", "无数据"),
        status    = "primary",
        size      = 'xs',
        checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove",lib = "glyphicon"))
      ),
      checkboxGroupButtons(
        inputId   = "letpub_revi",
        label     = "6.综述期刊",
        choices   = c("是", "否", "无数据"),
        status    = "primary",
        size      = 'xs',
        checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove",lib = "glyphicon"))
      ),
      pickerInput(
        inputId  = "letpub_freq",
        label    = "7.出版周期", 
        choices  = sort(unique(jdat_letpub$PublicationCycle)),
        options  = list(`selected-text-format`="count > 3",`actions-box`=TRUE), 
        multiple = TRUE
      ),
      pickerInput(
        inputId  = "letpub_loac",
        label    = "8.出版地区", 
        choices  = sort(unique(jdat_letpub$Region)),
        options  = list(`selected-text-format`="count > 3",`actions-box`=TRUE), 
        multiple = TRUE
      )
    )
  })
  # main
  output$main_letpub    <- renderUI({
    tagList(
      h3("LetPub 2018年数据"),
      tags$hr(),
      DT::dataTableOutput("table_letpub")
    )})
  output$table_letpub   <- DT::renderDataTable({
    if(length(input$letpub_item) != 0) ip1_item <- input$letpub_item else ip1_item <- item_letpub
    if(length(input$letpub_coll) != 0) ip1_coll <- input$letpub_coll else ip1_coll <- unique(jdat_letpub$IsSCI)
    if(length(input$letpub_divi) != 0) ip1_divi <- input$letpub_divi else ip1_divi <- unique(jdat_letpub$CASRanking)
    if(length(input$letpub_cate) != 0) ip1_cate <- input$letpub_cate else ip1_cate <- unique(jdat_letpub$Category)
    if(length(input$letpub_disc) != 0) ip1_disc <- input$letpub_disc else ip1_disc <- unique(jdat_letpub$Discipline)
    if(length(input$letpub_topj) != 0) ip1_topj <- input$letpub_topj else ip1_topj <- unique(jdat_letpub$IsTop)
    if(length(input$letpub_revi) != 0) ip1_revi <- input$letpub_revi else ip1_revi <- unique(jdat_letpub$IsReview)
    if(length(input$letpub_freq) != 0) ip1_freq <- input$letpub_freq else ip1_freq <- unique(jdat_letpub$PublicationCycle)
    if(length(input$letpub_loac) != 0) ip1_loac <- input$letpub_loac else ip1_loac <- unique(jdat_letpub$Region)
    showdt <- jdat_letpub[IsSCI %in% ip1_coll &
                            CASRanking %in% ip1_divi &
                            Category %in% ip1_cate &
                            Discipline %in% ip1_disc &
                            IsTop %in% ip1_topj &
                            IsReview %in% ip1_revi &
                            PublicationCycle %in% ip1_freq &
                            Region %in% ip1_loac,]
    session$userData$showdt_letpub <- showdt
    DT::datatable(
      showdt[, .SD, .SDcols = ip1_item], 
      escape = FALSE, 
      options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE),
      selection = list(mode = "multiple", target = "row")
    )
  })
  
  ########################  
  #### 5. cnki ####
  # sidebar
  output$sidebar_cnki <- renderUI({
    tagList(
      pickerInput(
        inputId  = "cnki_item",
        label    = "展示条目", 
        choices  = item_cnki,
        selected = item_cnki[c(3,6,7,8,10,13,16)],
        options  = list(`selected-text-format` = "count > 3", `actions-box` = TRUE),
        multiple = TRUE
      ),
      h4(strong("属性筛选")),
      pickerInput(
        inputId  = "cnki_cate1",
        label    = "1.一级分类", 
        choices  = cate_cnki_1,
        options  = list(`selected-text-format` = "count > 3", `actions-box` = TRUE),
        multiple = TRUE
      ),
      pickerInput(
        inputId  = "cnki_cate2",
        label    = "2.二级分类", 
        choices  = cate_cnki_2,
        options  = list(`selected-text-format` = "count > 3", `actions-box` = TRUE),
        multiple = TRUE
      ),
      pickerInput(
        inputId  = "cnki_eval",
        label    = "3.收录情况", 
        choices  = eval_cnki,
        options  = list(`selected-text-format` = "count > 3", `actions-box` = TRUE), 
        multiple = TRUE
      ),
      pickerInput(
        inputId  = "cnki_freq",
        label    = "4.出版周期", 
        choices  = freq_cnki,
        options  = list(`selected-text-format` = "count > 3", `actions-box` = TRUE),
        multiple = TRUE
      ),
      checkboxGroupButtons(
        inputId   = "cnki_lang",
        label     = "5.期刊语言",
        choices   = c("中文", "英文", "日文", "韩文"),
        status    = "primary",
        size      = 'xs',
        checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove",lib = "glyphicon"))
      ),
      checkboxGroupButtons(
        inputId   = "cnki_stat",
        label     = "6.出版状态",
        choices   = c("发行", "停刊", "合并"),
        status    = "primary",
        size      = 'xs',
        checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove",lib = "glyphicon"))
      ),
    )
  })
  # main
  output$main_cnki    <- renderUI({
    tagList(
      h3("CNKI 2022年数据"),
      tags$hr(),
      DT::dataTableOutput("table_cnki")
    )})
  output$table_cnki   <- DT::renderDataTable({
    if(length(input$cnki_item) != 0)  ip4_item  <- input$cnki_item  else ip4_item  <- item_cnki
    if(length(input$cnki_cate1) != 0) ip4_cate1 <- input$cnki_cate1 else ip4_cate1 <- cate_cnki_1
    if(length(input$cnki_cate2) != 0) ip4_cate2 <- input$cnki_cate2 else ip4_cate2 <- cate_cnki_2
    if(length(input$cnki_eval) != 0)  ip4_eval  <- input$cnki_eval  else ip4_eval  <- eval_cnki
    if(length(input$cnki_freq) != 0)  ip4_freq  <- input$cnki_freq  else ip4_freq  <- freq_cnki
    if(length(input$cnki_lang) != 0)  ip4_lang  <- input$cnki_lang  else ip4_lang  <- c("中文", "英文", "日文", "韩文")
    if(length(input$cnki_stat) != 0)  ip4_stat  <- input$cnki_stat  else ip4_stat  <- c("发行", "停刊", "合并")
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
    session$userData$showdt_cnki <- showdt
    DT::datatable(
      showdt[, .SD, .SDcols = ip4_item], 
      escape = FALSE, 
      options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE), 
      selection = list(mode = "multiple", target = "row")
    )
  })
  
  ########################
  #### 6. 对比 ####
  output$main_select <- renderUI({
    tagList(
      h3("已选期刊："),
      hr(),
      h4("JCR页："),
      DT::dataTableOutput("jdat_jcr"),
      hr(),
      h4("新锐分区页："),
      DT::dataTableOutput("jdat_xr"),
      hr(),
      h4("LetPub页："),
      DT::dataTableOutput("jdat_letpub"),
      hr(),
      h4("CNKI页："),
      DT::dataTableOutput("jdat_cnki")
    )
  })
  row_xr             <- reactive({input$table_xr_rows_selected})
  row_latpub         <- reactive({input$table_letpub_rows_selected})
  row_cnki           <- reactive({input$table_cnki_rows_selected})
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