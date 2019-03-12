
##########===== server =====##########
server <- function(input, output) {
  ########## load dataset ##########
  originaldata <- reactive({
    # jdata <- fread('https://raw.githubusercontent.com/lcpmgh/Journal/master/Journal.csv', stringsAsFactors=F)
    jdata <- fread('Journal.csv', stringsAsFactors=F)
    jdata <- dplyr::rename(jdata,"CiteScore(%)"="CiteScore")
    jdata[Is_SCI=="无",Is_SCI:="无数据"]
    jdata[Category=="",Category:="无数据"]
    jdata[Discipline=="",Discipline:="无数据"]
    jdata[is.na(IsTop),IsTop:="无数据"]
    jdata[is.na(IsReview),IsReview:="无数据"]
    jdata[is.na(Publication_Cycle),Publication_Cycle:="无数据"]
    jdata[is.na(Region),Region:="无数据"]
    return(jdata)
  })
  ########## module ##########
  output$table <- DT::renderDataTable({
    outda <- originaldata() %>% data.table()
    selcom <- input$inp_0
    if(length(input$inp_1)!=0) outda <- outda[Is_SCI %in% c(input$inp_1),]
    if(length(input$inp_2)!=0) outda <- outda[Division %in% input$inp_2,]
    if(length(input$inp_3)!=0) outda <- outda[Category %in% input$inp_3,]
    if(length(input$inp_4)!=0) outda <- outda[Discipline %in% input$inp_4,]
    if(length(input$inp_5)!=0) outda <- outda[IsTop %in% input$inp_5,]
    if(length(input$inp_6)!=0) outda <- outda[IsReview %in% input$inp_6,]
    if(length(input$inp_7)!=0) outda <- outda[Publication_Cycle %in% input$inp_7,]
    if(length(input$inp_8)!=0) outda <- outda[Region %in% input$inp_8,]
    outda <- if(length(input$inp_0)!=0) outda[,..selcom] else outda
    return(outda)
  })
}
