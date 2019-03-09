# letput journal
# 2019-03-08

library(XML)
library(RSelenium)
library(rvest)
library(stringr)
library(data.table)
library(magrittr)
library(xlsx)

####################========== PART 1 Create the crawler function ==========####################
crawler_jcr_1 <- function(){
  # Do not read string as a factor!
  options(stringsAsFactors=F)
  # load selenium service
  system("java -jar d:/selenium-server-standalone-2.50.1.jar", wait=F, invisible=T, minimized=T)
  # Configure the browser
  eCap  <- list(phantomjs.page.settings.userAgent="Mozilla/5.0 (Windows NT 6.1; WOW64; rv:29.0) Gecko/20120101 Firefox/29.0")
  # remDr <- remoteDriver(browserName="chrome", extraCapabilities=eCap)
  remDr <- remoteDriver(browserName="phantomjs", extraCapabilities=eCap)
  # Open and parse the initial page
  remDr$open()
  u <- 'https://www.letpub.com.cn/index.php?page=journalapp&fieldtag=&firstletter=&currentpage=1#journallisttable'
  remDr$navigate(u)
  opage <- remDr$getPageSource()[[1]] %>% read_html(encoding ="UTF-8") %>% htmlParse(encoding='UTF-8')
  nurl  <- xpathSApply(opage,"//form[@name='jumppageform']",xmlValue) %>% str_extract('\\d+') %>% as.numeric() %>% 1:.
  aurl  <- paste0('https://www.letpub.com.cn/index.php?page=journalapp&fieldtag=&firstletter=&currentpage=',nurl,'#journallisttable')
  tn <- 1
  an <- length(aurl)
  atabl1 <- NULL
  tablcol <- c('ISSN','Journal','IF2018','Division','Category','Discipline','Is_SCI','OA','Employment','Refereeing','View')
  for(turl in aurl){
    remDr$navigate(turl)
    tpage <- remDr$getPageSource()[[1]] %>% read_html(encoding ="UTF-8") %>% htmlParse(encoding='UTF-8')
    ttabl <- xpathSApply(tpage,"//table[@class='table_yjfx']")[[2]] %>% readHTMLTable(header=T) %>% set_colnames(.[1,]) %>% data.table() %>% .[c(-1,-.N),-11]
    names(ttabl)  <- tablcol
    tabbr <- ttabl$Journal
    ttabl$Journal <- xpathSApply(tpage,"//table[@class='table_yjfx']/tbody/tr/td/a[@target='_blank']",xmlValue) %>% .[as.logical(1:length(.)%%2)]
    ttabl$Abbr_Name <- str_remove(tabbr,ttabl$Journal)
    ttabl$Url     <- xpathSApply(tpage,"//table[@class='table_yjfx']/tbody/tr/td/a[@target='_blank']/@href") %>% str_remove('./') %>% paste0('https://www.letpub.com.cn/',.) %>% .[as.logical(1:length(.)%%2)]
    ttabl[Is_SCI=='SCISCIE',Is_SCI:='SCI/SCIE']
    atabl1 <- rbind(atabl1,ttabl)
    cat(sprintf("Summary page, [%d] pages has been crawled, a total of %d, [%.4f%%] completed",tn,an,tn/an*100),'\n')
    tn <- tn+1
  }
  return(atabl1)
}
crawler_jcr_2 <- function(atabl1){
  # for detail page
  options(stringsAsFactors=F)
  system("java -jar d:/selenium-server-standalone-2.50.1.jar", wait=F, invisible=T, minimized=T)
  eCap  <- list(phantomjs.page.settings.userAgent="Mozilla/5.0 (Windows NT 6.1; WOW64; rv:29.0) Gecko/20120101 Firefox/29.0")
  # remDr <- remoteDriver(browserName="chrome", extraCapabilities=eCap)
  remDr <- remoteDriver(browserName="phantomjs", extraCapabilities=eCap)
  remDr$open()
  ttabl2 <- data.table(Self_citing=NA, 
                       IF_5year=NA, 
                       H_Index=NA, 
                       CiteScore=NA, 
                       Region=NA, 
                       Publication_Cycle=NA,
                       Publication_year=NA, 
                       Article_NO._per_year=NA,
                       IsTop=NA, 
                       IsReview=NA)
  atabl2 <- ttabl2
  tn <- 1
  an <- length(atabl1$Url)
  for(tdurl in atabl1$Url){
    remDr$navigate(tdurl)
    dpage      <- remDr$getPageSource()[[1]] %>% read_html(encoding ="UTF-8") %>% htmlParse(encoding='UTF-8')
    ddpage_val <- xpathSApply(dpage,"//table[@class='table_yjfx']/tbody/tr",xmlValue)
    ddpage_xml <- xpathSApply(dpage,"//table[@class='table_yjfx']/tbody/tr")
    try({ttabl2$Self_citing           <- ddpage_val[str_detect(ddpage_val,'2017-2018自引率')] %>% str_extract("\\n\\d.+\\n") %>% str_trim() %>% str_remove("\\%") %>% as.numeric()},silent=T)
    try({ttabl2$IF_5year              <- ddpage_val[str_detect(ddpage_val,'五年IF')] %>% str_extract("\\n\\d.+\\n") %>% str_trim() %>% as.numeric()},silent=T)
    try({ttabl2$H_Index               <- ddpage_val[str_detect(ddpage_val,'h-index')] %>% str_extract("\\n\\d.+\\n") %>% str_trim() %>% as.numeric()},silent=T)
    try({ttabl2$CiteScore             <- ddpage_val[str_detect(ddpage_val,'CiteScore')] %>% str_extract("排名\\n.+\\n") %>% str_extract("\\d.+\\d$") %>% as.numeric()},silent=T)
    try({ttabl2$Region                <- ddpage_val[str_detect(ddpage_val,'出版国家或地区')] %>% str_extract("\\n.+\\n") %>% str_trim()},silent=T)
    try({ttabl2$Publication_Cycle     <- ddpage_val[str_detect(ddpage_val,'出版周期')] %>% str_extract("\\n.+\\n") %>% str_trim()},silent=T)
    try({ttabl2$Publication_year      <- ddpage_val[str_detect(ddpage_val,'出版年份')] %>% str_extract("\\n.+\\n") %>% str_trim() %>% as.numeric()},silent=T)
    try({ttabl2$Article_NO._per_year  <- ddpage_val[str_detect(ddpage_val,'年文章数')] %>% str_extract("\\n\\d+") %>% str_trim()},silent=T)
    try({ttabl2$IsTop                 <- ddpage_xml[str_detect(ddpage_val,'中科院SCI期刊分区')][[1]] %>% readHTMLTable(header=T) %>% .[1,'Top期刊']},silent=T)
    try({ttabl2$IsReview              <- ddpage_xml[str_detect(ddpage_val,'中科院SCI期刊分区')][[1]] %>% readHTMLTable(header=T) %>% .[1,'综述期刊']},silent=T)
    if(is.null(ttabl2$Self_citing))            ttabl2$Self_citing            <- NA
    if(is.null(ttabl2$IF_5year))               ttabl2$IF_5year               <- NA
    if(is.null(ttabl2$H_Index))                ttabl2$H_Index                <- NA
    if(is.null(ttabl2$CiteScore))              ttabl2$CiteScore              <- NA
    if(is.null(ttabl2$Region))                 ttabl2$Region                 <- NA
    if(is.null(ttabl2$Publication_Cycle))      ttabl2$Publication_Cycle      <- NA
    if(is.null(ttabl2$Publication_year))       ttabl2$Publication_year       <- NA
    if(is.null(ttabl2$Article_NO._per_year))   ttabl2$Article_NO._last_year  <- NA
    if(is.null(ttabl2$IsTop))                  ttabl2$IsTop                  <- NA
    if(is.null(ttabl2$IsReview))               ttabl2$IsReview               <- NA
    atabl2 <- rbind(atabl2,ttabl2)
    cat(sprintf("Details page，[%d] pages has been crawled, a total of %d, [%.4f%%] completed",tn,an,tn/an*100),'\n')
    tn <- tn+1
  }
  atabl2 <- atabl2[-1,]
  atabl <- cbind(atabl1,atabl2)
  return(atabl)
}


####################========== PART 2 Run crawler function and store the data ==========####################
dataset <- crawler_jcr_1()
write.csv(dataset, 'dataset.csv', row.names=F)
system.time({gds <- crawler_jcr_2(dataset[])})
# Colnames sort
cona <- c("ISSN","Journal","Abbr_Name","Is_SCI","IF2018","IF_5year","Self_citing","H_Index","CiteScore","Division","Category",
          "Discipline","IsTop","IsReview","Publication_Cycle","Article_NO._per_year","Publication_year","Region","OA",
          "Employment","Refereeing","View","Url")
gds <- gds[,..cona]
write.csv(gds,'Journal.csv',row.names=F)
# write.xlsx(gds,'Journal.xlsx',row.names=F)  #Inefficiency
