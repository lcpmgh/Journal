library(XML)
library(RSelenium)
library(rvest)
library(stringr)
library(data.table)
library(magrittr)
library(RCurl)
library(rlist)


########## 统计信息部分 ##########
# 爬虫环境配置，安装java环境，下载selenium放在如下所用目录
system("java -jar d:/selenium-server-standalone-3.12.0.jar", wait=F, invisible=T, minimized=T)

# 关闭无头模式，chrome和chromedriver版本要一致，
# chromedriver下载地址：http://npm.taobao.org/mirrors/chromedriver/
eCaps  <- list(phantomjs.page.settings.userAgent = "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:29.0) Gecko/20120101 Firefox/29.0")
remDr <- remoteDriver(browserName = "chrome", extraCapabilities = eCaps)
remDr$open()
url <- 'https://navi.cnki.net/knavi/all/index?uniplatform=NZKPT#'
remDr$navigate(url)
# 注意，在浏览器窗口中设置显示为表格模式，并手动切换期刊分类，每个分类模式下运行如下部分
jtable <- data.table()
while (T) {
  opage <- remDr$getPageSource()[[1]] %>% htmlParse() 
  jname <- xpathSApply(opage, "//span[@class='tab_1']//@title")
  jurl <- xpathSApply(opage, "//span[@class='tab_1']//@href") %>% 
    str_extract("eid=\\w+") %>% 
    str_remove("eid=") %>%
    paste0("https://navi.cnki.net/knavi/journals/",.,"/detail?uniplatform=NZKPT")
  jorganizer <- xpathSApply(opage, "//span[@class='tab_2']", xmlValue) %>% 
    str_remove_all("\n") %>% 
    str_trim() %>% 
    .[-1]
  jcif <- xpathSApply(opage, "//span[@class='tab_3']", xmlValue) %>% 
    str_remove_all("\n") %>% 
    str_trim() %>% 
    .[-1]
  jsif <- xpathSApply(opage, "//span[@class='tab_4']", xmlValue) %>% 
    str_remove_all("\n") %>% 
    str_trim() %>% 
    .[-1]
  jct <- xpathSApply(opage, "//span[@class='tab_5']", xmlValue) %>% 
    str_remove_all("\n") %>% 
    str_trim() %>% 
    .[-1]
  
  tjtable <- data.table(name = jname,
                        organizer = jorganizer,
                        cif = jcif,
                        sif = jsif,
                        ct = jct,
                        jurl = jurl)
  
  jtable <- rbind(jtable, tjtable)
  
  # 找到下一页的按钮并点击它，若没有，会出错，说明这一系列爬完
  nextpage <- remDr$findElement(using = "xpath", value = "//a[@class='next']")
  nextpage$clickElement()
  Sys.sleep(runif(1, min=1, max = 2))
}
# jtable为爬取的数据，数据处理部分省略（注意同一期刊可能归属不同类别，汇总后需剔除重复数据）


########## 详细信息部分 ##########
# 配置爬虫环境
system("java -jar d:/selenium-server-standalone-3.12.0.jar", wait=F, invisible=T, minimized=T)
# 使用非无头模式，无头模式不稳定有时无页面内容？
eCaps  <- list(phantomjs.page.settings.userAgent = "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:29.0) Gecko/20120101 Firefox/29.0")
# 使用chromedriver无头模式，chrome和chromedriver版本要一致，
# eCaps = list(chromeOptions = list(
#   args = c('--headless',
#            '--no-sandbox', 
#            '--disable-dev-shm-usage', 
#            '--disable-blink-features=AutomationControlled',
#            'user-agent=Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/61.0.3163.100 Safari/537.36'
#   )
# ))
remDr <- remoteDriver(browserName = "chrome", extraCapabilities = eCaps)
remDr$open()

# 整理要爬取的页面
detj <- fread("./jisummary.csv")
detj <- fread("./app/Journal/备份/jisummary.csv")

urlall <- detj$jurl

jiall <- data.table()
eiall <- data.table()

testurl <- urlall
# testurl <- urlall[-(1:11764)]
# testurl <- urlall[sample(1:12081, 10, replace = F)]


system.time({
n <- 1
  for(turl in testurl){
    tsta <- Sys.time() 
    # url <- 'https://navi.cnki.net/knavi/journals/YNDZ/detail?uniplatform=NZKPT'
    remDr$navigate(turl)
    opage <- remDr$getPageSource()[[1]] %>% htmlParse() 
    
    # 基本信息
    na1 <- xpathApply(opage, "//h3[@class='titbox titbox1']", xmlValue) %>% str_trim()
    na2 <- xpathApply(opage, "//dd[@class='infobox']/p", xmlValue)[[1]]
    
    biitem <- xpathSApply(opage, "//ul[@id='JournalBaseInfo']/li//label", xmlValue) 
    bivalue <- xpathSApply(opage, "//ul[@id='JournalBaseInfo']/li//span", xmlValue) 
    bi <- data.table(item = biitem, value = bivalue)
    
    # 发行信息
    piitem <- xpathSApply(opage, "//ul[@id='publishInfo']/li//label", xmlValue)
    pivalue <- xpathSApply(opage, "//ul[@id='publishInfo']/li//span", xmlValue)
    pi <- data.table(item = piitem, value = pivalue)
    
    # 评价信息
    eiitem <- xpathSApply(opage, "//p[@class='journalType journalType2']//span", xmlValue) %>% 
      str_trim() %>% 
      paste0(collapse = "/")
    tei <- xpathSApply(opage, "//ul[@id='evaluateInfo']//p[@class='database']", xmlValue) %>% 
      str_split(" ") %>% 
      list.rbind() %>% 
      as.data.table()
    
    # 信息汇总
    bis <- data.table(item = c("出版周期","ISSN","CN","出版地","语种","开本","邮发代号","创刊时间","目前状态","曾用刊名"))
    pis <- data.table(item = c("专辑名称","专题名称","出版文献量","总下载次数","总被引次数"))
    if(nrow(bi) == 0) {bi <- data.table(item=NA, value=NA); Sys.sleep(10)}
    if(nrow(pi) == 0) {pi <- data.table(item=NA, value=NA); Sys.sleep(10)}
    
    bic <- bi[bis, on = "item"]
    pic <- pi[pis, on = "item"]
    
    if(length(na1) == 0)  na1 <- NA_character_ else na1 <- na1
    if(length(na2) == 0)  na2 <- NA_character_ else na2 <- na2
    if(length(eiitem) == 0) ei  <- NA_character_ else ei <- eiitem
    
    tji <- data.table(
      name1     = na1,
      name2     = na2,
      oldname   = bic[item == "曾用刊名", value],
      frequency = bic[item == "出版周期", value],
      ISSN      = bic[item == "ISSN", value],
      CN        = bic[item == "CN", value],
      place     = bic[item == "出版地", value],
      language  = bic[item == "语种", value],
      format    = bic[item == "开本", value],
      postalcode = bic[item == "邮发代号", value],
      firstissue = bic[item == "创刊时间", value],
      status     = bic[item == "目前状态", value],
      category1 = pic[item == "专辑名称", value],
      category2 = pic[item == "专题名称", value],
      publishNO = str_extract(pic[item == "出版文献量", value], "\\d+"),
      downNO    = str_extract(pic[item == "总下载次数", value], "\\d+"),
      citeNO    = str_extract(pic[item == "总被引次数", value], "\\d+"),
      evaluate  = ei
    )
    
    jiall <- rbind(jiall, tji)
    eiall <- rbind(eiall, tei)
    
    curn <- n
    alln <- length(testurl)
    curc <- n/alln*100
    tend <- Sys.time()
    tspe <- as.numeric(tend-tsta)
    message(sprintf("当前页【%d】，已完成【%.2f%%】，剩余页【%d】，剩余【%.2f%%】,当前速度【%.4f sec/page】，剩余【%.0f sec, i.e. %.2 fmin】",
                    n, curc, alln-n, 100-curc, tspe, (alln-n)*tspe, (alln-n)*tspe/60))
    n <- n+1
    Sys.sleep(runif(1, min = 0.1, max = 0.5))
  }
})

# 经过去重，剩余11373种期刊, 详细数据处理过程略
