# Journal

学术期刊质量查询系统 （shinyapp在线版：[Journal](http://lcpmgh.com/Journal)）

收集中英文学术期刊信息，制作具有交互界面的查询系统，直观了解不同学术期刊的影响因子、分区、收录等信息，用于读文献时判断论文质量、投稿时选择目标期刊。此外本程序采用R语言编程，涉及爬虫和shiny等功能，可作为案例学习相关编程技能。

关于项目数据：中文期刊信息和2018年英文期刊信息采用R爬虫获取，分别源自知网和LetPub；2022JCR数据源自网络下载。

**所有代码和数据，切勿用于商业用途；适度使用爬虫工具，小心违法！**

===== 2022.08.13更新=====

+ 增加了选择期刊以进行对比的页面，使用时注意：
  
  + 在1~3页浏览时，单击表格中的期刊，可进行选择，选择结果将展示在第4页
  
  + 1~3页侧边栏，是对数据进行刷新，会重置第四页选择的结果，而search框中的选择，不改变数据，因此不会重置结果
  
  + 第四页仅做展示，没有可修改的参数项，为了期刊间对比，默认表格一页展示100条

+ 更改了原始数据journal2018.csv和journalchinese.csv中的url列，利用html语法定义为链接，则表格展示时仅出现“Link”字样，从而减小表格的占用的宽度

===== 2022.07.16更新===== 

增加了中文期刊的质量信息

对于中文爬虫，有几点需要注意：

+ 此次爬虫代码文件为#linshi5-WebCrawlerCNKIjournal.R
+ 本次爬虫采用selenium，所用R语言爬虫技巧可以应付绝大部分使用场景。
+ 对于爬虫环境搭建，主要包括Java环境、selenium-server、chromedriver三部分，网上相应教程较多。
+ Chromedriver无头模式不稳定，常有页面信息无反馈的情况，频率约2/1000，若是否用无头模式速度差别不大，则推荐不用headless。

===== 2022.07.13更新===== 

更新了JCR2022影响因子数据，由于该数据与LetPub的条目差异较大，因此并未删除原数据，两组数据同时展示。

===== 2022.02.30更新 =====  

修改了shinyapp，并将其部署在我自己的阿里云上，欢迎访问：[Journal](http://lcpmgh.com/Journal)，再也不用受制于shinyapps.io了。但是这个数据还是2018年的影响因子，实际上已经过时了，仅做参考吧，有时间再去更新数据。

===== 2019.03.12更新 =====  
用了几天Excel版，筛选和排序太过繁琐，查询起来也让人头疼，因此做了个shinyapp解决以上问题，此次将ui.r和server.r上传在23_journal_info文件夹中。

对于github上的网络版，配置好环境，在R中通过```shiny::runGitHub("Journal","lcpmgh",subdir="23_journal_info")```打开。本来想部署在shinyapps.io上，但是有中文乱码的bug，这里做了一个大幅度阉割的英文版[24_journal_info_en](https://lcpmgh.shinyapps.io/24_journal_info_en/)，就看看就行，连学科分类和期刊分区的筛选都没办法实现，等有机会把数据译成全英文的，再部署完整版。

本地使用时，建议将r加入环境变量，建立bat文件，代码类似```Rscript -e "library(shiny);runApp('D:/23_journal_info',launch.browser=T)"```（注意server.r中默认读取github上的数据），这样只需运行bat文件，省的每次都打开R，体验上更类似于一款独立的软件。

===== 以下为原readme内容 =====  
起因是想找一下优质期刊的论文看，然而查询中科院jcr分区太麻烦了，有个网站LetPub还能用，但是在我看来也不好用，看了下这个网站超级简陋，没有任何的反爬虫机制，还不如豆瓣电影，所以就写了个代码跑一下、

### **几个说明：**

1. 此次上传的内容包括，R爬虫脚本demo014_JournalInfo.r，以及抓取到的数据文件**Journal.csv**，**Journa_info.xlsx**，两个文件内容相同，都是LetPub全站的10235个期刊的23项信息。
2. 文件Journal.csv是代码直接输出的结果，Journa_info.xlsx是为了方便使用，在Excel中对csv文件进行转换，然后简单美化后的结果（R直接写入Excel要调用Java，效率太低了）。
3. 由于网站排版不是很规范，而且样本量太大，具体信息可能有误，参考时请慎重，若对某一项有疑问，可根据对应的Url核实，Url会连接到LetPub上该期刊的详情页面。
4. 特别说明，网络爬虫很容易触犯法律，因此 **本项目仅供学习交流，切勿作商业用途，由此造成的一切后果请自行承担！**

另外，之前电脑刚好重装系统，于是所有的东西都需要重新配置，在此也说明下此次R爬虫的准备工作：

1. r和rstudio，目前r v3.5.2，rstudio v1.1.463，r3.5版不能用3.4的旧包，所有都要重装，此版本rstudio不时会有bug，重启下就行了
2. jdk v11.0.2，windows10专业版1809，之前1703不能装最新的jdk，这次1809又不能用之前的jdk8，另外1809的桌面新建功能卡顿，貌似目前还没有解决办法 
3. phantomjs和chromedriver，去网上下载，加入环境变量中使用，由于此次爬虫过于简单，用phantomjs就行，并且因为不显示内容，速度比chrome要快不少
4. selenium server，v2.50.1，一个java的脚本，高版本的不支持phantomjs了，这个还能用
5. 此次所用r包，都位于CRAN上，直接装

此次运行只记录了第二个函数的时间，耗时两个多小时……
![函数crawler_jcr_2运行时间](https://github.com/lcpmgh/Journal/blob/master/timeconsuming.png "函数crawler_jcr_2运行时间")
