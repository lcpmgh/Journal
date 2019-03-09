# Journal
R爬虫 提取LetPub上的期刊信息

起因是想找一下优质期刊的论文看，然而查询中科院jcr分区太麻烦了，有个网站LetPub还能用，但是在我看来也不好用，看了下这个网站超级简陋，没有任何的反爬虫机制，还不如豆瓣电影，所以就写了个代码跑一下、

之前电脑刚好重装系统，于是所有的东西都需要重新配置，在此也说明下此次R爬虫的准备工作：
  1. r和rstudio，目前r v3.5.2，rstudio v1.1.463，r3.5版不能用3.4的旧包，所有都要重装，此版本rstudio不时会有bug，重启下就行了
  2. jdk v11.0.2，windows10专业版1809，之前1703不能装最新的jdk，这次1809又不能用之前的jdk8，另外1809的桌面新建功能卡顿，貌似目前还没有解决办法 
  3. phantomjs和chromedriver，去网上下载，加入环境变量中使用，由于此次爬虫过于简单，用phantomjs就行，并且因为不显示内容，速度比chrome要快不少
  4. selenium server，v2.50.1，一个java的脚本，高版本的不支持phantomjs了，这个还能用
  5. 此次所用r包，都位于CRAN上，直接装

这次上传的内容包括，R爬虫脚本demo014_JournalInfo.r，以及运行后生成的数据文件**Journal.csv**和**Journal1.xlsx**，两个文件内容相同，都是LetPub全站的10235个期刊的23项信息。

此次运行只记录了第二个函数的时间，耗时两个多小时……
![crawler_jcr_2运行时间](https://github.com/lcpmgh/Journal/blob/master/timeconsuming.png)
