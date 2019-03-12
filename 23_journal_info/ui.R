# Visualization of Journal information

##########===== ui =====##########
# load packages
library(DT)
library(shiny)
library(dplyr)
library(magrittr)
library(data.table)
library(shinyWidgets)

ui <- fluidPage(
  h3("期刊信息查询系统",align="center"),
  tags$hr(),
  sidebarLayout(
    sidebarPanel(width=3,
                 pickerInput(
                   inputId = "inp_0",
                   label = "选择要显示的条目", 
                   choices = c("ISSN","Journal","Abbr_Name","Is_SCI","IF2018","IF_5year","Self_citing","H_Index","CiteScore(%)","Division","Category",
                               "Discipline","IsTop","IsReview","Publication_Cycle","Article_NO._per_year","Publication_year","Region","OA",
                               "Employment","Refereeing","View","Url"),
                   selected = c("ISSN","Journal","Abbr_Name","Is_SCI","IF2018","IF_5year","Self_citing","H_Index","CiteScore(%)","Division","Category",
                                "Discipline","IsTop","IsReview","Publication_Cycle","Article_NO._per_year","Publication_year","Region","OA",
                                "Employment","Refereeing","View","Url"),
                   options = list(`selected-text-format`="count > 3",`actions-box`=TRUE),
                   multiple = TRUE
                 ),
                 
                 tags$hr(),
                 h3(strong("筛选项目")),
                 checkboxGroupButtons(
                   inputId = "inp_1",
                   label = "1.SCI或SCIE",
                   choices = c("SCI", "SCIE","SCI/SCIE","无数据"),
                   status = "primary",
                   checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove",lib = "glyphicon")),
                   size='xs'),
                 
                 checkboxGroupButtons(
                   inputId = "inp_2",
                   label = "2.中科院分区",
                   choices = c("1区", "2区", "3区","4区","未录"),
                   status = "primary",
                   checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove",lib = "glyphicon")),
                   size='xs'),
                 
                 pickerInput(
                   inputId = "inp_3",
                   label = "3.大类", 
                   choices = sort(c("管理科学","工程技术","医学","无数据","数学","化学","物理","生物","农林科学","地学天文","地学","环境科学与生态学","综合性期刊","社会科学")),
                   selected = c(),
                   options = list(`selected-text-format`="count > 3",`actions-box`=TRUE),
                   multiple = TRUE),
                 
                 pickerInput(
                   inputId = "inp_4",
                   label = "4.学科", 
                   choices = sort(c("运筹学与管理科学","地球科学综合","药学","应用化学",       
                                    "无数据","数学","急救医学","学科教育",
                                    "儿科","核医学","医学：伦理","化学综合",
                                    "分析化学","结构与建筑技术","计算机：理论方法","计算机：硬件",
                                    "计算机：软件工程","计算机：人工智能","计算机：控制论","计算机：信息系统",
                                    "计算机：跨学科应用","声学","材料科学：综合","生化与分子生物学",
                                    "运动科学","海洋与淡水生物学","奶制品与动物科学","农艺学",
                                    "食品科技","麻醉学","应用数学","工程：宇航",
                                    "天文与天体物理","植物科学","生物学","工程：生物医学",
                                    "医学实验技术","数学与计算生物学","行为科学","心脏和心血管系统",
                                    "动物学","骨科","外科","医学：内科",
                                    "皮肤病学","内分泌学与代谢","昆虫学","地球化学与地球物理",
                                    "自然地理","地质学","工程：地质","血液学",
                                    "细胞生物学","渔业","力学","工程：机械",
                                    "医学：研究与实验","冶金工程","免疫学","显微镜技术",
                                    "神经科学","临床神经病学","妇产科学","海洋学",
                                    "牙科与口腔外科","生态学","生物物理","肿瘤学",
                                    "眼科学","鸟类学","耳鼻喉科学","古生物学",
                                    "寄生虫学","护理","物理：综合","物理化学",
                                    "生理学","高分子科学","工程：综合","微生物学",
                                    "精神病学","风湿病学","兽医学","园艺",
                                    "综合性期刊","病毒学","解剖学与形态学","泌尿学与肾脏学",
                                    "全科医学与补充医学","康复医学","药物滥用","材料科学：复合",
                                    "工程：化工","机器人学","病理学","材料科学：硅酸盐",
                                    "生物工程与应用微生物","统计学与概率论","气象与大气科学","光学",
                                    "物理：原子、分子和化学物理","数学跨学科应用","遗传学","有机化学",
                                    "无机化学与核化学","物理：凝聚态物理","物理：数学物理","水资源",
                                    "环境科学","工程：电子与电气","老年医学","农业经济与政策",
                                    "农业综合","传染病学","公共卫生、环境卫生与职业卫生","逻辑学",
                                    "生化研究方法","胃肠肝病学","过敏","林学",
                                    "农业工程","工程：环境","科学史与科学哲学","营养学",
                                    "危重病医学","医学：法","卫生保健与服务","外周血管病",
                                    "进化生物学","生物多样性保护","心理学","男科学",
                                    "湖沼学","核科学技术","电信学","光谱学",
                                    "物理：核物理","自动化与控制系统","药物化学","材料科学：纸与木材",
                                    "能源与燃料","工程：工业","工程：大洋","物理：应用",
                                    "土壤科学","仪器仪表","工程：土木","材料科学：表征与测试",
                                    "矿业与矿物加工","毒理学","呼吸系统","听力学与言语病理学",
                                    "生殖生物学","热带医学","发育生物学","医学：信息",
                                    "神经成像","工程：海洋","矿物学","细胞与组织工程",
                                    "材料科学：生物材料","电化学","工程：制造","物理：流体与等离子体",
                                    "真菌学","晶体学","移植","物理：粒子与场物理",
                                    "材料科学：纺织","遥感","热力学","运输科技",
                                    "成像科学与照相技术","纳米科技","材料科学：膜","工程：石油",
                                    "初级卫生保健")),
                   selected = c(),
                   options = list(`selected-text-format`="count > 3",`actions-box`=TRUE),
                   multiple = TRUE),
                 
                 checkboxGroupButtons(
                   inputId = "inp_5",
                   label = "5.Top期刊",
                   choices = c("是", "否","无数据"),
                   status = "primary",
                   checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove",lib = "glyphicon")),
                   size='xs'),
                 
                 checkboxGroupButtons(
                   inputId = "inp_6",
                   label = "6.综述期刊",
                   choices = c("是", "否","无数据"),
                   status = "primary",
                   checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove",lib = "glyphicon")),
                   size='xs'),
                 
                 pickerInput(
                   inputId = "inp_7",
                   label = "7.出版周期", 
                   choices = sort(c("Quarterly","Monthly","Bimonthly","Annual","Semiannual",
                                    "Tri-annual","Semimonthly","Irregular","无数据","Two no. a year",
                                    "Weekly","Biweekly","Bi-monthly","bimonthly","Continuously updated",
                                    "Twelve no. a year","Six no. a year, 2004-","4 issues a year, 2010-","3 no. a year, 2003-","Six no. a year",
                                    "Quarterly, <1998->","Four issues a year","Quarterly, 2013-")),
                   selected = c(),
                   options = list(`selected-text-format`="count > 3",`actions-box`=TRUE),
                   multiple = TRUE),
                 
                 pickerInput(
                   inputId = "inp_8",
                   label = "8.出版地区", 
                   choices = sort(c("GERMANY","UNITED STATES","无数据","RUSSIA","AUSTRALIA","CROATIA",
                                    "ENGLAND","NORWAY","HUNGARY","DENMARK","NETHERLANDS","POLAND",
                                    "PEOPLES R CHINA","Chile","ARGENTINA","BRAZIL","MEXICO","BELGIUM",
                                    "TAIWAN","SLOVENIA","CZECH REPUBLIC","ITALY","ROMANIA","FRANCE",
                                    "SWITZERLAND","JAPAN","SWEDEN","AUSTRIA","PORTUGAL","VENEZUELA",
                                    "SLOVAKIA","TURKEY","Brazil","SERBIA","BULGARIA","SPAIN",
                                    "SINGAPORE","United States","INDIA","China","SOUTH AFRICA","UGANDA",
                                    "NIGERIA","KENYA","IRELAND","FINLAND","NEW ZEALAND","Spanish",
                                    "CHILE","SOUTH KOREA","India","SAUDI ARABIA","U ARAB EMIRATES","GREECE",
                                    "AZERBAIJAN","CANADA","COLOMBIA","BAHRAIN","IRAN","PHILIPPINES",
                                    "MALAYSIA","THAILAND","ARMENIA","Australia","MACEDONIA","LITHUANIA",
                                    "BANGLADESH","ISRAEL","BOSNIA & HERCEG","ETHIOPIA","SCOTLAND","LATVIA",
                                    "UZBEKISTAN","UKRAINE","Romania","ALLSA","US","USA",
                                    "Japan","Czech","Berlin Heidelberg","Khayyam","EGYPT","WALES",
                                    "ESTONIA","BYELARUS","ICELAND","URUGUAY","JORDAN","PAKISTAN",
                                    "Karachi","NEPAL","SRI LANKA","china","Germany","Ukraine",
                                    "Korea","Kuwait","Iceland","Italy","Libya","Russian Federation",
                                    "Spain","México","United Kingdom","Malaysia","Rio de Janeiro","Bucuresti",
                                    "Washington Luiz","Croatia","France","KUWAIT","Greece","Poland",
                                    "Austria","Turkey","Nigeria","Serbia","South Korea","Netherlands",
                                    "Pakistan","England","2002","Singapore","Hungary","Switzerland",
                                    "Puerto Rico","COSTA RICA","ECUADOR","JAMAICA","Korea (South)","New Zealand",
                                    "Mexico","Germany?","REP OF GEORGIA","MALAWI")),
                   selected = c(),
                   options = list(`selected-text-format`="count > 3",`actions-box`=TRUE),
                   multiple = TRUE)
    ),
    mainPanel(DT::dataTableOutput("table"))
  )
)

