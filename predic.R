library(BAPC)
library(dplyr)
library(stringr)

rm(list = ls())
setwd("E:\\projects\\Rprograms\\Lab\\IBD\\预测\\BAPC")

BC<-read.csv("../../data/ibd/IHME-GBD_2021_DATA-16854fe6-1.csv")
BC$age <- str_replace(BC$age," years","")
BC$age <- str_replace(BC$age,"<5","0-4")
unique(BC$age)

#### 预测的年龄结构
ages_group <- c("0-4","5-9","10-14","15-19","20-24","25-29", "30-34","35-39","40-44","45-49",
                "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+")


#----1.读取标准化年龄-----
#人口结构化 WHO2000-2025 
age_stand <- read.csv("WHO 2000-2025 new World Standard Population.csv")
unique(age_stand$std_population)


age_stand <- age_stand%>%filter(Age.group%in%ages_group)%>%mutate(Age.group=factor(Age.group,levels = ages_group ,ordered = T))%>%arrange(Age.group)
wstand <- age_stand$std_population
wstand 

#----2.读取疾病数据-----
unique(BC$location)
BC_Incidence_Female<-subset(BC,
                            BC$metric=="Number" &
                            BC$measure=="Deaths"&
                            BC$sex=="Male" &
                            BC$location=="China")

#View(BC_Incidence_Female)

##转化数据  长转宽
library(reshape)
BC_Incidence_Female_n <- reshape2::dcast(data = BC_Incidence_Female,year~age,value.var="val")
#View(BC_Incidence_Female_n)
BC_Incidence_Female_n<-BC_Incidence_Female_n[,-c(1,22)]
BC_Incidence_Female_n <- BC_Incidence_Female_n[,c(1,10,2:9,11:20)]#32行

BC_Incidence_Female_n<-BC_Incidence_Female_n%>%
                       apply(c(1,2),as.numeric)%>%
                       apply(c(1,2),round)%>%
                       as.data.frame()
#View(BC_Incidence_Female_n)
#BC_Incidence_Female_n <- BC_Incidence_Female_n[,]
#----3.读取人口学数据-----
library(purrr)
setwd("E:\\projects\\Rprograms\\Lab\\IBD\\data\\ibd\\GBD_population")
files <- list.files()
files

Population <- map_dfr(files,read.csv)
Population <- Population[,c(4,6,8,10,11,12)]

#Population_China <- Population[which(Population$location=="China"),]
#unique(Population_China$year)
colnames(Population)<-c("location","sex","age","metrix","year","val")
Population$sex[Population$sex=="male"]<-"Male"
Population$sex[Population$sex=="female"]<-"Female"
Population$sex[Population$sex=="both"]<-"Both"


Population <- Population%>%mutate(age=sub(" to ",replacement = "-",age))
Population <- Population%>%mutate(age=sub(" years",replacement = "",age))
Population <- Population%>%mutate(age=sub("<5",replacement = "0-4",age))
unique(Population$age) 


Population <- subset(Population,
                     Population$age%in%
                     c("0-4","5-9","10-14","15-19","20-24",
                       "25-29","30-34","35-39","40-44","45-49","50-54",
                       "55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+" ))


#----4.读取预测人口数据----
#https://ghdx.healthdata.org/record/ihme-data/global-population-forecasts-2017-2100
setwd("E:\\projects\\Rprograms\\Lab\\IBD\\预测\\BAPC")
GBD_population_prediction <- read.csv("IHME_POP_2017_2100_POP_REFERENCE_Y2020M05D01.csv")
GBD_population_prediction <- subset(GBD_population_prediction,
                                    GBD_population_prediction$year_id %in% 2022:2040)

#unique(GBD_population_prediction$sex)
#名称也是United States of America

GBD_population_prediction <- GBD_population_prediction[ ,c(2,4,6,7,11,14)]
colnames(GBD_population_prediction)<-c("location","sex","age","year","metrix","val")
#unique(GBD_population_prediction$age)

###处理并合并预测人口<1 year的人口数据
GBD_population_prediction <- GBD_population_prediction%>%mutate(age=sub(" to ",replacement = "-",age))
GBD_population_prediction <- GBD_population_prediction%>%mutate(age=sub("95 plus",replacement = "95+",age))

GBD_population_prediction_1year <- GBD_population_prediction %>% filter(age %in% c("Early Neonatal","Late Neonatal" ,"Post Neonatal"))%>%
                                   group_by(location,sex,year,metrix) %>% 
                                   summarise(val=sum(val)) %>% 
                                   mutate(age="<1 year")

#View(GBD_population_prediction_1year)
#unique(GBD_population_prediction_1year$age)

GBD_population_prediction <- subset(GBD_population_prediction,
                                    !(age %in% c("Early Neonatal","Late Neonatal" ,"Post Neonatal")))
#unique(GBD_population_prediction$age)

#合并
GBD_population_prediction <- rbind(GBD_population_prediction,GBD_population_prediction_1year)


GBD_population_prediction_age4<- GBD_population_prediction %>% filter(age %in% c("<1 year","1-4"))%>% 
  group_by(location,sex,year,metrix)%>%
  summarise(val=sum(val)) %>% 
  mutate(age="0-4")
#unique(GBD_population_prediction_age4$age)

GBD_population_prediction <- subset(GBD_population_prediction,
                                    !(age %in% c("<1 year","1-4")))
#View(GBD_population_prediction)

GBD_population_prediction <- rbind(GBD_population_prediction,GBD_population_prediction_age4)
GBD_population_prediction <- subset(GBD_population_prediction,
                                    age!="All Ages")

GBD_population_prediction <- GBD_population_prediction[,c(1,2,3,5,4,6)]


GBD <- rbind(Population,GBD_population_prediction)


GBD <- GBD %>% filter(age %in% ages_group) %>% 
                mutate(age=factor(age,levels = ages_group,ordered = T)) %>% 
                arrange(age)



GBD_Global_Female <- GBD %>% filter(location=="China"&sex=="Male")%>% filter(age %in% ages_group)
#View(GBD_Global_Female)
#长转宽
library(reshape)
GBD_Global_Female_n <- reshape2::dcast(data=GBD_Global_Female,year~age,value.var = "val")#年龄为行 年份为列
GBD_Global_Female_n <- GBD_Global_Female_n[,-1]


#View(GBD_Global_Female_n)

#----正式分析----
#BAPC模型要求疾病行列和人口数据保持一致
BC_predicted <- matrix(data = NA,nrow=(2040-2021),ncol=ncol(GBD_Global_Female_n)) %>% as.data.frame()
rownames(BC_predicted)<-seq(2022,2040,1)
colnames(BC_predicted)<-ages_group
#View(BC_predicted)


BC_Incidence_Female_n <- rbind(BC_Incidence_Female_n,BC_predicted)

rownames(BC_Incidence_Female_n)<-seq(1990,2040,1)
rownames(GBD_Global_Female_n)<-seq(1990,2040,1)
#View(GBD_Global_Female_n)

GBD_Global_Female_n<-GBD_Global_Female_n%>%
  apply(c(1,2),as.numeric)%>%
  apply(c(1,2),round)%>%
  as.data.frame()


library(INLA)

Female_esoph<-APCList(BC_Incidence_Female_n,GBD_Global_Female_n,gf=5)#gf=5表示年龄按5年计算
Female_bapc_result <- BAPC(Female_esoph,predict = list(npredict=19,retro=T),secondDiff=FALSE,stdweight = wstand,verbose = F)#npredict=9表示2022-2030年9年的数据


#----校正年龄后----
##----标准发病数----
#----校正年龄后----
##----标准发病数----
Female_ASR_number <- agestd.proj(x=Female_bapc_result)%>%as.data.frame()
Female_ASR_number$year <- 1990:2040

##----标准发病率rate---- 
####女性
Female_ASR_rate <- agestd.rate(x=Female_bapc_result)%>%as.data.frame()
Female_ASR_rate$mean <- Female_ASR_rate$mean*100000
Female_ASR_rate$year <- rownames(Female_ASR_rate)

#----预测区间----
Female_bapc_result <- qapc(Female_bapc_result,percentiles = c(0.025,0.975))
##获取标准化发病率及95%CI
Female_ASR_rate <- agestd.rate(x=Female_bapc_result)%>%as.data.frame()*10^5
Female_ASR_rate$year <- 1990:2040
#View(Female_ASR_rate)
write.csv(Female_ASR_rate,"China_Male_D.csv")

change <- (Female_ASR_rate[51,1]-Female_ASR_rate[33,1])/Female_ASR_rate[33,1]#示例 2030年(相对于2020年)的change

change

setwd("E:\\projects\\Rprograms\\Lab\\IBD\\预测\\BAPC")
save.image("IBD_predict.Rdata")
