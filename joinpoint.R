rm(list = ls())
setwd("E:\\projects\\Rprograms\\Lab\\IBD\\jointpoint")
THA<-read.csv('../data/ibd/IHME-GBD_2021_DATA-16854fe6-1.csv')
options(scipen = 200)
library(dplyr)

unique(THA$measure)
THA$measure <- str_replace(THA$measure,"DALYs (Disability-Adjusted Life Years)","DALYs")

# THA <- THA %>%
#   filter(age == 'Age-standardized',
#          metric == 'Rate',
#          location =="India",
#          measure ==  "DALYs"  ) %>%
#   mutate(se=(upper-lower)/(2*1.96))%>%
#   arrange(sex, year) 


THA <- THA %>%
  filter(age == 'Age-standardized',
         metric == 'Rate',
         sex =="Both",
         measure ==  "DALYs (Disability-Adjusted Life Years)"  ) %>%
  mutate(se=(upper-lower)/(2*1.96))%>%
  arrange(location, year) #


write.csv(THA,"DALYs_jp.csv")




#https://surveillance.cancer.gov/joinpoint/download

APC<-read.table("export.Export.APC.txt",header = T)
APC<-APC[,c(1,4:8,11)]
APC$measure<-'APC'
colnames(APC)<-c("sex","Start","End","val","lower","upper","P.Value","measure" )

AAPC<-read.table("export.Export.AAPC.txt",header = T)
AAPC<-AAPC[,c(1,4:8,11)]

AAPC$measure<-'AAPC'
colnames(AAPC)<-c("sex","Start","End","val","lower","upper","P.Value","measure" )

ASPR<-rbind(AAPC,APC)
write.csv(ASPR,"DALYs.csv")
