rm(list = ls())

setwd('E:\\projects\\Rprograms\\Lab\\IBD\\可视化')
data=read.csv('../data/ibd/IHME-GBD_2021_DATA-5fb10659-1.csv')
View(data)

data=gbd_filter(data,year=="2021",metric=='Rate')
data=gbd_select_age_group(data,keep_original_format = TRUE)


data$location <- factor(data$location,
                      levels = c("United States of America", "China","India"))

unique(data$age)
data$measure <- factor(data$measure,
                     levels = c("Deaths","DALYs ","Prevalence","Incidence"))

age_groups <- c("<5 years", "5-9 years", "10-14 years", "15-19 years", "20-24 years",
                "25-29 years", "30-34 years", "35-39 years","40-44 years", "45-49 years",
                "50-54 years", "55-59 years", "60-64 years","65-69 years", "70-74 years",
                "75-79 years", "80-84 years", "85-89 years","90-94 years","95+ years")


data$age <- factor(data$age, levels = age_groups, ordered = TRUE)
data[!complete.cases(data),]

data=na.omit(data)
library(ggsci)

p=visual_age_trend(data,
                   location_select = c("United States of America", "China","India"),
                   measure_select =  c("Deaths","DALYs ","Prevalence","Incidence"),
                   sex_select = c("Both")) 


p+ labs(
  title = "Comparison of Deaths, DALYs, Prevalence, and Incidence for Both(2021)",
  x = "Age Group",
  y = "Rate per 100,000"
) +
  
  scale_x_discrete(labels = function(x) gsub(" years", "", x))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(
          size = 14,             
          hjust = 0.5)        
        )



p1<-visual_age_trend(data,
                 location_select = c("United States of America", "China","India"),
                 measure_select = c("Deaths","DALYs ","Prevalence","Incidence"),
                 sex_select = c("Female", "Male"))
p1+ labs(
  title = "Comparison of Deaths, DALYs, Prevalence, and Incidence by sex(2021)",
  x = "Age Group",
  y = "Rate per 100,000"
) +
  
  scale_x_discrete(labels = function(x) gsub(" years", "", x))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(
          size = 14,           
          hjust = 0.5)        
  )
