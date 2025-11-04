library(tidyverse)
library(magrittr)
library(data.table)
library(gt)
rm(list = ls())
setwd('E:/projects/Rprograms/Lab/IBD/三线表')
data=read.csv('IHME-GBD_2021_DATA-6d34f396-1.csv')
data=gbd_filter(data,measure == "Prevalence",age %in% c('Age-standardized',
                                                        'All ages'))

write.csv(data,'Prevalence.csv')


location=unique(data$location)
order=c("Global","United States of America","China",
        "Republic of India")
      

t_Incidence=GBD_table(
  input_path = "IHME-GBD_2021_DATA-6d34f396-1.csv",    
  measure = "Incidence",              
  locations = location,                 
  year1 = 1990,                       
  year2 = 2021,                       
  regions_order = order,                
  output_path = 'Incidence.docx'               
)

t_Prevalence=GBD_table(
  input_path = "Prevalence.csv",     
  measure = "Prevalence",             
  locations = location,                 
  year1 = 1990,                
  year2 = 2021,                   
  regions_order = order,          
  output_path = 'Prevalence.docx'                 
)


t_Deaths=GBD_table(
  input_path = "IHME-GBD_2021_DATA-6d34f396-1.csv",    
  measure = "Deaths",        
  locations = location,                   
  year1 = 1990,                       
  year2 = 2021,                        
  regions_order = order,               
  output_path = 'Deaths.docx'                 
)

t_DALYs=GBD_table(
  input_path = "IHME-GBD_2021_DATA-6d34f396-1.csv",    
  measure = "DALYs",               
  locations = location,                    
  year1 = 1990,                        
  year2 = 2021,                       
  regions_order = order,              
  output_path = 'DALYs.docx'                  
)
