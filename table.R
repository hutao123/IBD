library_required_packages()
#授权码：751d56ca050978cd6e0edd4a068c190bb04a98250db767b4852136be0a2c890e
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
  input_path = "IHME-GBD_2021_DATA-6d34f396-1.csv",      # 输入文件的路径
  measure = "Incidence",                # 测量指标（Incidence、Mortality 等）
  locations = location,                     # 地点列表（默认使用特定的一组地点）
  year1 = 1990,                         # 第一个年份（默认1990年）
  year2 = 2021,                         # 第二个年份（默认2019年）
  regions_order = order,                 # 地区顺序（默认按照特定顺序排列）
  output_path = 'Incidence.docx'                    # 输出表格保存路径（默认不保存）
)

t_Prevalence=GBD_table(
  input_path = "Prevalence.csv",      # 输入文件的路径
  measure = "Prevalence",                # 测量指标（Incidence、Mortality 等）
  locations = location,                     # 地点列表（默认使用特定的一组地点）
  year1 = 1990,                         # 第一个年份（默认1990年）
  year2 = 2021,                         # 第二个年份（默认2019年）
  regions_order = order,                 # 地区顺序（默认按照特定顺序排列）
  output_path = 'Prevalence.docx'                    # 输出表格保存路径（默认不保存）
)


t_Deaths=GBD_table(
  input_path = "IHME-GBD_2021_DATA-6d34f396-1.csv",      # 输入文件的路径
  measure = "Deaths",                # 测量指标（Incidence、Mortality 等）
  locations = location,                     # 地点列表（默认使用特定的一组地点）
  year1 = 1990,                         # 第一个年份（默认1990年）
  year2 = 2021,                         # 第二个年份（默认2019年）
  regions_order = order,                 # 地区顺序（默认按照特定顺序排列）
  output_path = 'Deaths.docx'                    # 输出表格保存路径（默认不保存）
)

t_DALYs=GBD_table(
  input_path = "IHME-GBD_2021_DATA-6d34f396-1.csv",      # 输入文件的路径
  measure = "DALYs",                # 测量指标（Incidence、Mortality 等）
  locations = location,                     # 地点列表（默认使用特定的一组地点）
  year1 = 1990,                         # 第一个年份（默认1990年）
  year2 = 2021,                         # 第二个年份（默认2019年）
  regions_order = order,                 # 地区顺序（默认按照特定顺序排列）
  output_path = 'DALYs.docx'                    # 输出表格保存路径（默认不保存）
)
