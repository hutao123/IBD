library_required_packages()
library(ggplot2)
library(reshape2)
library(dplyr)
#install.packages('readxl')

rm(list = ls())
setwd('E:\\projects\\Rprograms\\Lab\\IBD\\可视化')
IBD_china <- read.csv('../data/ibd/IHME-GBD_2021_DATA-16854fe6-1.csv')
options(scipen = 200)
#View(IBD_china)
IBD_china$val <- round(IBD_china$val, 2)

IBD_china$age <- str_replace(IBD_china$age," years","")
IBDS <- IBD_china
unique(IBDS$location)
age_group <- c("<5","5-9","10-14","15-19","20-24", "25-29", "30-34", "35-39", "40-44", "45-49",
               "50-54", "55-59", "60-64", "65-69", "70-74", "75-79",
               "80-84", "85-89", "90-94","95+","All ages","Age-standardized")
IBDS$age <- factor(IBDS$age , levels = age_group, ordered = TRUE)
data_in<- gbd_filter(IBDS, year %in% c(1990,2021),
                        metric %in% c('Rate','Number'),
                        location=='China',
                        measure=='Prevalence',
                        sex=="Both",
                        !age %in% c("All ages","Age-standardized"))# 读取21个地区的数据


#tiff(file = "D_China.tiff", width =6, height =4, units = "px", res = 300,compression = "lzw")
#860*600 字号14
visual_dual_axis_GBD(data_in,
                     colors = c('#7E57C2','#FFC107'),
                     x_lab = "Year",
                     y_lab_primary = "Prevalence Number",
                     y_lab_secondary = "Crude Prevalence rate per 100,000",
                     legend_title_number = "Number",
                     legend_title_rate = "Rate",
                     primary_limits = NULL,
                     secondary_limits = NULL
                   )+  theme(
                     # 调整X轴文本角度和对齐，并设置字体大小
                     axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 14),
                     # 调整Y轴主刻度文本大小
                     axis.text.y = element_text(size = 14),
                     # 调整Y轴次刻度文本大小（右侧）
                     axis.text.y.right = element_text(size = 12),
                     # 调整X轴标题大小
                     axis.title.x = element_text(size = 14),
                     # 调整左侧Y轴标题大小
                     axis.title.y.left = element_text(size = 14),
                     # 调整右侧Y轴标题大小
                     axis.title.y.right = element_text(size = 14),
                     # 调整图例标题大小
                     legend.title = element_text(size = 12),
                     # 调整图例文本大小
                     legend.text = element_text(size = 12)
                   )
#dev.off()




