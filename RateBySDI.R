
library(tidyverse)
library(ggh4x)
library(ggsci)
library(magrittr)
library(grid)

rm(list = ls())
setwd('E:\\projects\\Rprograms\\Lab\\IBD\\可视化')
data=read.csv('../data/ibd/IHME-GBD_2021_DATA-16854fe6-1.csv')
unique(data$location)

data$location <- factor(data$location,
                        levels = c("Global", "America","China","India"))
unique(data$measure)

data$measure <- factor(data$measure,
                       levels = c("Prevalence","Incidence","Deaths","DALYs (Disability-Adjusted Life Years)"))
#data$location <- str_replace(data$location,"DALYs ","DALYs (Disability-Adjusted Life Years)" )
#420*880
visual_SDI_GBDrate(
  data,
  selected_locations = data$location,
  selected_measures = data$measure
  )+  theme(
    # 调整X轴文本角度和对齐，并设置字体大小
    axis.text.x = element_text(size = 10),
    # 调整Y轴主刻度文本大小
    axis.text.y = element_text(size = 10),
    # 调整图例文本大小
    legend.text = element_text(size = 12)
  )


##----------Fig1 折线图----------------
unique(data$measure)
WCBA <- data
df<-subset(WCBA,
           WCBA$age=='Age-standardized' &
             WCBA$sex=='Female' &
             WCBA$metric=='Rate' &
             WCBA$measure=='Deaths'&
             WCBA$location %in% c("Global", "America","China","India"))


df <- df[,c(2,7,8)]
range(df$val)#31.01337 741.58966

df$location <- factor(df$location,
                        levels = c("Global", "America","China","India"))


#有图例 上方
tiff(file = "d_both.tiff", width =4, height =4, units = "in", res = 300,compression = "lzw")

ggplot(data = df, aes(x = year, y = val, group = location, color = location)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  labs(x = "", y = "DALYs rate per 100,000") +
  scale_x_continuous(
    limits = c(1990, 2021),
    breaks = seq(1990, 2020, 10),
    expand = c(0.02, 0)
  ) +
  scale_y_continuous(
    limits = c(0, 1.3),
    breaks = seq(0, 1.3, 5),
    expand = c(0, 0)
  ) +
  scale_color_brewer(
    palette = "Set1",
    name = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.border = element_rect(fill = NA, color = "black", linewidth = 0.6),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.2),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.4),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "top",
    legend.direction = "horizontal",
    legend.box.spacing = unit(0, "pt"),
    legend.margin = margin(t = 0, b = 10),
    legend.spacing.x = unit(4, "pt"),
    plot.margin = margin(10, 10, 10, 10)
  ) +
  guides(color = guide_legend(
    override.aes = list(size = 3),
    label.position = "right",
    keywidth = unit(12, "pt")
  ))

dev.off()

