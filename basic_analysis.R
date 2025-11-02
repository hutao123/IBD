library(dplyr)
library(ggplot2)

rm(list = ls())
setwd('E:/projects/Rprograms/Lab/IBD')


fig1b<-readxl::read_excel("./data/fig1条形图.xlsx")
View(fig1b)

# 定义颜色
colors <- c("#1F77B4","#FF7F0E", "#2CA02C","#7F7F7F") 

fig1b$group <- factor(fig1b$group, levels = unique(fig1b$group))

##EAPC of the rate
ggplot(fig1b, aes(x = group, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge", width = 1) +

  scale_fill_manual(values = colors) + 
  scale_y_continuous (limits = c (-4, 4), breaks = seq (-4,4,1)) + 
  scale_x_discrete(limits = rev(levels(fig1b$group)))+
       x = "", y = "EAPC of ASR(1990-2021)",
       fill = "")+theme(
     panel.background = element_blank(),
     strip.background = element_blank(),
     plot.title = element_text(hjust = 0.5),
     panel.border = element_rect(fill = NA, color = "black")
   )



##----------两年对比柱状图----------------

fig2b<-read.csv("./data/ibd/Colon_SDI.csv")
df<-subset(fig2b,
           fig2b$age=='All ages' &
           fig2b$sex=='Both'&
           fig2b$metric=='Number' &
           fig2b$measure=='Incidence'&
           fig2b$year %in% c(1990,2023))
df <- df[,c(2,7,8)]
unique(df$location)



df$location<- factor(df$location, levels = c("High-middle SDI", "Middle SDI", "Low-middle SDI", "Low SDI" ))
df$year <- factor(df$year,levels = c(1990,2023))
# 定义颜色
colors <- c("#1F77B4","#FF7F0E") 

my_plot <- ggplot(df, aes(x = location, y = val, fill = year)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  coord_flip()+# 这一行代码翻转了坐标轴
  scale_fill_manual(values = colors) + # 自定义填充颜色
  scale_y_continuous (labels = function(x) format(x, scientific = TRUE)) + # 设置横坐标刻度节点
  scale_x_discrete(limits = rev(levels(df$location)))+#反转排序
  labs(title = " ",
       x = "", y = "Incidence cases",
       fill = "")+theme(
         panel.background = element_blank(),
         panel.border = element_blank(), # 移除绘图区边框
         axis.line.x = element_line(color = "black"), # 单独保留X轴底部线
         axis.line.y = element_line(color = "black"), # 单独保留Y轴左侧线
         # 刻度线设置
         axis.ticks.x = element_line(color = "black", size = 0.5),
         axis.ticks.y = element_line(color = "black", size = 0.5),
         # 坐标轴标签旋转
         axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size=14),
         # 其他样式保持
         axis.text.y = element_text(size=14),
          panel.grid = element_blank(), # 移除网格线
         plot.title = element_text(hjust = 0.5),
         legend.position = "top" # 根据需求隐藏图例
       )


ggsave(
  filename = "两年对比柱状图_Incidence.svg",  # 文件名
  plot = my_plot,            # 图形对象
  device = "svg",            # 指定SVG格式
  width = 8,                 # 宽度(英寸)
  height = 6,                # 高度(英寸)
  dpi = 300                  # 分辨率（不影响矢量图，但影响嵌入的栅格元素）
)

##Percentage change
fig1b_pc<-readxl::read_excel("fig1条形图.xlsx",sheet='条形图2')
View(fig1b_pc)

# 定义颜色
colors <- c("#0066FF","#FF8800", "#AAAAAA") # 为三个变量定义颜色

fig1b_pc$group <- factor(fig1b_pc$group, levels = unique(fig1b_pc$group))

##EAPC of the rate
ggplot(fig1b_pc, aes(x = group, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge", width = 1) +
  coord_flip() + # 这一行代码翻转了坐标轴
  scale_fill_manual(values = colors) + # 自定义填充颜色
  scale_y_continuous (limits = c (-2, 10), breaks = seq (-2,10,2)) + # 设置横坐标刻度节点
  scale_x_discrete(limits = rev(levels(fig1b$group)))+#反转排序
  labs(title = "Migraine burden in WCBA in global and 5 territories",
       x = "", y = "EAPC of the rate",
       fill = "")+theme(
         panel.background = element_blank(),
         strip.background = element_blank(),
         # 添加黑色边框
         panel.border = element_rect(fill = NA, color = "black")
       )



##----------地区对比折线图----------------

df<-subset(fig2b,
           fig2b$age=='Age-standardized' &
           fig2b$metric=='Rate' &
           fig2b$sex=='Both'&
           fig2b$measure=='Deaths'
)

df <- df[,c(2,7,8)]
write.csv(df,"折线图—Incidence(Rate).csv")
range(df$val)# 6.151281 45.939874
          
my_plot1 <- ggplot(data = df,aes(x=year,y=val,group = location,color=location))+
  geom_point()+
  geom_line()+
  xlab("Year")+#横坐标名称
  ylab("ASMR")+#纵坐标名称
  theme(panel.background = element_blank(),
    strip.background = element_blank(),
    panel.border = element_rect(fill = NA, color = "black"))+
  theme(
    panel.background = element_blank(),
    strip.background = element_blank(),
    # 关键修改：移除外框线
    panel.border = element_blank(),
    axis.text.x = element_text(size=14),
    # 其他样式保持
    axis.text.y = element_text(size=14),
    # 显式保留坐标轴线和刻度线
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    axis.ticks = element_line(color = "black")
  ) +
  scale_x_continuous(limits = c(1990,2023),breaks = seq(1990,2023,10))+#更改横坐标刻度值
  scale_y_continuous(limits = c(5,13),breaks = seq(5,13,2))


ggsave(
  filename = "地区对比折线图_Deaths.svg",  # 文件名
  plot = my_plot1,            # 图形对象
  device = "svg",            # 指定SVG格式
  width = 8,                 # 宽度(英寸)
  height = 6,                # 高度(英寸)
  dpi = 300                  # 分辨率（不影响矢量图，但影响嵌入的栅格元素）
)



