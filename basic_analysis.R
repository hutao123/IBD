library(dplyr)
library(ggplot2)

rm(list = ls())
setwd('E:/projects/Rprograms/Lab/IBD')


fig1b<-readxl::read_excel("./data/fig1条形图.xlsx")
View(fig1b)


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
  coord_flip()+
  scale_fill_manual(values = colors) +
  scale_y_continuous (labels = function(x) format(x, scientific = TRUE)) +
  scale_x_discrete(limits = rev(levels(df$location)))+
  labs(title = " ",
       x = "", y = "Incidence cases",
       fill = "")+theme(
         panel.background = element_blank(),
         panel.border = element_blank(), 
         axis.line.x = element_line(color = "black"), 
         axis.line.y = element_line(color = "black"),    
         axis.ticks.x = element_line(color = "black", size = 0.5),
         axis.ticks.y = element_line(color = "black", size = 0.5),
         axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size=14),
         axis.text.y = element_text(size=14),
          panel.grid = element_blank(), 
         plot.title = element_text(hjust = 0.5),
         legend.position = "top"
       )


ggsave(
  filename = "两年对比柱状图_Incidence.svg",  
  plot = my_plot,           
  device = "svg",            
  width = 8,                
  height = 6,               
  dpi = 300                 
)

##Percentage change
fig1b_pc<-readxl::read_excel("fig1条形图.xlsx",sheet='条形图2')
View(fig1b_pc)

# 定义颜色
colors <- c("#0066FF","#FF8800", "#AAAAAA") 
fig1b_pc$group <- factor(fig1b_pc$group, levels = unique(fig1b_pc$group))

##EAPC of the rate
ggplot(fig1b_pc, aes(x = group, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge", width = 1) +
  coord_flip() + 
  scale_fill_manual(values = colors) + 
  scale_y_continuous (limits = c (-2, 10), breaks = seq (-2,10,2)) + 
  scale_x_discrete(limits = rev(levels(fig1b$group)))+
  labs(title = "Migraine burden in WCBA in global and 5 territories",
       x = "", y = "EAPC of the rate",
       fill = "")+theme(
         panel.background = element_blank(),
         strip.background = element_blank(),
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
  xlab("Year")
  ylab("ASMR")
  theme(panel.background = element_blank(),
    strip.background = element_blank(),
    panel.border = element_rect(fill = NA, color = "black"))+
  theme(
    panel.background = element_blank(),
    strip.background = element_blank(),
    panel.border = element_blank(),
    axis.text.x = element_text(size=14),
    axis.text.y = element_text(size=14),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    axis.ticks = element_line(color = "black")
  ) +
  scale_x_continuous(limits = c(1990,2023),breaks = seq(1990,2023,10))+
  scale_y_continuous(limits = c(5,13),breaks = seq(5,13,2))


ggsave(
  filename = "地区对比折线图_Deaths.svg",  
  plot = my_plot1,          
  device = "svg",            
  width = 8,                
  height = 6,                
  dpi = 300             
)



