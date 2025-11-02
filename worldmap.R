library(ggmap)
library(maps)
library(dplyr)

rm(list = ls())
setwd("E:\\projects\\Rprograms\\Lab\\IBD\\世界地图")
WCBA <- read.csv("IHME-GBD_204.csv",header = T)  ## 读取我们的数据
options(scipen = 200)
unique(WCBA$measure)
###----1990------
### case change
####   case change MAP
case_1990<-subset(WCBA,
                  WCBA$year==1990 &
                    WCBA$age=='All ages' &
                    WCBA$metric=='Number' &
                    WCBA$measure=='Deaths'
)

case_1990$val<-case_1990$val/1000000
###----2021------
case_2021<-subset(WCBA,WCBA$year==2021 &
                    WCBA$age=='All ages' &
                    WCBA$metric=='Number' &
                    WCBA$measure=='Deaths'
                  
)
case_2021$val<-case_2021$val/1000000

case_1990 <- case_1990[,c(2,8)]
case_2021 <- case_2021[,c(2,8)]
names(case_1990) <- c('location','case_1990')
names(case_2021) <- c('location','case_2021')
country_asr <- merge(case_1990, case_2021, by='location')
country_asr$val <- (country_asr$case_2021-country_asr$case_1990)/country_asr$case_1990  ### 获取我们的结果

#########################################################################################
####  map 

worldData <- map_data('world')
worldData <-subset(worldData,region !='Antarctica')

#unique(country_asr$location)
#unique(worldData$region)

country_asr$location <- as.character(country_asr$location) 


###以下代码的目的是让country_asr$location的国家名称与worldData的国家名称一致
### 这样才能让数据映射到地图上
country_asr$location[country_asr$location == 'Bolivarian Republic of Venezuela'] = 'Venezuela'
country_asr$location[country_asr$location == 'Principality of Andorra'] = 'Andorra'
country_asr$location[country_asr$location == 'Republic of the Union of Myanmar'] = 'Myanmar'
country_asr$location[country_asr$location == 'Kingdom of Belgium'] = 'Belgium'
country_asr$location[country_asr$location == 'Russian Federation'] = 'Russia'
country_asr$location[country_asr$location == 'Republic of Paraguay'] = 'Paraguay'
country_asr$location[country_asr$location == 'Islamic Republic of Afghanistan'] = 'Afghanistan'
country_asr$location[country_asr$location == 'Federative Republic of Brazil'] = 'Brazil'
country_asr$location[country_asr$location == 'Republic of Italy'] = 'Italy'
country_asr$location[country_asr$location == 'Plurinational State of Bolivia'] = 'Bolivia'
country_asr$location[country_asr$location == 'Kingdom of Bhutan'] = 'Bhutan'
country_asr$location[country_asr$location == 'Grand Duchy of Luxembourg'] = 'Luxembourg'
country_asr$location[country_asr$location == 'State of Libya'] = 'Libya'
country_asr$location[country_asr$location == 'Republic of the Philippines'] = 'Philippines'
country_asr$location[country_asr$location == 'Republic of Vanuatu'] = 'Vanuatu'
country_asr$location[country_asr$location == 'Togolese Republic'] = 'Togo'
country_asr$location[country_asr$location == 'Eastern Republic of Uruguay'] = 'Uruguay'
country_asr$location[country_asr$location == 'French Republic'] = 'France'
country_asr$location[country_asr$location == "People's Republic of Bangladesh"] = 'Bangladesh'
country_asr$location[country_asr$location == 'Republic of Tajikistan'] = 'Tajikistan'

country_asr$location[country_asr$location == 'Republic of Ecuador'] = 'Ecuador'
country_asr$location[country_asr$location == 'Republic of Korea'] = 'South Korea'
country_asr$location[country_asr$location == 'Commonwealth of Dominica'] = 'Dominica'
country_asr$location[country_asr$location == 'Republic of Poland'] = 'Poland'
country_asr$location[country_asr$location == 'Brunei Darussalam'] = 'Brunei'
country_asr$location[country_asr$location == 'Kingdom of Cambodia'] = 'Cambodia'

country_asr$location[country_asr$location == "Antigua and Barbuda"] = 'Lands Antigua'
a <- country_asr[country_asr$location == "Lands Antigua",]
a$location <- 'Barbuda'
country_asr <- rbind(country_asr,a)

country_asr$location[country_asr$location == 'Republic of Uzbekistan'] = 'Uzbekistan'
country_asr$location[country_asr$location == 'Republic of Chile'] = 'Chile'
country_asr$location[country_asr$location == 'Republic of Austria'] = 'Austria'
country_asr$location[country_asr$location == "Democratic People's Republic of Korea"] = 'North Korea'
country_asr$location[country_asr$location == 'Lebanese Republic'] = 'Lebanon'
country_asr$location[country_asr$location == 'Kingdom of Sweden'] = 'Sweden'

country_asr$location[country_asr$location == "Saint Kitts and Nevis"] = 'Saint Kitts'
a <- country_asr[country_asr$location == "Saint Kitts",]
a$location <- 'Nevis'
country_asr <- rbind(country_asr,a)

country_asr$location[country_asr$location == 'Republic of Colombia'] = 'Colombia'
country_asr$location[country_asr$location == 'Republic of Honduras'] = 'Honduras'
country_asr$location[country_asr$location == "Saint Vincent and the Grenadines"] = 'Saint Vincent'
a <- country_asr[country_asr$location == "Saint Vincent",]
a$location <- 'Grenadines'
country_asr <- rbind(country_asr,a)


country_asr$location[country_asr$location == "People's Republic of China"] = 'China'
country_asr$location[country_asr$location == 'Republic of Armenia'] = 'Armenia'
country_asr$location[country_asr$location == 'Republic of Singapore'] = 'Singapore'
country_asr$location[country_asr$location == 'Republic of Zimbabwe'] = 'Zimbabwe'
country_asr$location[country_asr$location == 'Independent State of Samoa'] = 'Samoa'
country_asr$location[country_asr$location == 'Kingdom of Bahrain'] = 'Bahrain'
country_asr$location[country_asr$location == 'Republic of Albania'] = 'Albania'

country_asr$location[country_asr$location == 'Republic of Belarus'] = 'Belarus'
country_asr$location[country_asr$location == "United Kingdom of Great Britain and Northern Ireland"] = 'UK'
country_asr$location[country_asr$location == 'Republic of Mozambique'] = 'Mozambique'
country_asr$location[country_asr$location == 'Republic of Bulgaria'] = 'Bulgaria'
country_asr$location[country_asr$location == 'Republic of Peru'] = 'Peru'
country_asr$location[country_asr$location == 'Republic of Fiji'] = 'Fiji'
country_asr$location[country_asr$location == 'Republic of Malta'] = 'Malta'
country_asr$location[country_asr$location == 'Gabonese Republic'] = 'Gabon'
country_asr$location[country_asr$location == 'Republic of Guinea'] = 'Guinea'
country_asr$location[country_asr$location == 'Republic of Liberia'] = 'Liberia'

country_asr$location[country_asr$location == 'Republic of Sudan'] = 'Sudan'
country_asr$location[country_asr$location == 'Kingdom of Morocco'] = 'Morocco'
country_asr$location[country_asr$location == 'Federal Republic of Germany'] = 'Germany'
country_asr$location[country_asr$location == 'Republic of Nicaragua'] = 'Nicaragua'


country_asr$location[country_asr$location == 'Trinidad and Tobago'] = 'Trinidad'
a <- country_asr[country_asr$location == "Trinidad",]

a$location <- 'Tobago'
country_asr <- rbind(country_asr,a)


country_asr$location[country_asr$location == 'Kingdom of Denmark'] = 'Denmark'
country_asr$location[country_asr$location == 'Republic of Guyana'] = 'Guyana'
country_asr$location[country_asr$location == 'Kingdom of Eswatini'] = 'Swaziland'
country_asr$location[country_asr$location == 'Republic of Guinea-Bissau'] = 'Guinea-Bissau'
country_asr$location[country_asr$location == 'Republic of Serbia'] = 'Serbia'
country_asr$location[country_asr$location == 'Republic of Iraq'] = 'Iraq'
country_asr$location[country_asr$location == 'Slovak Republic'] = 'Slovak'
country_asr$location[country_asr$location == 'Kingdom of Tonga'] = 'Tonga'
country_asr$location[country_asr$location == 'Republic of Costa Rica'] = 'Costa Rica'

country_asr$location[country_asr$location == 'Republic of Rwanda'] = 'Rwanda'
country_asr$location[country_asr$location == 'Republic of Cyprus'] = 'Cyprus'
country_asr$location[country_asr$location == 'Kingdom of Norway'] = 'Norway'
country_asr$location[country_asr$location == 'Republic of Equatorial Guinea'] = 'Equatorial Guinea'
country_asr$location[country_asr$location == 'Republic of Iceland'] = 'Iceland'
country_asr$location[country_asr$location == 'Sultanate of Oman'] = 'Oman'
country_asr$location[country_asr$location == 'Republic of Guatemala'] = 'Guatemala'
country_asr$location[country_asr$location == 'Kingdom of the Netherlands'] = 'Netherlands'
country_asr$location[country_asr$location == 'Commonwealth of the Bahamas'] = 'Bahamas'
country_asr$location[country_asr$location == 'Republic of the Congo'] = 'Republic of Congo'
country_asr$location[country_asr$location == 'Republic of Maldives'] = 'Maldives'

country_asr$location[country_asr$location == 'Republic of Mauritius'] = 'Mauritius'
country_asr$location[country_asr$location == 'Democratic Republic of Timor-Leste'] = 'Timor-Leste'
country_asr$location[country_asr$location == 'United States of America'] = 'USA'
country_asr$location[country_asr$location == 'Taiwan (Province of China)'] = 'Taiwan'
country_asr$location[country_asr$location == 'Democratic Socialist Republic of Sri Lanka'] = 'Sri Lanka'
country_asr$location[country_asr$location == 'Republic of South Africa'] = 'South Africa'
country_asr$location[country_asr$location == 'Republic of Slovenia'] = 'Slovenia'
country_asr$location[country_asr$location == 'Republic of Malawi'] = 'Malawi'
country_asr$location[country_asr$location == 'Republic of Turkey'] = 'Turkey'
country_asr$location[country_asr$location == 'Republic of Haiti'] = 'Haiti'
country_asr$location[country_asr$location == 'Republic of Croatia'] = 'Croatia'
country_asr$location[country_asr$location == 'Republic of Azerbaijan'] = 'Azerbaijan'
country_asr$location[country_asr$location == 'State of Kuwait'] = 'Kuwait'
country_asr$location[country_asr$location == 'Republic of Yemen'] = 'Yemen'
country_asr$location[country_asr$location == 'Republic of Estonia'] = 'Estonia'
country_asr$location[country_asr$location == 'Kingdom of Thailand'] = 'Thailand'
country_asr$location[country_asr$location == 'United States Virgin Islands'] = 'Virgin Islands'
country_asr$location[country_asr$location == 'Socialist Republic of Viet Nam'] = 'Vietnam'
country_asr$location[country_asr$location == 'Kingdom of Spain'] = 'Spain'
country_asr$location[country_asr$location == 'Republic of Finland'] = 'Finland'
country_asr$location[country_asr$location == 'Portuguese Republic'] = 'Portugal'
country_asr$location[country_asr$location == 'Republic of Madagascar'] = 'Madagascar'
country_asr$location[country_asr$location == 'Republic of India'] = 'India'
country_asr$location[country_asr$location == 'Republic of Seychelles'] = 'Seychelles'
country_asr$location[country_asr$location == 'Republic of Mali'] = 'Mali'
country_asr$location[country_asr$location == 'Republic of Namibia'] = 'Namibia'
country_asr$location[country_asr$location == 'Principality of Monaco'] = 'Monaco'
country_asr$location[country_asr$location == 'Swiss Confederation'] = 'Switzerland'
country_asr$location[country_asr$location == 'Hellenic Republic'] = 'Greece'
country_asr$location[country_asr$location == 'State of Qatar'] = 'Qatar'
country_asr$location[country_asr$location == 'Kingdom of Saudi Arabia'] = 'Saudi Arabia'
country_asr$location[country_asr$location == 'Republic of San Marino'] = 'San Marino'
country_asr$location[country_asr$location == 'Republic of Kiribati'] = 'Kiribati'
country_asr$location[country_asr$location == 'Federal Democratic Republic of Ethiopia'] = 'Ethiopia'
country_asr$location[country_asr$location == 'State of Israel'] = 'Israel'
country_asr$location[country_asr$location == 'Republic of Suriname'] = 'India'
country_asr$location[country_asr$location == 'United Republic of Tanzania'] = 'Tanzania'
country_asr$location[country_asr$location == "Lao People's Democratic Republic"] = 'Laos'
country_asr$location[country_asr$location == 'Republic of Ghana'] = 'Ghana'
country_asr$location[country_asr$location == 'Islamic Republic of Iran'] = 'Iran'
country_asr$location[country_asr$location == 'Republic of Indonesia'] = 'Indonesia'
country_asr$location[country_asr$location == 'United Mexican States'] = 'Mexico'
country_asr$location[country_asr$location == 'Republic of Nauru'] = 'Nauru'
country_asr$location[country_asr$location == 'Republic of Tunisia'] = 'Tunisia'
country_asr$location[country_asr$location == 'Federal Democratic Republic of Nepal'] = 'Nepal'
country_asr$location[country_asr$location == 'Islamic Republic of Pakistan'] = 'Pakistan'
country_asr$location[country_asr$location == 'Republic of Senegal'] = 'Senegal'
country_asr$location[country_asr$location == 'Islamic Republic of Mauritania'] = 'Mauritania'
country_asr$location[country_asr$location == 'Syrian Arab Republic'] = 'Syria'
country_asr$location[country_asr$location == 'Independent State of Papua New Guinea'] = 'Papua New Guinea'
country_asr$location[country_asr$location == 'Republic of the Marshall Islands'] = 'Marshall Islands'
country_asr$location[country_asr$location == 'Union of the Comoros'] = 'Comoros'
country_asr$location[country_asr$location == 'Kyrgyz Republic'] = 'Kyrgyzstan'
country_asr$location[country_asr$location == "People's Democratic Republic of Algeria"] = 'Algeria'
country_asr$location[country_asr$location == 'Federated States of Micronesia'] = 'Micronesia'
country_asr$location[country_asr$location == 'Hashemite Kingdom of Jordan'] = 'Jordan'
country_asr$location[country_asr$location == 'Republic of Lithuania'] = 'Lithuania'
country_asr$location[country_asr$location == 'Republic of Chad'] = 'Chad'
country_asr$location[country_asr$location == 'Republic of Zambia'] = 'Zambia'
country_asr$location[country_asr$location == 'Republic of South Sudan'] = 'South Sudan'
country_asr$location[country_asr$location == 'Republic of Latvia'] = 'Latvia'
country_asr$location[country_asr$location == 'Republic of the Niger'] = 'Niger'
country_asr$location[country_asr$location == 'Argentine Republic'] = 'Argentina'
country_asr$location[country_asr$location == 'Republic of Kazakhstan'] = 'Kazakhstan'
country_asr$location[country_asr$location == 'Republic of Niue'] = 'Niue'
country_asr$location[country_asr$location == 'Republic of El Salvador'] = 'El Salvador'
country_asr$location[country_asr$location == 'Republic of Benin'] = 'Benin'
country_asr$location[country_asr$location == 'Republic of Burundi'] = 'Burundi'
country_asr$location[country_asr$location == 'Federal Republic of Somalia'] = 'Somalia'
country_asr$location[country_asr$location == 'Republic of Cabo Verde'] = 'Cape Verde'
country_asr$location[country_asr$location == 'Republic of Cuba'] = 'Cuba'
country_asr$location[country_asr$location == 'State of Eritrea'] = 'Eritrea'
country_asr$location[country_asr$location == 'Republic of Palau'] = 'Palau'
country_asr$location[country_asr$location == 'Arab Republic of Egypt'] = 'Egypt'
country_asr$location[country_asr$location == 'Republic of Djibouti'] = 'Djibouti'
country_asr$location[country_asr$location == 'Republic of Cameroon'] = 'Cameroon'
country_asr$location[country_asr$location == 'Republic of Uganda'] = 'Uganda'
country_asr$location[country_asr$location == 'Republic of Moldova'] = 'Moldova'
country_asr$location[country_asr$location == 'Democratic Republic of Sao Tome and Principe'] = 'Sao Tome and Principe'
country_asr$location[country_asr$location == 'Republic of Angola'] = 'Angola'
country_asr$location[country_asr$location == 'Republic of Botswana'] = 'Botswana'
country_asr$location[country_asr$location == 'Republic of Panama'] = 'Panama'
country_asr$location[country_asr$location == 'Federal Republic of Nigeria'] = 'Nigeria'
country_asr$location[country_asr$location == 'Republic of Kenya'] = 'Kenya'
country_asr$location[country_asr$location == "Republic of Côte d'Ivoire"] = 'Ivory Coast'
country_asr$location[country_asr$location == 'Kingdom of Lesotho'] = 'Lesotho'
country_asr$location[country_asr$location == 'Republic of the Gambia'] = 'Gambia'
country_asr$location[country_asr$location == 'Republic of Sierra Leone'] = 'Sierra Leone'



total <- full_join(worldData,country_asr,by = c('region'='location')) %>%
          filter(val !='NA')
#View(total)
range(total$val)#-0.6657182 35.2025468

total <- total %>%
  mutate(val2 = cut(total$val, 
                    breaks = c(-1,1.5,2.5, 5, 20, 40,80,120,200,280),  # 区间分割点
                    labels = c("-1~1.5", "1.5~2.5", "2.5~5", "5~20", "20~40", 
                               "40~80", "80~120", "120~200","200+"),  # 匹配的标签，修正了遗漏和错误的区间
                    include.lowest = TRUE, right = TRUE))



p1<- total %>% 
  ggplot()+
  geom_polygon(aes(x = long, y = lat,group = group,fill=val2),
               colour="black",linewidth=0.5) +
  theme_bw()+
 # scale_fill_brewer(palette = 'Spectral') + #RColorBrewer 包含多种预设的颜色序列
  guides(fill = guide_legend(title='Change in Deaths cases', ncol = 2))+
  scale_fill_manual(values = c('#08519C', '#3182BD', '#6BAED6', '#9ECAE1', '#EFF3FF', 
                               '#FEE0D2', '#FCBBA1', '#FB6A4A', '#CB181D'))+
  theme(legend.position = c(0.15,0.25),
        legend.title = element_text(color="black", 
                                  size =8, 
                                  #face = "bold"
        ),
        legend.text = element_text(color="black", 
                                   size = 6
                                   #face = "bold",
        ),
        panel.grid=element_blank(),#移除背景网格线
        panel.border = element_rect(color='black',
                                    fill=NA,
                                    size = 0.5),
        #legend.position = 'none',
        axis.title.x = element_blank(),#移除了x轴的标题、刻度标签和刻度线，使得x轴不可见。
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
  )

p2 <- p1+ labs(x=" ",y="",title="America")+
  coord_cartesian(xlim = c(-125, -66),ylim = c(24, 50))+theme(legend.position = 'none',plot.title = element_text(size = 10, face = "bold"))

p3 <- p1+ labs(x=" ",y="",title="China")+
  coord_cartesian(xlim = c(73, 135),ylim = c(18, 54))+theme(legend.position = 'none',plot.title = element_text(size = 10, face = "bold"))

p4 <- p1+ labs(x=" ",y="",title="India")+
  coord_cartesian(xlim =  c(68, 97),ylim = c( 8, 37))+theme(legend.position = 'none',plot.title = element_text(size = 10, face = "bold"))


library(patchwork)
p1 + (p2+p3+p4+plot_layout(ncol = 3,widths=c(1,1,1))) +
  plot_layout(nrow=2,heights = c(0.8,0.3))





