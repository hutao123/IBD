library_required_packages()
rm(list = ls())
setwd('E:\\projects\\Rprograms\\Lab\\IBD')

data=read.csv('data/ibd/IHME-GBD_2021_DATA-16854fe6-1.csv')
unique(data$measure)


population=merge_csv_files_vroom_progress('data/ibd/GBD_population')
population=gbd_rename_column(population)
View(population)
#class(population$location)
class(population$location)

loc_name <- c("Global","America", "China","India")

a=decoposition_analysis_locations(data,pop = population,sex_choice = 'Both',
                                locations =loc_name,measure_sel="DALYs (Disability-Adjusted Life Years)")

write.csv(a$decomposition_data,"DALYs.csv")
plot<-a$plot
plot + labs(title = "Decomposition Analysis for All")
#######

##
res=decoposition_analysis_sex(data=data,pop=population,locations = 'India',
                              measure_sel="DALYs (Disability-Adjusted Life Years)")
res$plot + labs(title = "Decomposition Analysis for India") + theme(plot.title = element_text(size = 10)) 
