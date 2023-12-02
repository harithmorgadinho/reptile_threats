#threatened_animals_vs_threat
library(sf)
sf::sf_use_s2(FALSE)

shp1 = st_read('reptiles_13oct.shp')
colnames(shp1)

library(rnaturalearth)
library(rnaturalearthdata)
worldMap <- rnaturalearth::ne_countries(scale = 10, type = "countries", returnclass = 'sf')
worldMap_behrmann = st_transform(worldMap,  
                                 crs = '+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0')
r = raster(st_zm(worldMap_behrmann), resolution = 50000 ,
           crs = '+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0')



load('/Users/harith/Dropbox/Copenhagen_postdoc/sf_reptiles_summarized.Rdata')
part1 = read.csv('/Users/harith/Dropbox/Copenhagen_postdoc/redlist_species_data_1e6a78a5-c09e-430f-906e-a710a61fb4b0/threats.csv',stringsAsFactors = F)
part2 = read.csv('/Users/harith/Dropbox/Copenhagen_postdoc/redlist_species_data_0e1e13b5-f355-4302-9cf4-822d33b08258/threats.csv',stringsAsFactors = F)

all_threats = rbind(part1,part2)
sf_merged = merge(sf_reptiles,all_threats[,3:5], by.x = 'binomial',by.y = 'scientificName')

length(unique(sf_merged$binomial))

sf_reptiles_threatened = sf_reptiles[sf_reptiles$binomial %in% all_threats$scientificName,]

sf_reptiles_threatened$binomial = as.factor(sf_reptiles_threatened$binomial)
library(raster)
library(fasterize)

raster_threat = fasterize(sf = st_collection_extract(sf_reptiles_threatened, "POLYGON"),raster = r,
                          fun = 'count',
                          field = 'binomial')

sf_reptiles$binomial = as.factor(sf_reptiles$binomial)
raster_richness = fasterize(sf = st_collection_extract(sf_reptiles, "POLYGON"),raster = r,
                            fun = 'count',
                            field = 'binomial')
raster_richness[raster_richness<10]= NA
plot(raster_threat)

library(stringr)

x = sf_reptiles$binomial[1]
first_name = function(x){
  return(str_split(x,pattern = ' ')[[1]][1])
}

names = unlist(lapply(sf_reptiles$binomial,FUN = first_name))

sf_reptiles$sp_name = unlist(lapply(sf_reptiles$binomial,FUN = first_name))

all_threats$sp_name = unlist(lapply(all_threats$scientificName,FUN = first_name))

colnames(all_threats)
sf_reptiles_threatened2 = sf_reptiles[sf_reptiles$sp_name %in% all_threats$sp_name,]

sf_reptiles_threatened2$binomial = as.factor(sf_reptiles_threatened2$binomial)
raster_threat_genus = fasterize(sf = st_collection_extract(sf_reptiles_threatened2, "POLYGON"),raster = r,
                                fun = 'count',
                                field = 'binomial')

plot(raster_threat_genus)


future_threat = raster('/Users/harith/Dropbox/Copenhagen_postdoc/Data_Archive_future_threats/CDTR/CumDevRank.tif')

newproj <- "+proj=cea +lat_ts=30 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"
future_threat2 <- projectRaster(future_threat, crs=newproj)

future_threat3 = crop(future_threat2,raster_threat_genus)
future_threat4<- resample(future_threat3, raster_threat_genus)
plot(future_threat4)
plot(raster_threat_genus)

raster_richness[is.na(raster_richness)]= 0
shp3 = st_as_sf(rasterToPolygons(raster_richness))

raster_threat[is.na(raster_threat)]= 0
shp_temp = st_as_sf(rasterToPolygons(raster_threat))

shp3$threat_sp = shp_temp$layer

raster_threat_genus[is.na(raster_threat_genus)]= 0
shp_temp = st_as_sf(rasterToPolygons(raster_threat_genus))

shp3$threat_gn = shp_temp$layer

future_threat4[is.na(future_threat4)]= 0
shp_temp = st_as_sf(rasterToPolygons(future_threat4))

shp3$cum_future_threat = shp_temp$CumDevRank

#shp3[shp3$threat_sp==0,]$threat_sp = NA



head(shp3)
library(ggplot2)


shp3$probability_threatened = shp3$threat_gn/shp3$layer



ggplot(data = shp3,aes(x = probability_threatened,y = threat_sp))+geom_point()+
  #stat_summary(fun.data= mean_cl_normal) + 
  geom_smooth(method='lm')

st_geometry(shp3) = NULL

shp3_v2 = shp3[!is.nan(shp3$probability_threatened) & !is.infinite(shp3$probability_threatened),]

shp3_v2 = shp3_v2[shp3_v2$layer>10,]

test =lm(data = shp3_v2,threat_sp~probability_threatened)
summary(test)

ggplot(data = shp3_v2,aes(x = probability_threatened,y = threat_sp))+geom_point()+
  #stat_summary(fun.data= mean_cl_normal) + 
  geom_smooth(method='lm')



### all threats
x = sf_reptiles
get_threatened_genus = function(x,threat = 'hunting'){
  
  hunting = c("5.1.1","5.1.2","5.1.3","5.1.4")
  agriculture = c("2.1.1","2.1.2","2.1.3","2.1.4","2.2.1","2.2.2","2.2.3","2.3.1",
                  "2.3.2","2.3.4","2.4.1","2.4.2","2.4.3")
  logging = c("5.3.1", "5.3.2","5.3.3", "5.3.4", "5.3.5")
  pollution = c("9.1.1" ,"9.1.2", "9.1.3" ,"9.2.1" ,"9.2.2", "9.2.3", "9.3.1", "9.3.2", "9.3.3", "9.3.4",
                "9.4","9.5.1", "9.5.2","9.5.3","9.5.4" ,"9.6.1","9.6.2","9.6.3","9.6.4")
  climate_change = c("11.1", "11.2" ,"11.3" ,"11.4", "11.5")
  invasive = c('8.1.1','8.1.2')
  
  codes_list=list(hunting,agriculture,logging,pollution,climate_change,invasive)
  names(codes_list) = c('hunting','agriculture','logging','climate_change','pollution','invasive')
  
  sf_reptiles_threatened = all_threats[all_threats$code %in% codes_list[[threat]],]$scientificName
  x1 = x[x$binomial %in% sf_reptiles_threatened,]
  
  x1$binomial = as.factor(x1$binomial)
  
  raster_threat = fasterize(sf = st_collection_extract(x1, "POLYGON"),raster = r,
                            fun = 'count',
                            field = 'binomial')
  
  x$binomial = as.factor(x$binomial)
  raster_richness = fasterize(sf = st_collection_extract(x, "POLYGON"),raster = r,
                              fun = 'count',
                              field = 'binomial')
  
  
  first_name = function(x){
    return(str_split(x,pattern = ' ')[[1]][1])
  }
  
  names = unlist(lapply(sf_reptiles$binomial,FUN = first_name))
  
  sf_reptiles$sp_name = unlist(lapply(sf_reptiles$binomial,FUN = first_name))
  
  all_threats$sp_name = unlist(lapply(all_threats$scientificName,FUN = first_name))
  
  colnames(all_threats)
  
  threatened_gn = all_threats[all_threats$code  %in% codes_list[[threat]],]$sp_name
  
  
  sf_reptiles_threatened2 = sf_reptiles[sf_reptiles$sp_name %in% threatened_gn,]
  
  sf_reptiles_threatened2$binomial = as.factor(sf_reptiles_threatened2$binomial)
  raster_threat_genus = fasterize(sf = st_collection_extract(sf_reptiles_threatened2, "POLYGON"),raster = r,
                                  fun = 'count',
                                  field = 'binomial')
  
  
  
  raster_richness[is.na(raster_richness)]= 0
  shp3 = st_as_sf(rasterToPolygons(raster_richness))
  
  raster_threat[is.na(raster_threat)]= 0
  shp_temp = st_as_sf(rasterToPolygons(raster_threat))
  
  shp3$threat_sp = shp_temp$layer
  
  raster_threat_genus[is.na(raster_threat_genus)]= 0
  shp_temp = st_as_sf(rasterToPolygons(raster_threat_genus))
  
  shp3$threat_gn = shp_temp$layer
  
  shp3$probability_threatened = shp3$threat_gn/shp3$layer
  return(shp3)
}

list_threats=c('hunting','agriculture','logging','pollution','invasive','climate_change')


df_numbers_threats = data.frame()
for (i in seq_along(list_threats)){
  threat = list_threats[i]
  
print(threat)

hunting = c("5.1.1","5.1.2","5.1.3","5.1.4")
agriculture = c("2.1.1","2.1.2","2.1.3","2.1.4","2.2.1","2.2.2","2.2.3","2.3.1",
                "2.3.2","2.3.4","2.4.1","2.4.2","2.4.3")
logging = c("5.3.1", "5.3.2","5.3.3", "5.3.4", "5.3.5")
pollution = c("9.1.1" ,"9.1.2", "9.1.3" ,"9.2.1" ,"9.2.2", "9.2.3", "9.3.1", "9.3.2", "9.3.3", "9.3.4",
              "9.4","9.5.1", "9.5.2","9.5.3","9.5.4" ,"9.6.1","9.6.2","9.6.3","9.6.4")
climate_change = c("11.1", "11.2" ,"11.3" ,"11.4", "11.5")
invasive = c('8.1.1','8.1.2')

codes_list=list(hunting,agriculture,logging,pollution,climate_change,invasive)
names(codes_list) = c('hunting','agriculture','logging','climate_change','pollution','invasive')

sf_reptiles_threatened = all_threats[all_threats$code %in% codes_list[[threat]],]$scientificName

first_name = function(x){
  return(str_split(x,pattern = ' ')[[1]][1])
}

names = unlist(lapply(sf_reptiles$binomial,FUN = first_name))

sf_reptiles$sp_name = unlist(lapply(sf_reptiles$binomial,FUN = first_name))

all_threats$sp_name = unlist(lapply(all_threats$scientificName,FUN = first_name))

colnames(all_threats)

threatened_gn = all_threats[all_threats$code  %in% codes_list[[threat]],]$sp_name


sf_reptiles_threatened2 = sf_reptiles[sf_reptiles$sp_name %in% threatened_gn,]


df_temp =cbind.data.frame(threat,species = length(sf_reptiles_threatened), inferred = length(sf_reptiles_threatened2$binomial))

df_numbers_threats = rbind(df_numbers_threats,df_temp)

}

write.xlsx(df_numbers_threats,'df_numbers_threats.xls')



hunting_shp = get_threatened_genus(x,threat = 'hunting')

agriculture_shp = get_threatened_genus(x,threat = 'agriculture')

pollution_shp = get_threatened_genus(x,threat = 'pollution')

climate_change_shp = get_threatened_genus(x,threat = 'climate_change')

invasive_shp = get_threatened_genus(x,threat = 'invasive')

logging_shp = get_threatened_genus(x,threat = 'logging')

library(RColorBrewer)
library(scales)

color.schemes<-list(
  "Logging" = brewer.pal(8,"Greens"),
  "Agriculture" = rev(brewer.pal(11,"BrBG")[1:5]),
  "Hunting" = brewer.pal(8,"Reds"),
  "Pollution" = brewer.pal(8,"Blues"),
  "Invasives" = brewer.pal(8,"Purples"),
  "Climate change" = brewer.pal(8,"Oranges")
)

shp3 = hunting_shp
shp3$threat_sp_prob = shp3$threat_sp/shp3$layer
#shp3_v2 = shp3[!is.nan(shp3$probability_threatened) & !is.infinite(shp3$probability_threatened),]
shp3_v2 = shp3[shp3$layer>10,]



size_axis = 8
size_axis_text = 7

p1 = ggplot(data = shp3_v2,aes(x = threat_gn,y = threat_sp_prob))+geom_point(col = color.schemes$Hunting[5],pch = 21,alpha = 0.25)+
  #stat_summary(fun.data= mean_cl_normal) + 
  geom_smooth(method='lm',col = 'black')+
  theme_minimal()+
  theme(axis.title.x = element_text(hjust = 0.9),
        axis.title.y = element_text(hjust = 0.9),
        axis.title = element_text(size = size_axis),
        axis.text = element_text(size = size_axis_text),
        plot.tag = element_text(face='bold'))+
  ylab('Probability of threat (Hunting)')+
  xlab('Species potentially affected by threat')+
  labs(tag = 'a.')

m1 = lm(data = shp3_v2,threat_sp_prob~threat_gn,family=gaussian)
summary(m1)
m1 = summary(m1)
m1 = as.data.frame(m1$coefficients)

shp3 = agriculture_shp
shp3$threat_sp_prob = shp3$threat_sp/shp3$layer
#shp3_v2 = shp3[!is.nan(shp3$probability_threatened) & !is.infinite(shp3$probability_threatened),]
shp3_v2 = shp3[shp3$layer>10,]

m2 = lm(data = shp3_v2,threat_sp_prob~threat_gn,family=gaussian)
summary(m2)
m2 = summary(m2)
m2 = as.data.frame(m2$coefficients)


p2 = ggplot(data = shp3_v2,aes(x = threat_gn,y = threat_sp_prob))+geom_point(col = color.schemes$Agriculture[3],pch = 21,alpha = 0.25)+
  #stat_summary(fun.data= mean_cl_normal) + 
  geom_smooth(method='lm',col = 'black')+
  theme_minimal()+
  theme(axis.title.x = element_text(hjust = 0.9),
        axis.title.y = element_text(hjust = 0.9),
        axis.title = element_text(size = size_axis),
        axis.text = element_text(size = size_axis_text),
        plot.tag = element_text(face='bold'))+
  ylab('Probability of threat (Agriculture)')+
  xlab('Species potentially affected by threat')+
  labs(tag = 'b.')



shp3 = logging_shp
shp3$threat_sp_prob = shp3$threat_sp/shp3$layer
#shp3_v2 = shp3[!is.nan(shp3$probability_threatened) & !is.infinite(shp3$probability_threatened),]
shp3_v2 = shp3[shp3$layer>10,]

m3 = lm(data = shp3_v2,threat_sp_prob~threat_gn,family=gaussian)
summary(m3)
m3 = summary(m3)
m3 = as.data.frame(m3$coefficients)



p3 = ggplot(data = shp3_v2,aes(x = threat_gn,y = threat_sp_prob))+geom_point(col = color.schemes$Logging[5],pch = 21,alpha = 0.25)+
  #stat_summary(fun.data= mean_cl_normal) + 
  geom_smooth(method='lm',col = 'black')+
  theme_minimal()+
  theme(axis.title.x = element_text(hjust = 0.9),
        axis.title.y = element_text(hjust = 0.9),
        axis.title = element_text(size = size_axis),
        axis.text = element_text(size = size_axis_text),
        plot.tag = element_text(face='bold'))+
  ylab('Probability of threat (Logging)')+
  xlab('Species potentially affected by threat')+
  labs(tag = 'c.')

shp3 = pollution_shp
shp3$threat_sp_prob = shp3$threat_sp/shp3$layer
#shp3_v2 = shp3[!is.nan(shp3$probability_threatened) & !is.infinite(shp3$probability_threatened),]
shp3_v2 = shp3[shp3$layer>10,]

m4 = lm(data = shp3_v2,threat_sp_prob~threat_gn,family=gaussian)
summary(m4)
m4 = summary(m4)
m4 = as.data.frame(m4$coefficients)


p4 = ggplot(data = shp3_v2,aes(x = threat_gn,y = threat_sp_prob))+geom_point(col = color.schemes$Pollution[5],pch = 21,alpha = 0.25)+
  #stat_summary(fun.data= mean_cl_normal) + 
  geom_smooth(method='lm',col = 'black')+
  theme_minimal()+
  theme(axis.title.x = element_text(hjust = 0.9),
        axis.title.y = element_text(hjust = 0.9),
        axis.title = element_text(size = size_axis),
        axis.text = element_text(size = size_axis_text),
        plot.tag = element_text(face='bold'))+
  ylab('Probability of threat (Pollution)')+
  xlab('Species potentially affected by threat')+
  labs(tag = 'd.')

shp3 = invasive_shp
shp3$threat_sp_prob = shp3$threat_sp/shp3$layer
#shp3_v2 = shp3[!is.nan(shp3$probability_threatened) & !is.infinite(shp3$probability_threatened),]
shp3_v2 = shp3[shp3$layer>10,]


m5 = lm(data = shp3_v2,threat_sp_prob~threat_gn,family=gaussian)
summary(m5)
m5 = summary(m5)
m5 = as.data.frame(m5$coefficients)


p5 = ggplot(data = shp3_v2,aes(x = threat_gn,y = threat_sp_prob))+geom_point(col = color.schemes$Invasives[5],pch = 21,alpha = 0.25)+
  #stat_summary(fun.data= mean_cl_normal) + 
  geom_smooth(method='lm',col = 'black')+
  theme_minimal()+
  theme(axis.title.x = element_text(hjust = 0.9),
        axis.title.y = element_text(hjust = 0.9),
        axis.title = element_text(size = size_axis),
        axis.text = element_text(size = size_axis_text),
        plot.tag = element_text(face='bold'))+
  ylab('Probability of threat (Invasives)')+
  xlab('Species potentially affected by threat')+
  labs(tag = 'e.')

shp3 = climate_change_shp
shp3$threat_sp_prob = shp3$threat_sp/shp3$layer
#shp3_v2 = shp3[!is.nan(shp3$probability_threatened) & !is.infinite(shp3$probability_threatened),]
shp3_v2 = shp3[shp3$layer>10,]

m6 = lm(data = shp3_v2,threat_sp_prob~threat_gn,family=gaussian)
summary(m6)
m6 = summary(m6)
m6 = as.data.frame(m6$coefficients)

p6 = ggplot(data = shp3_v2,aes(x = threat_gn,y = threat_sp_prob))+geom_point(col = color.schemes$`Climate change`[5],pch = 21,alpha = 0.25)+
  #stat_summary(fun.data= mean_cl_normal) + 
  geom_smooth(method='lm',col = 'black')+
  theme_minimal()+
  theme(axis.title.x = element_text(hjust = 0.9),
        axis.title.y = element_text(hjust = 0.9),
        axis.title = element_text(size = size_axis),
        axis.text = element_text(size = size_axis_text),
        plot.tag = element_text(face='bold'))+
  ylab('Probability of threat (Climate change)')+
  xlab('Species potentially affected by threat')+
  labs(tag = 'f.')


library(gridExtra)

pdf('regression_pt_spsv2.pdf',width = 10,height = 5)
grid.arrange(p1,p2,p3,p4,p5,p6,ncol=3)
dev.off()

regression_symbols = function(x){
  x[x<0.001] = '***'
  x[x>0.05] = ' '
  x[x<0.05 & x>0.01] = '*'
  x[x<0.01 & x>0.001] = '**'
  return(x)
}




final_table = cbind.data.frame(variable = row.names(m1),
                               Hunting = paste0(round(m1[,1],5),'(',round(m1[,2],5),')',regression_symbols(m1[,4])),
                               Agriculture = paste0(round(m2[,1],5),'(',round(m2[,2],5),')',regression_symbols(m2[,4])),
                               Logging = paste0(round(m3[,1],5),'(',round(m3[,2],5),')',regression_symbols(m3[,4])),
                               Pollution = paste0(round(m4[,1],5),'(',round(m4[,2],5),')',regression_symbols(m4[,4])),
                               Invasive = paste0(round(m5[,1],5),'(',round(m5[,2],5),')',regression_symbols(m5[,4])),
                               CChange = paste0(round(m6[,1],5),'(',round(m6[,2],5),')',regression_symbols(m6[,4])))



#not wroth it, regression figures


final_table_2 = cbind.data.frame(variable = row.names(m1),
                                 Hunting = signif(m1[,1],2),sd_A = signif(m1[,2],2),stars_A = regression_symbols(m1[,4]),
                                 Agriculture = signif(m2[,1],2),sd_R = signif(m2[,2],2),stars_R = regression_symbols(m2[,4]),
                                 Logging = signif(m3[,1],2),sd_M = signif(m3[,2],2),stars_M = regression_symbols(m3[,4]),
                                 Pollution = signif(m4[,1],2),sd_B = signif(m4[,2],2),stars_B = regression_symbols(m4[,4]),
                                 Invasive = signif(m5[,1],2),sd_B = signif(m5[,2],2),stars_B = regression_symbols(m5[,4]),
                                 CChange = signif(m6[,1],2),sd_B = signif(m6[,2],2),stars_B = regression_symbols(m6[,4]))

final_table=as.data.frame(t(final_table))
colnames(final_table) = final_table[1,]
final_table = final_table[-1,]

library(xlsx)
write.xlsx(final_table,file = 'final_table_regressions.xls')
library(reshape2)

melt(final_table,id = 'variable')


colnames(final_table)
df_melt1 = melt(final_table_2[,c(1,2,5,8,11,14,17)],id = 'variable')
df_melt2 = melt(final_table_2[,c(1,3,6,9,12,15,18)],id = 'variable')
df_melt3 = melt(final_table_2[,c(1,4,7,10,13,16,19)],id = 'variable')

melt_final = cbind.data.frame(df_melt1,df_melt2[,-1],df_melt3[,-1])
colnames(melt_final) = c('id','coeficients_name','coeficients_value','sd_name','sd_value','stars_name','stars_value')






all_ms$variable = c('Hunting','Agriculture','Logging','Polution','Invasive','Climate Change')

all_ms$variable <- factor(all_ms$variable, 
                                  levels = unique(all_ms$variable))

axis_text_size = 11
geom_point_size = 3
p1 = ggplot(melt_final[melt_final$coeficients_name == 'Hunting',], aes(x=id, y=coeficients_value,col = id,group = coeficients_name)) + 
  geom_point(position=position_dodge(width=0.75),size = geom_point_size) +
  geom_errorbar(aes(ymin=coeficients_value-sd_value, ymax=coeficients_value+sd_value), position=position_dodge(width=0.75),width = 0.2,size = 0.25)+
  coord_flip()+theme_minimal()+
  theme(axis.title = element_blank(),
        legend.position = 'none',
        axis.text.y = element_text(size = axis_text_size),
        axis.text.x = element_text(size = 10),
        legend.key.size = unit(1,"line"),
        panel.border = element_rect(colour = "grey40",fill='transparent'))+
  scale_colour_manual(values = c('deepskyblue',rep('grey40',7)))+
  geom_hline(yintercept = 0, linetype="dotted", color = "red", size=0.5)+
  #scale_x_discrete(labels = labels)+
  geom_text(aes(label=stars_value,x = id,y = coeficients_value),angle = 0,
            position = position_nudge(x = -0.25),col = 'grey70')

library(ggplot2)

nrow(sf_reptiles)
