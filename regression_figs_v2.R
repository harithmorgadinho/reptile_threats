library(sf)
library(raster)
load('reptiles_grid_probability_final_with_threatened_urbanization_any_threat.Rdata')
colnames(r_grid2)

r_grid2 = r_grid2[r_grid2$richness>9,]

pal = colorRampPalette(c("#3F7F9F","#5AB7F1","#90C15E","#F6CB45","#DA3D31"))

#  p = ggplot()+geom_sf(data = r_grid2[r_grid2$richness>15,], aes(fill = richness),col = NA)+
#    scale_fill_gradientn('',colours =c(pal(30)))+
#    theme_void()
# pdf('test_richness_v2.pdf')
# print(p)
# dev.off()


load('worldmap_merge_final_summ.Rdata')
names_iucn_regions = cbind.data.frame(larger_realm = 1:14,
                                      names = worldmap_merge_final_summ$iucn_region)

iucn_regions_grids_final2 = read.csv('iucn_regions_grids_final2.csv',row.names = 1)

min(iucn_regions_grids_final2$cell_id)

str(r_grid2)

r_grid3 = merge(r_grid2,iucn_regions_grids_final2,by.x = 'layer',by.y = 'cell_id',all.x = T)




list_cells_with_data=r_grid3[!is.na(r_grid3$hunting) | !is.na(r_grid3$agriculture) | !is.na(r_grid3$logging) | !is.na(r_grid3$pollution)
                             | !is.na(r_grid3$invasives) | !is.na(r_grid3$invasives) | !is.na(r_grid3$climate_change) | !is.na(r_grid3$urbanization),]$layer


na_sf=r_grid3[r_grid3$layer %in% list_cells_with_data & is.na(r_grid3$names),]


distance_cells_to_regions=st_distance(na_sf,worldmap_merge_final_summ)

units(distance_cells_to_regions) = NULL
final_df_nas = data.frame()
for(i in 1:nrow(distance_cells_to_regions)){
  print(i)
  temp = distance_cells_to_regions[i,]
  which(temp == min(temp))
  tempdf=cbind.data.frame(i,region = which(temp == min(temp)))
  final_df_nas = rbind(final_df_nas,tempdf) 
}
unique(final_df_nas$region)

final_df_nas$layer = na_sf$layer
final_df_nas_fixed = merge(final_df_nas,names_iucn_regions,by.x = 'region',by.y = 'larger_realm',sort = F)
final_df_nas_fixed = final_df_nas_fixed[order(final_df_nas_fixed$layer),]
na_sf$names = final_df_nas_fixed$names



r_grid3[r_grid3$layer %in% list_cells_with_data & is.na(r_grid3$names),]$names = final_df_nas_fixed$names


# p = ggplot()+geom_sf(data = r_grid3, aes(fill = names),col = NA)
# pdf('test_nas_22_4.pdf')
# print(p)
# dev.off()

r_grid3
unique(r_grid3$names)
library(reshape2)

r_grid4 = r_grid3
st_geometry(r_grid4) = NULL
colnames(r_grid4)[13] = 'iucn_region'

r_grid4_melt0 = melt(r_grid4[,c(1,3:9,11,13)],id = c('layer','iucn_region'))
r_grid4_melt1 = melt(r_grid4[,c(1,10)],id = c('layer'))

r_grid4_melt = merge(r_grid4_melt0,r_grid4_melt1,by.x = 'layer',by.y = 'layer')

r_grid4_melt[is.na(r_grid4_melt$layer),]

r_grid4_melt0[is.na(r_grid4_melt0$layer),]
r_grid4_melt1[is.na(r_grid4_melt1$layer),]


nrow(r_grid4_melt)
head(r_grid4_melt)

table(r_grid4_melt$layer)
unique(r_grid4_melt$variable.x)

library(ggplot2)
library(RColorBrewer)
library(scales)

urb_pal=colorRampPalette(c('white','black'))

color.schemes<-list(
  "Logging" = brewer.pal(8,"Greens"),
  "Agriculture" = rev(brewer.pal(11,"BrBG")[1:5]),
  "Hunting" = brewer.pal(8,"Reds"),
  "Pollution" = brewer.pal(8,"Blues"),
  "Invasives" = brewer.pal(8,"Purples"),
  "Climate change" = brewer.pal(8,"Oranges"),
  "Urbanization" = urb_pal(8))

size_axis = 8
size_axis_text = 7
threats_pal = c(color.schemes$Hunting[5],
                color.schemes$Agriculture[3],
                color.schemes$Logging[5],
                color.schemes$Pollution[5],
                color.schemes$Invasives[5],
                color.schemes$`Climate change`[5],
                'black')


biomes_pal = c('firebrick','goldenrod','pink','steelblue','limegreen','purple')
iucn_regions_pal = c('lightblue','goldenrod','orange','#A5BE00','firebrick','dodgerblue','purple','gold','#588E29','#654321','red','#AA9ABA')
iucn_regions_pal = c('lightblue','pink','#654321','#A5BE00','firebrick','dodgerblue','purple','gold','#588E29','orange','red','#AA9ABA')

sort(unique(r_grid4_melt$iucn_region))

library(gridExtra)
library(png)
library(grid)

img <- readPNG('cat.png', native = TRUE)
cat_img <- rasterGrob(img, interpolate=TRUE)

img <- readPNG('saw.png', native = TRUE)
saw_img <- rasterGrob(img, interpolate=TRUE)

img <- readPNG('factory.png', native = TRUE)
factory_img <- rasterGrob(img, interpolate=TRUE)

img <- readPNG('wolf-icon-png-2858.png', native = TRUE)
hunting_img <- rasterGrob(img, interpolate=TRUE)

img <- readPNG('agriculture.png', native = TRUE)
agriculture_img <- rasterGrob(img, interpolate=TRUE)

img <- readPNG('climate_change.png', native = TRUE)
climate_c_img <- rasterGrob(img, interpolate=TRUE)

img <- readPNG('urbanization2.png', native = TRUE)
urbanization_img <- rasterGrob(img, interpolate=TRUE)

unique(r_grid4_melt$iucn_region)


r_grid4_melt[is.na(r_grid4_melt$layer),]



r_grid4_melt = r_grid4_melt[!r_grid4_melt$iucn_region %in% c('Antartic','Antarctica'),]
r_grid4_melt[is.na(r_grid4_melt$layer),]

#making the models


list_threats = sort(unique(r_grid4_melt$variable.x))
list_iucn_regions = sort(unique(r_grid4_melt$iucn_region))

finaldf2 = data.frame()
for(i in seq_along(list_threats)){
  threat = list_threats[i]
  print(threat)
  finaldf = data.frame()
  for(m in seq_along(list_iucn_regions)){
    region = list_iucn_regions[m]
    lm_temp = lm(data = r_grid4_melt[r_grid4_melt$variable.x == threat & r_grid4_melt$iucn_region == region,],value.y~value.x)
    lm_temp = summary(lm_temp)
    lm_temp = as.data.frame(lm_temp$coefficients)
    temp_df=cbind.data.frame(region,lm_temp[1,],lm_temp[2,])
    colnames(temp_df)[2] = 'Intercept'
    colnames(temp_df)[6] = 'Coefficient'
    finaldf = rbind(finaldf,temp_df)
  }
  finaldf2_temp = cbind.data.frame(threat,finaldf)
  finaldf2 = rbind(finaldf2,finaldf2_temp)
}

row.names(finaldf2) = NULL

r_grid4_melt$iucn_region = factor(r_grid4_melt$iucn_region,levels = 
                                           c('North America','Mesoamerica','Caribbean Islands','South America',
                                             'Europe','North Africa','Sub-Saharan Africa', 'West and Central Asia', 
                                             'North Asia','East Asia','South and Southeast Asia','Oceania'))


list_regions = c('North America','Mesoamerica','Caribbean Islands','South America',
                 'Europe','North Africa','Sub-Saharan Africa', 'West and Central Asia', 
                 'North Asia','East Asia','South and Southeast Asia','Oceania')

regression_symbols = function(x){
  x[x<0.001] = '***'
  x[x>0.05] = ' '
  x[x<0.05 & x>0.01] = '*'
  x[x<0.01 & x>0.001] = '**'
  return(x)
}

max(finaldf2$Coefficient)
min(finaldf2$Coefficient)

finaldf2$symbols = regression_symbols(finaldf2[,10])

unique(r_grid4_melt$variable.x)
r_grid4_melt$variable.x = factor(r_grid4_melt$variable.x, levels = c('logging','pollution','agriculture','invasives','hunting','climate_change','urbanization','any_threat'))

load('df_cols.Rdata')

#option A - separated----
list_threats
list_plots = list()
for(i in seq_along(list_regions)){
  test = data.frame(x = 0.5,y = -0.5, lab=finaldf2[finaldf2$region == list_regions[i],][,c(1,11)]$symbols,variable.x=list_threats)
  
  p = ggplot(data = r_grid4_melt[r_grid4_melt$iucn_region %in% list_regions[i],],aes(x = value.x,y = value.y))+
    annotate("rect", xmin = -Inf, xmax =Inf, ymin = -Inf, ymax = Inf, fill = df_cols$iucn_regions_pal[i], alpha = 0.25) +
    #geom_text(data = finaldf2[finaldf2$region == list_regions[i],][,c(1,11)],x = 0.5,y = -0.5,aes(label = symbols))+
    geom_smooth(method='lm',alpha = 0.75,
                #aes(col = variable.x),
                col = df_cols$iucn_regions_pal[i],
                size = 2)+
    facet_grid(~variable.x)+
    theme_void()+
    theme(
      axis.text.y = element_blank(),
      axis.text.x = element_blank(),
      legend.position = 'none',
      plot.margin = unit(c(0,.1,0,0),'cm'),
      panel.background = element_rect(fill = NA,colour = NA),
      panel.grid.major = element_line(colour = 'grey90'),
      strip.text.x = element_blank(),
      strip.background = element_blank())+
    scale_y_continuous(limits = c(-1,1))+
    scale_x_continuous(limits = c(0,1))+
    geom_text(aes(x, y, label=lab),data=test,size = 4)
  
  list_plots[[i]] =  p
}

threats_pal = c(threats_pal[3],
                threats_pal[5],
                threats_pal[2],
                threats_pal[4],
                threats_pal[1],
                threats_pal[6],
                threats_pal[7])
#option B - together----
list_threats
list_plots = list()
colnames(r_grid3)

list_regions2 = list()
list_regions2[[1]] = c(list_regions[1])
list_regions2[[2]] = c(list_regions[2],list_regions[3],list_regions[4])
list_regions2[[3]] = c(list_regions[5],list_regions[8],list_regions[9],list_regions[10])
list_regions2[[4]] = c(list_regions[6],list_regions[7])
list_regions2[[5]] = c(list_regions[11],list_regions[12])


temp = df_cols[6,]
df_cols[6,] = df_cols[7,]
df_cols[7,] = temp

unique(r_grid4_melt$iucn_region)

order_regions = c('West and Central Asia','North America','Europe','North Asia','East Asia','Sub-Saharan Africa','North Africa','South and Southeast Asia','Mesoamerica',
'Caribbean Islands','South America','Oceania')

r_grid4_melt$iucn_region = factor(r_grid4_melt$iucn_region,levels = order_regions)

for(i in seq_along(list_regions2)){
  test = data.frame(x = 0.5,y = -0.5, lab=finaldf2[finaldf2$region %in% unlist(list_regions2[i]),][,c(1,2,11)])
  pal_temp = df_cols[df_cols$var1 %in% unlist(list_regions2[i]),]$iucn_regions_pal
  
  to_remove = paste0(test[test$lab.symbols ==' ',]$lab.threat,'_',test[test$lab.symbols ==' ',]$lab.region)
  to_plot = r_grid4_melt[r_grid4_melt$iucn_region %in% unlist(list_regions2[i]),]
  to_plot$remove_list = paste0(to_plot$variable.x,'_',to_plot$iucn_region)
  to_plot = to_plot[!to_plot$remove_list %in% to_remove,]
  
  p = ggplot(data = to_plot,aes(x = value.x,y = value.y))+
    annotate("rect", xmin = -Inf, xmax =Inf, ymin = -Inf, ymax = Inf, fill = NA, alpha = 0.25) +
    geom_point(aes(fill = iucn_region),size = 0.25,alpha = 1,col = 'transparent',shape = 21,stroke=0.01)+
    geom_smooth(method = lm, size = 0.75, color = alpha('black',0.5), se = FALSE,aes(group = iucn_region)) + 
    stat_smooth(method='lm',alpha = 0.5,aes(col = iucn_region),size = 0.55)+
    facet_grid(~variable.x)+
    theme_void()+
    theme(
      axis.text.y = element_blank(),
      axis.text.x = element_blank(),
      legend.position = 'none',
      plot.margin = unit(c(0,0,0,0),'cm'),
      panel.background = element_rect(fill = NA,colour = NA),
      panel.grid.major = element_line(colour = 'grey90',linewidth = 0.1),
      strip.text.x = element_blank(),
      panel.spacing = unit(0, "cm"),
      strip.background = element_blank())+
    scale_y_continuous(limits = c(-0.5,1))+
    scale_x_continuous(limits = c(0,1))+
    scale_colour_manual(values = alpha(pal_temp,0.9))+
    scale_fill_manual(values = alpha(pal_temp,0.65))
  
    #geom_text(aes(x, y, label=lab),data=test,size = 4)
  
  list_plots[[i]] =  p
}
#----
grid.arrange(list_plots[[1]],list_plots[[2]],list_plots[[3]],list_plots[[4]],list_plots[[5]],ncol = 1)
grid_regression = arrangeGrob(list_plots[[1]],list_plots[[2]],list_plots[[3]],list_plots[[4]],list_plots[[5]],ncol = 1)




#world analysis
finaldf = data.frame()
for(i in seq_along (list_threats)){
  lm_temp = lm(data = r_grid4_melt[r_grid4_melt$variable.x %in% list_threats[i],],value.y~value.x)
  lm_temp = summary(lm_temp)
  lm_temp = as.data.frame(lm_temp$coefficients)
  temp_df=cbind.data.frame(threat = list_threats[i],region = 'World',lm_temp[1,],lm_temp[2,])
  colnames(temp_df)[2] = 'Intercept'
  colnames(temp_df)[6] = 'Coefficient'
  finaldf = rbind(finaldf,temp_df)
}

row.names(finaldf) = NULL

finaldf$symbols = regression_symbols(finaldf[,10])

test = data.frame(x = 0.5,y = -0.5, lab=finaldf$symbols,
                  variable.x=list_threats)



p_world = ggplot(data = r_grid4_melt,aes(x = value.x,y = value.y))+
  annotate("rect", xmin = -Inf, xmax =Inf, ymin = -Inf, ymax = Inf, fill = 'black', alpha = 0) +
  geom_point(fill = 'black',size = 0.25,alpha = 1,col = 'transparent',shape = 21,stroke=0.01)+
  geom_smooth(method='lm',alpha = 0,col = 'white',size = 0.75)+
  geom_smooth(method='lm',alpha = 0.5,col = 'black',size = 0.5)+
  facet_grid(~variable.x)+
  theme_void()+
  theme(
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    legend.position = 'none',
    plot.margin = unit(c(0,.1,0,0),'cm'),
    panel.background = element_rect(fill = NA,colour = NA),
    panel.grid.major = element_line(colour = 'grey90'),
    strip.text.x = element_blank(),
    strip.background = element_blank())+
  scale_y_continuous(limits = c(-0.5,1))+
  scale_x_continuous(limits = c(0,1))

# pdf('regression_lms_plotsv6.pdf',width = 6,height = 4)
# grid.arrange(p_world,list_plots[[1]],list_plots[[2]],list_plots[[3]],list_plots[[4]],list_plots[[5]],ncol = 1)
# dev.off()

grid_regression = arrangeGrob(p_world,list_plots[[1]],list_plots[[2]],list_plots[[3]],list_plots[[4]],list_plots[[5]],ncol = 1)

ggsave(plot = grid_regression,filename = 'grid_regression_28_9_23.png',width = 6,height = 4,dpi = 1000)

#for the separate version


  
p_world = ggplot(data = r_grid4_melt,aes(x = value.x,y = value.y))+
  annotate("rect", xmin = -Inf, xmax =Inf, ymin = -Inf, ymax = Inf, fill = 'black', alpha = 0) +
  #geom_point(fill = 'black',size = 0.25,alpha = 1,col = 'transparent',shape = 21,stroke=0.01)+
  geom_smooth(method='lm',alpha = 0.75,col = 'black',size = 2)+
  facet_grid(~variable.x)+
  theme_void()+
  theme(
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    legend.position = 'none',
    plot.margin = unit(c(0,0,0,0),'cm'),
    panel.background = element_rect(fill = NA,colour = NA),
    panel.grid.major = element_line(colour = 'grey90',linewidth = 0.1),
    strip.text.x = element_blank(),
    panel.spacing = unit(0, "cm"),
    strip.background = element_blank())+
  scale_y_continuous(limits = c(-0.5,1))+
  scale_x_continuous(limits = c(0,1))+
  geom_text(aes(x, y, label=lab),data=test,size = 4)

grid_regression = arrangeGrob(p_world,list_plots[[1]],list_plots[[2]],list_plots[[3]],list_plots[[4]],list_plots[[5]],list_plots[[6]],list_plots[[7]],list_plots[[8]],list_plots[[9]],list_plots[[10]],list_plots[[11]],list_plots[[12]],ncol = 1)
ggsave(plot = grid_regression,filename = 'grid_regression_28_9_23_separate.png',width = 12,height = 20,dpi = 1000)


library(openxlsx)
write.xlsx(finaldf2,file = 'finaldf2_table_regressions.xlsx')


df_summary=r_grid4_melt %>% group_by(iucn_region,variable.x) %>% 
  summarise(median = median(value.x),sd = sd(value.x))

df_n=r_grid4_melt %>% group_by(iucn_region) %>% 
  summarise(n = n())

df_summary_all_world=r_grid4_melt %>% group_by(variable.x) %>% 
  summarise(median = median(value.x),sd = sd(value.x))

df_summary_all_world$iucn_region = 'all_world'

df_summary = rbind(df_summary,df_summary_all_world)

df_n$percentage = 100*df_n$n/sum(df_n$n)

df_summary_melt = melt(df_summary,id = c('iucn_region','variable.x'))
df_summary_melt_plot = ggplot(data = df_summary_melt)+
  geom_bar(aes(x = value,y = iucn_region,fill = variable),position = 'dodge',stat = 'identity',col='black',size = 0.5)+
  facet_wrap(~variable.x,nrow = 1)+
  geom_text(aes(x = value,y = iucn_region,label = round(value,2),group = variable),
            position = position_dodge(width = 0.9),hjust=0,size = 3)+
  theme_minimal()+
  theme(legend.position = 'bottom',axis.title = element_blank(),axis.text.x = element_blank(),
        axis.text.y = element_text(size = 7))+
  theme(plot.margin = margin(0,0,0,0, "cm"))+
  scale_x_continuous(limits = c(0,1.2))+
  scale_fill_manual('',values = c('steelblue','grey70'))

pdf('df_summary_melt_plot.pdf',width = 10,height = 6)
print(df_summary_melt_plot)
dev.off()


library(openxlsx)

write.xlsx(df_summary,file = 'df_summary.xlsx')
write.xlsx(df_n,file = 'df_n.xlsx')

r_grid4_melt
p1 = ggplot(data = r_grid4_melt)+
  geom_boxplot(aes(fill = variable.x,x = value.x,y  = iucn_region),outlier.size = 0.1)
p2 = ggplot(data = r_grid4_melt)+
  geom_boxplot(aes(fill = variable.y,x = value.y,y  = iucn_region),outlier.size = 0.1)



df_richness = r_grid4 %>% group_by(iucn_region) %>% summarise(median = median(richness),avg = mean(richness),sd = sd(richness))

df_richness$copy_paste = paste0(df_richness$median,'(',round(df_richness$sd,2),')')
write.xlsx(df_richness,'df_richness.xlsx')


