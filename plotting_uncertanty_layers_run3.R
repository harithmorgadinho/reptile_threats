#uncertainty_map
#threat_layers
library(sf)
sf::sf_use_s2(FALSE)
library(raster)

library(dplyr)

load('/Users/gdt366/Dropbox/Postdoc_socioeconomic/r_grid_13_dec.Rdata')

load('probability_any_threat.Rdata')

r_grid$any_threat_r = probability_any_threat

r_grid[r_grid$richness_r<10,]$hunting_r = NA
r_grid[r_grid$richness_r<10,]$agriculture_r = NA
r_grid[r_grid$richness_r<10,]$logging_r = NA
r_grid[r_grid$richness_r<10,]$pollution_r = NA
r_grid[r_grid$richness_r<10,]$invasives_r = NA
r_grid[r_grid$richness_r<10,]$climate_change_r = NA
r_grid[r_grid$richness_r<10,]$urbanization_r = NA
r_grid[r_grid$richness_r<10,]$any_threat_r = NA


num_quantiles <- 10 # For quantiles


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

legend.title.size = 10
annotate.size = 3
size_legend_text = 7
legend_height = 0.5

colnames(r_grid)

get_decile_group <- function(x) {
  if(is.na(x) || x == 0) return(NA)
  sum(x > deciles) # This returns the decile group
}

valid_x = r_grid[!is.na(r_grid$hunting_r_unc_med) & r_grid$land %in%1,]$hunting_r_unc_med
deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
r_grid$hunting_r_unc_med_dec = NA
r_grid[!is.na(r_grid$hunting_r_unc_med) & r_grid$land %in%1,]$hunting_r_unc_med_dec = sapply(valid_x, get_decile_group)

table(r_grid$hunting_r_unc_med_dec)

valid_x = r_grid[!is.na(r_grid$agriculture_r_unc_med) & r_grid$land %in%1,]$agriculture_r_unc_med
deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
r_grid$agriculture_r_unc_med_dec = NA
r_grid[!is.na(r_grid$agriculture_r_unc_med) & r_grid$land %in%1,]$agriculture_r_unc_med_dec = sapply(valid_x, get_decile_group)



valid_x = r_grid[!is.na(r_grid$logging_r_unc_med) & r_grid$land %in%1,]$logging_r_unc_med
deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
r_grid$logging_r_unc_med_dec = NA
r_grid[!is.na(r_grid$logging_r_unc_med) & r_grid$land %in%1,]$logging_r_unc_med_dec = sapply(valid_x, get_decile_group)

valid_x = r_grid[!is.na(r_grid$pollution_r_unc_med) & r_grid$land %in%1,]$pollution_r_unc_med
deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
r_grid$pollution_r_unc_med_dec = NA
r_grid[!is.na(r_grid$pollution_r_unc_med) & r_grid$land %in%1,]$pollution_r_unc_med_dec = sapply(valid_x, get_decile_group)


valid_x = r_grid[!is.na(r_grid$invasives_r_unc_med) & r_grid$land %in%1,]$invasives_r_unc_med
deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
r_grid$invasives_r_unc_med_dec = NA
r_grid[!is.na(r_grid$invasives_r_unc_med) & r_grid$land %in%1,]$invasives_r_unc_med_dec = sapply(valid_x, get_decile_group)


valid_x = r_grid[!is.na(r_grid$climate_change_r_unc_med) & r_grid$land %in%1,]$climate_change_r_unc_med
deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
r_grid$climate_change_r_unc_med_dec = NA
r_grid[!is.na(r_grid$climate_change_r_unc_med) & r_grid$land %in%1,]$climate_change_r_unc_med_dec = sapply(valid_x, get_decile_group)


valid_x = r_grid[!is.na(r_grid$urbanization_r_unc_med) & r_grid$land %in%1,]$urbanization_r_unc_med
deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
r_grid$urbanization_r_unc_med_dec = NA
r_grid[!is.na(r_grid$urbanization_r_unc_med) & r_grid$land %in%1,]$urbanization_r_unc_med_dec = sapply(valid_x, get_decile_group)

r_grid[r_grid$uncertainty_r_any_threat_med %in% 0,]$uncertainty_r_any_threat_med = NA

valid_x = r_grid[!is.na(r_grid$uncertainty_r_any_threat_med) & r_grid$land %in%1,]$uncertainty_r_any_threat_med
deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
r_grid$uncertainty_r_any_threat_med_dec = NA
r_grid[!is.na(r_grid$uncertainty_r_any_threat_med) & r_grid$land %in%1,]$uncertainty_r_any_threat_med_dec = sapply(valid_x, get_decile_group)


pal = colorRampPalette(brewer.pal(6,'BrBG'))

theme_harith = theme(legend.title = element_text(angle = -90,hjust = 0.5,size = legend.title.size),
                     legend.key.width = unit(0.5, 'cm'),
                     legend.key.height = unit(legend_height, 'cm'),
                     panel.background = element_blank(),
                     axis.title = element_blank(),
                     axis.text.x = element_blank(),
                     axis.text.y = element_blank(),
                     axis.ticks = element_blank(),
                     legend.text = element_text(size = size_legend_text),
                     plot.tag = element_text(face='bold'),
                     legend.background = element_blank(),
                     plot.background=element_rect(fill="transparent",colour=NA),
                     legend.key = element_rect(fill = "transparent", colour = "transparent"))

p1 = ggplot() +
  geom_sf(data = sf_land_union,linewidth = 0,bg = 'grey70',col =  'grey70') +
  #geom_sf(data = r_grid[r_grid$land_areas == 1,],linewidth = 0.0001,bg = 'grey90',col = 'grey90') +
  geom_sf(data = r_grid[r_grid$hunting_r_unc_med_dec>0,],aes(fill = as.factor(hunting_r_unc_med_dec),col = as.factor(hunting_r_unc_med_dec)),linewidth = 0) +
  scale_fill_manual('Impact uncertainty',values = rev(pal(10)),na.value = NA,na.translate = F)+
  scale_colour_manual('Impact uncertainty',values = rev(pal(10)),na.value = NA,na.translate = F)+
  annotate('text',x = 16382470,y = -7053445,label = 'Hunting',size = annotate.size)+
  theme_harith+
  guides(fill = guide_legend(title.position = "right",reverse = T))+
  guides(col = guide_legend(title.position = "right",reverse = T))+
  labs(tag = 'e')+
  annotation_custom(hunting_img, xmin=-17182470, xmax=-2000000, ymin=-8853445, ymax= -3700000)


p2 = ggplot() +
  geom_sf(data = sf_land_union,linewidth = 0,bg = 'grey70',col =  'grey70') +
  #geom_sf(data = r_grid[r_grid$land_areas == 1,],linewidth = 0.0001,bg = 'grey90',col = 'grey90') +
  geom_sf(data = r_grid[r_grid$agriculture_r_unc_med_dec>0,],aes(fill = as.factor(agriculture_r_unc_med_dec),col = as.factor(agriculture_r_unc_med_dec)),linewidth = 0) +
  scale_fill_manual('Impact uncertainty',values = rev(pal(10)),na.value = NA,na.translate = F)+
  scale_colour_manual('Impact uncertainty',values = rev(pal(10)),na.value = NA,na.translate = F)+
  annotate('text',x = 16382470,y = -7053445,label = 'Agriculture',size = annotate.size)+
  theme_harith+
  guides(fill = guide_legend(title.position = "right",reverse = T))+
  guides(col = guide_legend(title.position = "right",reverse = T))+
  labs(tag = 'c')+
  annotation_custom(agriculture_img, xmin=-17182470, xmax=-3000000, ymin=-8853445, ymax= -3700000)

p3 = ggplot() +
  geom_sf(data = sf_land_union,linewidth = 0,bg = 'grey70',col =  'grey70') +
  #geom_sf(data = r_grid[r_grid$land_areas == 1,],linewidth = 0.0001,bg = 'grey90',col = 'grey90') +
  geom_sf(data = r_grid[r_grid$logging_r_unc_med_dec>0,],aes(fill = as.factor(logging_r_unc_med_dec),col = as.factor(logging_r_unc_med_dec)),linewidth = 0) +
  scale_fill_manual('Impact uncertainty',values = rev(pal(10)),na.value = NA,na.translate = F)+
  scale_colour_manual('Impact uncertainty',values = rev(pal(10)),na.value = NA,na.translate = F)+
  annotate('text',x = 16382470,y = -7053445,label = 'Logging',size = annotate.size)+
  theme_harith+
  guides(fill = guide_legend(title.position = "right",reverse = T))+
  guides(col = guide_legend(title.position = "right",reverse = T))+
  labs(tag = 'a')+
  annotation_custom(saw_img, xmin=-17182470, xmax=-3000000, ymin=-8853445, ymax= -3700000)


p4 = ggplot() +
  geom_sf(data = sf_land_union,linewidth = 0,bg = 'grey70',col =  'grey70') +
  #geom_sf(data = r_grid[r_grid$land_areas == 1,],linewidth = 0.0001,bg = 'grey90',col = 'grey90') +
  geom_sf(data = r_grid[r_grid$pollution_r_unc_med_dec>0,],aes(fill = as.factor(pollution_r_unc_med_dec),col = as.factor(pollution_r_unc_med_dec)),linewidth = 0) +
  scale_fill_manual('Impact uncertainty',values = rev(pal(10)),na.value = NA,na.translate = F)+
  scale_colour_manual('Impact uncertainty',values = rev(pal(10)),na.value = NA,na.translate = F)+
  annotate('text',x = 16382470,y = -7053445,label = 'Pollution',size = annotate.size)+
  theme_harith+
  guides(fill = guide_legend(title.position = "right",reverse = T))+
  guides(col = guide_legend(title.position = "right",reverse = T))+
  labs(tag = 'b')+
  annotation_custom(factory_img, xmin=-16782470, xmax=-3000000, ymin=-7753445, ymax= -3700000)

p5 = ggplot() +
  geom_sf(data = sf_land_union,linewidth = 0,bg = 'grey70',col =  'grey70') +
  #geom_sf(data = r_grid[r_grid$land_areas == 1,],linewidth = 0.0001,bg = 'grey90',col = 'grey90') +
  geom_sf(data = r_grid[r_grid$invasives_r_unc_med_dec>0,],aes(fill = as.factor(invasives_r_unc_med_dec),col = as.factor(invasives_r_unc_med_dec)),linewidth = 0) +
  scale_fill_manual('Impact uncertainty',values = rev(pal(10)),na.value = NA,na.translate = F)+
  scale_colour_manual('Impact uncertainty',values = rev(pal(10)),na.value = NA,na.translate = F)+
  annotate('text',x = 16382470,y = -7053445,label = 'Invasives',size = annotate.size)+
  theme_harith+
  guides(fill = guide_legend(title.position = "right",reverse = T))+
  guides(col = guide_legend(title.position = "right",reverse = T))+
  labs(tag = 'd')+
  annotation_custom(cat_img, xmin=-17182470, xmax=-3000000, ymin=-8853445, ymax= -3700000)



p6 = ggplot() +
  geom_sf(data = sf_land_union,linewidth = 0,bg = 'grey70',col =  'grey70') +
  #geom_sf(data = r_grid[r_grid$land_areas == 1,],linewidth = 0.0001,bg = 'grey90',col = 'grey90') +
  geom_sf(data = r_grid[r_grid$climate_change_r_unc_med_dec>0,],aes(fill = as.factor(climate_change_r_unc_med_dec),col = as.factor(climate_change_r_unc_med_dec)),linewidth = 0) +
  scale_fill_manual('Impact uncertainty',values = rev(pal(10)),na.value = NA,na.translate = F)+
  scale_colour_manual('Impact uncertainty',values = rev(pal(10)),na.value = NA,na.translate = F)+
  annotate('text',x = 15082470,y = -7053445,label = 'Climate Change',size = annotate.size)+
  theme_harith+
  guides(fill = guide_legend(title.position = "right",reverse = T))+
  guides(col = guide_legend(title.position = "right",reverse = T))+
  labs(tag = 'f')+
  annotation_custom(climate_c_img, xmin=-17182470, xmax=-3000000, ymin=-8853445, ymax= -3700000)



p7 = ggplot() +
  geom_sf(data = sf_land_union,linewidth = 0,bg = 'grey70',col =  'grey70') +
  #geom_sf(data = r_grid[r_grid$land_areas == 1,],linewidth = 0.0001,bg = 'grey90',col = 'grey90') +
  geom_sf(data = r_grid[r_grid$urbanization_r_unc_med_dec>0,],aes(fill = as.factor(urbanization_r_unc_med_dec),col = as.factor(urbanization_r_unc_med_dec)),linewidth = 0) +
  scale_fill_manual('Impact uncertainty',values = rev(pal(10)),na.value = NA,na.translate = F)+
  scale_colour_manual('Impact uncertainty',values = rev(pal(10)),na.value = NA,na.translate = F)+
  annotate('text',x = 15682470,y = -7053445,label = 'Urbanization',size = annotate.size)+
  theme_harith+
  guides(fill = guide_legend(title.position = "right",reverse = T))+
  guides(col = guide_legend(title.position = "right",reverse = T))+
  labs(tag = 'g')+
  annotation_custom(urbanization_img, xmin=-17182470, xmax=-3000000, ymin=-7753445, ymax= -3700000)



r_grid$uncertainty_r_any_threat_med_dec
table(r_grid$uncertainty_r_any_threat_med_dec)

p8 = ggplot() +
  geom_sf(data = sf_land_union,linewidth = 0,bg = 'grey70',col =  'grey70') +
  #geom_sf(data = r_grid[r_grid$land_areas == 1,],linewidth = 0.0001,bg = 'grey90',col = 'grey90') +
  geom_sf(data = r_grid[r_grid$uncertainty_r_any_threat_med_dec>0,],aes(fill = as.factor(uncertainty_r_any_threat_med_dec),col = as.factor(uncertainty_r_any_threat_med_dec)),linewidth = 0) +
  scale_fill_manual('Impact uncertainty',values = rev(pal(10)),na.value = NA,na.translate = F)+
  scale_colour_manual('Impact uncertainty',values = rev(pal(10)),na.value = NA,na.translate = F)+
  annotate('text',x = 15682470,y = -7053445,label = 'Any threat',size = annotate.size)+
  theme_harith+
  guides(fill = guide_legend(title.position = "right",reverse = T))+
  guides(col = guide_legend(title.position = "right",reverse = T))+
  labs(tag = 'h')



figSM2_grid = arrangeGrob(p3,p4,p2,p5,p1,p6,p7,p8,ncol=2)

ggsave(file = 'figure_uncertainty_Dec_14.png',plot = figSM2_grid,width = 10,height = 9,dpi = 2000)






colnames(r_grid_uncertainty_layers)
list_groups = c('_a','_r','_m','_b')
list_threats = c('_logging','_agriculture','_hunting','_pollution','_invasives','_climate_change','_urbanization')
list_plots1 = c(paste0('uncertainty',list_groups[1],list_threats),"uncertainty_layer_amphibians")
list_plots2 = c(paste0('uncertainty',list_groups[2],list_threats),"uncertainty_layer_reptiles")
list_plots3 = c(paste0('uncertainty',list_groups[3],list_threats),"uncertainty_layer_mammals")
list_plots4 = c(paste0('uncertainty',list_groups[4],list_threats),"uncertainty_layer_birds")

list_plots = list(list_plots1,list_plots2,list_plots3,list_plots4)

r_grid_plotting = r_grid

for(i in seq_along(list_plots)){
  print(i)
  r_grid_plotting$plotting = r_grid_plotting[[list_plots[[i]][[1]]]]
  p1 = ggplot()+geom_sf(data = r_grid_plotting,aes(col = plotting))+
    scale_color_gradientn(list_plots[[i]][[1]],colours = terrain.colors(100),limits = c(1,max(na.omit(r_grid_plotting$plotting))),na.value = 'grey90')
  r_grid_plotting$plotting = r_grid_plotting[[list_plots[[i]][[2]]]]
  
  p2 = ggplot()+geom_sf(data = r_grid_plotting,aes(col = plotting))+
    scale_color_gradientn(list_plots[[i]][[2]],colours = terrain.colors(100),limits = c(1,max(na.omit(r_grid_plotting$plotting))),na.value = 'grey90')
  r_grid_plotting$plotting = r_grid_plotting[[list_plots[[i]][[3]]]]
  
  p3 = ggplot()+geom_sf(data = r_grid_plotting,aes(col = plotting))+
    scale_color_gradientn(list_plots[[i]][[3]],colours = terrain.colors(100),limits = c(1,max(na.omit(r_grid_plotting$plotting))),na.value = 'grey90')
  r_grid_plotting$plotting = r_grid_plotting[[list_plots[[i]][[4]]]]
  
  p4 = ggplot()+geom_sf(data = r_grid_plotting,aes(col = plotting))+
    scale_color_gradientn(list_plots[[i]][[4]],colours = terrain.colors(100),limits = c(1,max(na.omit(r_grid_plotting$plotting))),na.value = 'grey90')
  r_grid_plotting$plotting = r_grid_plotting[[list_plots[[i]][[5]]]]
  
  p5 = ggplot()+geom_sf(data = r_grid_plotting,aes(col = plotting))+
    scale_color_gradientn(list_plots[[i]][[5]],colours = terrain.colors(100),limits = c(1,max(na.omit(r_grid_plotting$plotting))),na.value = 'grey90')
  r_grid_plotting$plotting = r_grid_plotting[[list_plots[[i]][[6]]]]
  
  p6 = ggplot()+geom_sf(data = r_grid_plotting,aes(col = plotting))+
    scale_color_gradientn(list_plots[[i]][[6]],colours = terrain.colors(100),limits = c(1,max(na.omit(r_grid_plotting$plotting))),na.value = 'grey90')
  r_grid_plotting$plotting = r_grid_plotting[[list_plots[[i]][[7]]]]
  
  p7 = ggplot()+geom_sf(data = r_grid_plotting,aes(col = plotting))+
    scale_color_gradientn(list_plots[[i]][[7]],colours = terrain.colors(100),limits = c(1,max(na.omit(r_grid_plotting$plotting))),na.value = 'grey90')
  r_grid_plotting$plotting = r_grid_plotting[[list_plots[[i]][[8]]]]
  
  p8 = ggplot()+geom_sf(data = r_grid_plotting,aes(col = plotting))+
    scale_color_gradientn(list_plots[[i]][[8]],colours = terrain.colors(100),limits = c(1,max(na.omit(r_grid_plotting$plotting))),na.value = 'grey90')
  
  p_all = arrangeGrob(p1,p2,p3,p4,p5,p6,p7,p8,ncol=2)
  
  ggsave(filename = paste0(list_groups[i],'p_all','.pdf'),width = 10,height = 8,plot = p_all)
  
}


list_plots1 = c(paste0('uncertainty',list_groups[1],list_threats,'_med'),"uncertainty_layer_amphibians_med")
list_plots2 = c(paste0('uncertainty',list_groups[2],list_threats,'_med'),"uncertainty_layer_reptiles_med")
list_plots3 = c(paste0('uncertainty',list_groups[3],list_threats,'_med'),"uncertainty_layer_mammals_med")
list_plots4 = c(paste0('uncertainty',list_groups[4],list_threats,'_med'),"uncertainty_layer_birds_med")

list_plots = list(list_plots1,list_plots2,list_plots3,list_plots4)

r_grid_plotting = r_grid

for(i in seq_along(list_plots)){
  print(i)
  r_grid_plotting$plotting = r_grid_plotting[[list_plots[[i]][[1]]]]
  p1 = ggplot()+geom_sf(data = r_grid_plotting,aes(col = plotting))+
    scale_color_gradientn(list_plots[[i]][[1]],colours = terrain.colors(100),limits = c(1,max(na.omit(r_grid_plotting$plotting))),na.value = 'grey90')
  r_grid_plotting$plotting = r_grid_plotting[[list_plots[[i]][[2]]]]
  
  p2 = ggplot()+geom_sf(data = r_grid_plotting,aes(col = plotting))+
    scale_color_gradientn(list_plots[[i]][[2]],colours = terrain.colors(100),limits = c(1,max(na.omit(r_grid_plotting$plotting))),na.value = 'grey90')
  r_grid_plotting$plotting = r_grid_plotting[[list_plots[[i]][[3]]]]
  
  p3 = ggplot()+geom_sf(data = r_grid_plotting,aes(col = plotting))+
    scale_color_gradientn(list_plots[[i]][[3]],colours = terrain.colors(100),limits = c(1,max(na.omit(r_grid_plotting$plotting))),na.value = 'grey90')
  r_grid_plotting$plotting = r_grid_plotting[[list_plots[[i]][[4]]]]
  
  p4 = ggplot()+geom_sf(data = r_grid_plotting,aes(col = plotting))+
    scale_color_gradientn(list_plots[[i]][[4]],colours = terrain.colors(100),limits = c(1,max(na.omit(r_grid_plotting$plotting))),na.value = 'grey90')
  r_grid_plotting$plotting = r_grid_plotting[[list_plots[[i]][[5]]]]
  
  p5 = ggplot()+geom_sf(data = r_grid_plotting,aes(col = plotting))+
    scale_color_gradientn(list_plots[[i]][[5]],colours = terrain.colors(100),limits = c(1,max(na.omit(r_grid_plotting$plotting))),na.value = 'grey90')
  r_grid_plotting$plotting = r_grid_plotting[[list_plots[[i]][[6]]]]
  
  p6 = ggplot()+geom_sf(data = r_grid_plotting,aes(col = plotting))+
    scale_color_gradientn(list_plots[[i]][[6]],colours = terrain.colors(100),limits = c(1,max(na.omit(r_grid_plotting$plotting))),na.value = 'grey90')
  r_grid_plotting$plotting = r_grid_plotting[[list_plots[[i]][[7]]]]
  
  p7 = ggplot()+geom_sf(data = r_grid_plotting,aes(col = plotting))+
    scale_color_gradientn(list_plots[[i]][[7]],colours = terrain.colors(100),limits = c(1,max(na.omit(r_grid_plotting$plotting))),na.value = 'grey90')
  r_grid_plotting$plotting = r_grid_plotting[[list_plots[[i]][[8]]]]
  
  p8 = ggplot()+geom_sf(data = r_grid_plotting,aes(col = plotting))+
    scale_color_gradientn(list_plots[[i]][[8]],colours = terrain.colors(100),limits = c(1,max(na.omit(r_grid_plotting$plotting))),na.value = 'grey90')
  
  p_all = arrangeGrob(p1,p2,p3,p4,p5,p6,p7,p8,ncol=2)
  
  ggsave(filename = paste0(list_groups[i],'p_all_med','.pdf'),width = 10,height = 8,plot = p_all)
  
}

#>50%
col_pal = colorRampPalette(c('steelblue','khaki','indianred'))
for(i in seq_along(list_plots)){
  print(i)
  r_grid_plotting$plotting = r_grid_plotting[[list_plots[[i]][[1]]]]
  r_grid_plotting$plotting = r_grid_plotting$plotting/max(na.omit(r_grid_plotting$plotting))
  #min_value = min(na.omit(r_grid_plotting[r_grid_plotting$plotting>0,]$plotting))
  p1 = ggplot()+
    geom_sf(data = r_grid2[r_grid2$land_areas>0,],col = 'grey90',fill = 'grey90')+
    geom_sf(data = r_grid_plotting,aes(col = plotting,fill = plotting))+
    scale_color_gradientn(list_plots[[i]][[1]],colours = col_pal(100),limits = c(0.5,max(na.omit(r_grid_plotting$plotting))),na.value = 'transparent')+
    scale_fill_gradientn(list_plots[[i]][[1]],colours = col_pal(100),limits = c(0.5,max(na.omit(r_grid_plotting$plotting))),na.value = 'transparent')+
    theme_void()
  
  r_grid_plotting$plotting = r_grid_plotting[[list_plots[[i]][[2]]]]
  r_grid_plotting$plotting = r_grid_plotting$plotting/max(na.omit(r_grid_plotting$plotting))
  
  p2 = ggplot()+
    geom_sf(data = r_grid2[r_grid2$land_areas>0,],col = 'grey90',fill = 'grey90')+
    geom_sf(data = r_grid_plotting,aes(col = plotting,fill = plotting))+
    scale_color_gradientn(list_plots[[i]][[2]],colours = col_pal(100),limits = c(0.5,max(na.omit(r_grid_plotting$plotting))),na.value = 'transparent')+
    scale_fill_gradientn(list_plots[[i]][[2]],colours = col_pal(100),limits = c(0.5,max(na.omit(r_grid_plotting$plotting))),na.value = 'transparent')+
    theme_void()
  
  r_grid_plotting$plotting = r_grid_plotting[[list_plots[[i]][[3]]]]
  r_grid_plotting$plotting = r_grid_plotting$plotting/max(na.omit(r_grid_plotting$plotting))
  
  p3 = ggplot()+
    geom_sf(data = r_grid2[r_grid2$land_areas>0,],col = 'grey90',fill = 'grey90')+
    geom_sf(data = r_grid_plotting,aes(col = plotting,fill = plotting))+
    scale_color_gradientn(list_plots[[i]][[3]],colours = col_pal(100),limits = c(0.5,max(na.omit(r_grid_plotting$plotting))),na.value = 'transparent')+
    scale_fill_gradientn(list_plots[[i]][[3]],colours = col_pal(100),limits = c(0.5,max(na.omit(r_grid_plotting$plotting))),na.value = 'transparent')+
    theme_void()
  
  r_grid_plotting$plotting = r_grid_plotting[[list_plots[[i]][[4]]]]
  r_grid_plotting$plotting = r_grid_plotting$plotting/max(na.omit(r_grid_plotting$plotting))
  
  p4 = ggplot()+
    geom_sf(data = r_grid2[r_grid2$land_areas>0,],col = 'grey90',fill = 'grey90')+
    geom_sf(data = r_grid_plotting,aes(col = plotting,fill = plotting))+
    scale_color_gradientn(list_plots[[i]][[4]],colours = col_pal(100),limits = c(0.5,max(na.omit(r_grid_plotting$plotting))),na.value = 'transparent')+
    scale_fill_gradientn(list_plots[[i]][[4]],colours = col_pal(100),limits = c(0.5,max(na.omit(r_grid_plotting$plotting))),na.value = 'transparent')+
    theme_void()
  
  r_grid_plotting$plotting = r_grid_plotting[[list_plots[[i]][[5]]]]
  r_grid_plotting$plotting = r_grid_plotting$plotting/max(na.omit(r_grid_plotting$plotting))
  
  p5 = ggplot()+
    geom_sf(data = r_grid2[r_grid2$land_areas>0,],col = 'grey90',fill = 'grey90')+
    geom_sf(data = r_grid_plotting,aes(col = plotting,fill = plotting))+
    scale_color_gradientn(list_plots[[i]][[5]],colours = col_pal(100),limits = c(0.5,max(na.omit(r_grid_plotting$plotting))),na.value = 'transparent')+
    scale_fill_gradientn(list_plots[[i]][[5]],colours = col_pal(100),limits = c(0.5,max(na.omit(r_grid_plotting$plotting))),na.value = 'transparent')+
    theme_void()
  
  r_grid_plotting$plotting = r_grid_plotting[[list_plots[[i]][[6]]]]
  r_grid_plotting$plotting = r_grid_plotting$plotting/max(na.omit(r_grid_plotting$plotting))
  
  p6 = ggplot()+
    geom_sf(data = r_grid2[r_grid2$land_areas>0,],col = 'grey90',fill = 'grey90')+
    geom_sf(data = r_grid_plotting,aes(col = plotting,fill = plotting))+
    scale_color_gradientn(list_plots[[i]][[6]],colours = col_pal(100),limits = c(0.5,max(na.omit(r_grid_plotting$plotting))),na.value = 'transparent')+
    scale_fill_gradientn(list_plots[[i]][[6]],colours = col_pal(100),limits = c(0.5,max(na.omit(r_grid_plotting$plotting))),na.value = 'transparent')+
    theme_void()
  
  r_grid_plotting$plotting = r_grid_plotting[[list_plots[[i]][[7]]]]
  r_grid_plotting$plotting = r_grid_plotting$plotting/max(na.omit(r_grid_plotting$plotting))
  
  p7 = ggplot()+
    geom_sf(data = r_grid2[r_grid2$land_areas>0,],col = 'grey90',fill = 'grey90')+
    geom_sf(data = r_grid_plotting,aes(col = plotting,fill = plotting))+
    scale_color_gradientn(list_plots[[i]][[7]],colours = col_pal(100),limits = c(0.5,max(na.omit(r_grid_plotting$plotting))),na.value = 'transparent')+
    scale_fill_gradientn(list_plots[[i]][[7]],colours = col_pal(100),limits = c(0.5,max(na.omit(r_grid_plotting$plotting))),na.value = 'transparent')+
    theme_void()
  
  r_grid_plotting$plotting = r_grid_plotting[[list_plots[[i]][[8]]]]
  r_grid_plotting$plotting = r_grid_plotting$plotting/max(na.omit(r_grid_plotting$plotting))
  
  p8 = ggplot()+
    geom_sf(data = r_grid2[r_grid2$land_areas>0,],col = 'grey90',fill = 'grey90')+
    geom_sf(data = r_grid_plotting,aes(col = plotting,fill = plotting))+
    scale_color_gradientn(list_plots[[i]][[8]],colours = col_pal(100),limits = c(0.5,max(na.omit(r_grid_plotting$plotting))),na.value = 'transparent')+
    scale_fill_gradientn(list_plots[[i]][[8]],colours = col_pal(100),limits = c(0.5,max(na.omit(r_grid_plotting$plotting))),na.value = 'transparent')+
    theme_void()
  
  p_all = arrangeGrob(p1,p2,p3,p4,p5,p6,p7,p8,ncol=2)
  
  ggsave(filename = paste0(list_groups[i],'p_all_med_over_05','.png'),width = 10,height = 8,plot = p_all)
  
}
