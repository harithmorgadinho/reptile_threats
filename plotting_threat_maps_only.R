library(sf)
sf::sf_use_s2(FALSE)
library(raster)
library(fasterize)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)
library(gridExtra)
library(rmapshaper)

sf_land = st_read('/Users/gdt366/Dropbox/african_snakes/land-polygons-complete-4326/land_polygons.shp')
sf_land = st_transform(sf_land,  
                       crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0")
sf_land$fake = 1

load('reptiles_grid_probability_final_with_threatened_urbanization_any_threat.Rdata')


sf_land_intersected = st_intersects(sf_land,r_grid2)
land_areas = unlist(lapply(t(sf_land_intersected), FUN = length))
r_grid2$land_areas = land_areas

library(ggplot2)


test1 = ggplot()+
  geom_sf(data = r_grid2, aes(fill = hunting),col = NA)+
  scale_fill_gradientn(colours = terrain.colors(10),na.value = 'black')




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
  "Urbanization" = brewer.pal(8,"RdPu"))


legend.title.size = 10
annotate.size = 3
size_legend_text = 7

r_grid2[r_grid2$richness<10,]$hunting = NA
r_grid2[r_grid2$richness<10,]$agriculture = NA
r_grid2[r_grid2$richness<10,]$logging = NA
r_grid2[r_grid2$richness<10,]$pollution = NA
r_grid2[r_grid2$richness<10,]$invasives = NA
r_grid2[r_grid2$richness<10,]$climate_change = NA
r_grid2[r_grid2$richness<10,]$urbanization = NA
r_grid2[r_grid2$richness<10,]$any_threat = NA

colnames(r_grid2)



library(gridExtra)
library(png)
library(grid)

img <- readPNG('cat.png', native = TRUE)
cat_img <- rasterGrob(img, interpolate=TRUE)

img <- readPNG('download.png', native = TRUE)
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


theme_harith = theme(legend.title = element_text(angle = -90,hjust = 0.5,size = legend.title.size),
                     legend.key.width = unit(0.3, 'cm'),
                     legend.key.height = unit(0.75, 'cm'),
                     panel.background = element_blank(),
                     axis.title = element_blank(),
                     legend.text = element_text(size = size_legend_text),
                     plot.tag = element_text(face='bold'),
                     legend.background = element_blank(),
                     plot.background=element_rect(fill="transparent",colour=NA),
                     legend.key = element_rect(fill = "transparent", colour = "transparent"),
                     panel.border = element_rect(colour=NA,fill = NA),
                     plot.tag.position = c(0.02,0.95),
                     plot.margin = unit(x = c(0, 0, 0, 0), units = "mm"),
                     legend.margin=margin(c(0,0,0,0), unit='cm'))

theme_harith = theme(legend.title = element_blank(),
                     legend.key.width = unit(0.3, 'cm'),
                     legend.key.height = unit(0.3, 'cm'),
                     panel.background = element_blank(),
                     axis.title = element_blank(),
                     legend.text = element_text(size = size_legend_text),
                     plot.tag = element_text(face='bold'),
                     legend.background = element_blank(),
                     plot.background=element_rect(fill="transparent",colour=NA),
                     legend.key = element_rect(fill = "transparent", colour = "transparent"),
                     panel.border = element_rect(colour=NA,fill = NA),
                     plot.tag.position = c(0.02,0.95),
                     plot.margin = unit(x = c(0, 0, 0, 0), units = "mm"),
                     legend.margin=margin(c(0,0,0,0), unit='cm'),
                     legend.position = c(0.9,0.8))

p1 = ggplot() +
  geom_sf(data = r_grid2[r_grid2$land_areas == 1,],linewidth = 0.0001,bg = 'grey70',col =  'grey70') +
  geom_sf(data = r_grid2,aes(fill = hunting,col = hunting),linewidth = 0.0001) +
  scale_fill_gradientn('Impact probability',colours = color.schemes$Hunting,na.value = NA,limits = c(0,1))+
  scale_colour_gradientn('Impact probability',colours = color.schemes$Hunting,na.value = NA,limits = c(0,1))+
  #theme_void()+
  annotate('text',x = 16382470,y = -7053445,label = 'Hunting',size = annotate.size)+
  theme_harith+
  guides(fill = guide_colorbar(title.position = "right"),
         plot.tag.position = c(0.1,0.9))+
  labs(tag = 'e')+
  annotation_custom(hunting_img, xmin=-17182470, xmax=-2000000, ymin=-8853445, ymax= -3700000)




p2 = ggplot() +
  geom_sf(data = r_grid2[r_grid2$land_areas == 1,],linewidth = 0.0001,bg = 'grey70',col = 'grey70') +
  geom_sf(data = r_grid2,aes(fill = agriculture,col=agriculture),linewidth = 0.0001) +
  scale_fill_gradientn('Impact probability',colours = color.schemes$Agriculture,na.value = NA,limits = c(0,1))+
  scale_colour_gradientn('Impact probability',colours = color.schemes$Agriculture,na.value = NA,limits = c(0,1))+
  #theme_void()+
  annotate('text',x = 16382470,y = -7053445,label = 'Agriculture',size = annotate.size)+
  theme_harith+
  guides(fill = guide_colorbar(title.position = "right"))+
  labs(tag = 'c')+
  annotation_custom(agriculture_img, xmin=-17182470, xmax=-3000000, ymin=-8853445, ymax= -3700000)

p3 = ggplot() +
  geom_sf(data = r_grid2[r_grid2$land_areas == 1,],linewidth = 0.0001,bg = 'grey70',col = 'grey70') +
  geom_sf(data = r_grid2,aes(fill = logging,col = logging),linewidth = 0.0001) +
  scale_fill_gradientn('Impact probability',colours = color.schemes$Logging,na.value = NA,limits = c(0,1))+
  scale_colour_gradientn('Impact probability',colours = color.schemes$Logging,na.value = NA,limits = c(0,1))+
  #theme_void()+
  annotate('text',x = 16382470,y = -7053445,label = 'Logging',size = annotate.size)+
  theme_harith+
  guides(fill = guide_colorbar(title.position = "right"))+
  labs(tag = 'a')+
  annotation_custom(saw_img, xmin=-17182470, xmax=-3000000, ymin=-8853445, ymax= -3700000)


p4 = ggplot() +
  geom_sf(data = r_grid2[r_grid2$land_areas == 1,],linewidth = 0.0001,bg = 'grey70',col = 'grey70') +
  geom_sf(data = r_grid2,aes(fill = pollution,col = pollution),linewidth = 0.0001) +
  scale_fill_gradientn('Impact probability',colours = color.schemes$Pollution,na.value = NA,limits = c(0,1))+
  scale_colour_gradientn('Impact probability',colours = color.schemes$Pollution,na.value = NA,limits = c(0,1))+
  annotate('text',x = 16382470,y = -7053445,label = 'Pollution',size = annotate.size)+
  theme_harith+
  guides(fill = guide_colorbar(title.position = "right"))+
  labs(tag = 'b')+
  annotation_custom(factory_img, xmin=-16782470, xmax=-3000000, ymin=-7753445, ymax= -3700000)

p5 = ggplot() +
  geom_sf(data = r_grid2[r_grid2$land_areas == 1,],linewidth = 0.0001,bg = 'grey70',col = 'grey70') +
  geom_sf(data = r_grid2,aes(fill = invasives,col = invasives),linewidth = 0.0001) +
  scale_fill_gradientn('Impact probability',colours = color.schemes$Invasives,na.value = NA,limits = c(0,1))+
  scale_colour_gradientn('Impact probability',colours = color.schemes$Invasives,na.value = NA,limits = c(0,1))+
  #theme_void()+
  annotate('text',x = 16382470,y = -7053445,label = 'Invasives',size = annotate.size)+
  theme_harith+
  labs(tag = 'd')+
  guides(fill = guide_colorbar(title.position = "right"))+
  annotation_custom(cat_img, xmin=-17182470, xmax=-3000000, ymin=-8853445, ymax= -3700000)

p6 = ggplot() +
  geom_sf(data = r_grid2[r_grid2$land_areas == 1,],linewidth = 0.0001,bg = 'grey70',col = 'grey70') +
  geom_sf(data = r_grid2,aes(fill = climate_change,col = climate_change),linewidth = 0.0001) +
  scale_fill_gradientn('Impact probability',colours = color.schemes$`Climate change`,na.value = NA,limits = c(0,1))+
  scale_colour_gradientn('Impact probability',colours = color.schemes$`Climate change`,na.value = NA,limits = c(0,1))+
  annotate('text',x = 15082470,y = -7053445,label = 'Climate Change',size = annotate.size)+
  theme_harith+
  guides(fill = guide_colorbar(title.position = "right"))+
  labs(tag = 'f')+
  annotation_custom(climate_c_img, xmin=-17182470, xmax=-3000000, ymin=-8853445, ymax= -3700000)

p7 = ggplot() +
  geom_sf(data = r_grid2[r_grid2$land_areas == 1,],linewidth = 0.0001,bg = 'grey70',col = 'grey70') +
  geom_sf(data = r_grid2,aes(fill = urbanization,col = urbanization),linewidth = 0.0001) +
  scale_fill_gradientn('Impact probability',colours = color.schemes$Urbanization,na.value = NA,limits = c(0,1))+
  scale_colour_gradientn('Impact probability',colours = color.schemes$Urbanization,na.value = NA,limits = c(0,1))+
  annotate('text',x = 15682470,y = -7053445,label = 'Urbanization',size = annotate.size)+
  theme_harith+
  guides(fill = guide_colorbar(title.position = "right"))+
  labs(tag = 'g')+
  annotation_custom(urbanization_img, xmin=-17182470, xmax=-3000000, ymin=-7753445, ymax= -3700000)


r_grid2_df = r_grid2
st_geometry(r_grid2_df) = NULL



highest_threat = function(input){
  
  if(is.infinite(max(na.omit(t(input[,3:9]))))){
    threat = NA
  }else{
    threat = names(which(t(input[,3:9])[,1] == max(na.omit(input[,3:9]))))
    if(length(threat)>1){
      threat = 'Many'
    }
  }
  return(threat)
}

list_higher_threats = list()
for(i in 1:nrow(r_grid2_df)){
  print(i)
  list_higher_threats[[i]] = highest_threat(r_grid2_df[i,])
}

unique(unlist(list_higher_threats))
r_grid2_df$higher_threat = unlist(list_higher_threats)

r_grid2$higher_threat = r_grid2_df$higher_threat

#r_grid2$higher_threat = factor(r_grid2$higher_threat,levels = c('hunting','agriculture','logging','pollution','invasives','climate_change','urbanization','Many'))

threats_pal = c(color.schemes$Hunting[6],
                color.schemes$Agriculture[3],
                color.schemes$Logging[5],
                color.schemes$Pollution[5],
                color.schemes$Invasives[5],
                color.schemes$`Climate change`[5],
                color.schemes$Urbanization[7],'black')
unique(r_grid2$higher_threat)

#labels_threats = c('Hunting','Agriculture','Logging','Pollution','Invasives','Climate change','Urbanization','Numerous')
unique(r_grid2$higher_threat)
r_grid2$higher_threat = factor(r_grid2$higher_threat,levels = c('logging','agriculture','hunting','urbanization',
                                                                'pollution','invasives',
                                                                'climate_change','Many'))
threats_pal = c(color.schemes$Logging[5],
                color.schemes$Agriculture[3],
                color.schemes$Hunting[6],
                color.schemes$Urbanization[7],
                color.schemes$Pollution[5],
                color.schemes$Invasives[5],
                color.schemes$`Climate change`[5],
                'black')

p8 = ggplot() +
  geom_sf(data = r_grid2[r_grid2$land_areas == 1,],linewidth = 0.0001,bg = 'grey70',col = 'grey70') +
  geom_sf(data = r_grid2,aes(fill = higher_threat, col = higher_threat),linewidth = 0.0001) +
  scale_fill_manual('',values = threats_pal,na.value = NA,na.translate = F)+
  scale_colour_manual('',values = threats_pal,na.value = NA,na.translate = F)+
  annotate('text',x = 15682470,y = -7053445,label = 'Highest threat',size = annotate.size)+
  theme_harith+
  guides(fill = guide_legend(title.position = "right",nrow = 4))+
  theme(
    #legend.title = element_text(angle = -90,hjust = 0.5,size = legend.title.size),
  legend.key.width = unit(0.35, 'cm'),
  legend.key.height = unit(0.35, 'cm'),
  legend.text = element_blank(),
  legend.spacing.x = unit(0.1, 'cm')
  #legend.text = element_blank()
  #       panel.background = element_blank(),
  #       axis.title = element_blank(),
  #       legend.text = element_text(size = size_legend_text),
  #       plot.tag = element_text(face='bold'),
  #       plot.background=element_rect(fill="transparent",colour=NA),
  #       legend.key = element_rect(fill = "transparent", colour = "transparent"),
  #       legend.position = 'none'
    )+
  labs(tag = 'h')
  # annotation_custom(saw_img, xmin=-15182470, xmax=-8182470, ymin=-3753445, ymax= -1700000)+
  # annotation_custom(factory_img, xmin=-11182470, xmax=-8182470, ymin=-3753445, ymax= -1700000)+
  # annotation_custom(agriculture_img, xmin=-15182470, xmax=-8182470, ymin=-4253445, ymax= -1200000)
  # annotation_custom(cat_img, xmin=-17182470, xmax=-10182470, ymin=-7753445, ymax= -1700000)+
  # annotation_custom(hunting_img, xmin=-17182470, xmax=-10182470, ymin=-7753445, ymax= -1700000)+
  # annotation_custom(climate_c_img, xmin=-17182470, xmax=-10182470, ymin=-7753445, ymax= -1700000)+
  # annotation_custom(urbanization_img, xmin=-17182470, xmax=-10182470, ymin=-7753445, ymax= -1700000)


#ggsave(file = 'p8_test.png',plot = p8,width = 2.5,height = 4.5,dpi = 500)



library(gridExtra)

fig1_grid = arrangeGrob(p3,p4,p2,p5,p1,p6,p7,p8,ncol=2)

ggsave(file = 'figure1_Sep_27v2.png',plot = fig1_grid,width = 9,height = 9,dpi = 2000)


## multiplied by richness

p1 = ggplot() +
  geom_sf(data = r_grid2[r_grid2$land_areas == 1,],linewidth = 0.0001,bg = 'grey70',col = 'grey70') +
  geom_sf(data = r_grid2,aes(fill = hunting*richness,col = hunting*richness),linewidth = 0.0001) +
  scale_fill_gradientn('Expected number',colours = color.schemes$Hunting,na.value = NA)+
  scale_colour_gradientn('Expected number',colours = color.schemes$Hunting,na.value = NA)+
  #theme_void()+
  annotate('text',x = 16382470,y = -7053445,label = 'Hunting',size = annotate.size)+
  theme_harith+
  # theme(legend.title = element_text(angle = -90,hjust = 0.5,size = legend.title.size),
  #       legend.key.width = unit(0.5, 'cm'),
  #       legend.key.height = unit(0.75, 'cm'),
  #       panel.background = element_blank(),
  #       axis.title = element_blank(),
  #       legend.text = element_text(size = size_legend_text),
  #       plot.tag = element_text(face='bold'),
  #       legend.background = element_blank(),
  #       plot.background=element_rect(fill="transparent",colour=NA),
  #       legend.key = element_rect(fill = "transparent", colour = "transparent"))+
  guides(fill = guide_colorbar(title.position = "right"))+
  labs(tag = 'e')+
  annotation_custom(hunting_img, xmin=-17182470, xmax=-2000000, ymin=-8853445, ymax= -3700000)




p2 = ggplot() +
  geom_sf(data = r_grid2[r_grid2$land_areas == 1,],linewidth = 0.0001,bg = 'grey70',col = 'grey70') +
  geom_sf(data = r_grid2,aes(fill = agriculture*richness,col=agriculture*richness),linewidth = 0.0001) +
  scale_fill_gradientn('Expected number',colours = color.schemes$Agriculture,na.value = NA)+
  scale_colour_gradientn('Expected number',colours = color.schemes$Agriculture,na.value = NA)+
  #theme_void()+
  annotate('text',x = 16382470,y = -7053445,label = 'Agriculture',size = annotate.size)+
  theme_harith+
  # theme(legend.title = element_text(angle = -90,hjust = 0.5,size = legend.title.size),
  #       legend.key.width = unit(0.5, 'cm'),
  #       legend.key.height = unit(0.75, 'cm'),
  #       panel.background = element_blank(),
  #       axis.title = element_blank(),
  #       legend.text = element_text(size = size_legend_text),
  #       plot.tag = element_text(face='bold'),
  #       legend.background = element_blank(),
  #       plot.background=element_rect(fill="transparent",colour=NA),
  #       legend.key = element_rect(fill = "transparent", colour = "transparent"))+
  guides(fill = guide_colorbar(title.position = "right"))+
  labs(tag = 'c')+
  annotation_custom(agriculture_img, xmin=-17182470, xmax=-3000000, ymin=-8853445, ymax= -3700000)

p3 = ggplot() +
  geom_sf(data = r_grid2[r_grid2$land_areas == 1,],linewidth = 0.0001,bg = 'grey70',col = 'grey70') +
  geom_sf(data = r_grid2,aes(fill = logging*richness,col = logging*richness),linewidth = 0.0001) +
  scale_fill_gradientn('Expected number',colours = color.schemes$Logging,na.value = NA)+
  scale_colour_gradientn('Expected number',colours = color.schemes$Logging,na.value = NA)+
  #theme_void()+
  annotate('text',x = 16382470,y = -7053445,label = 'Logging',size = annotate.size)+
  theme_harith+
  # theme(legend.title = element_text(angle = -90,hjust = 0.5,size = legend.title.size),
  #       legend.key.width = unit(0.5, 'cm'),
  #       legend.key.height = unit(0.75, 'cm'),
  #       panel.background = element_blank(),
  #       axis.title = element_blank(),
  #       legend.text = element_text(size = size_legend_text),
  #       plot.tag = element_text(face='bold'),
  #       legend.background = element_blank(),
  #       plot.background=element_rect(fill="transparent",colour=NA),
  #       legend.key = element_rect(fill = "transparent", colour = "transparent"))+
  guides(fill = guide_colorbar(title.position = "right"))+
  labs(tag = 'a')+
  annotation_custom(saw_img, xmin=-17182470, xmax=-3000000, ymin=-8853445, ymax= -3700000)


p4 = ggplot() +
  geom_sf(data = r_grid2[r_grid2$land_areas == 1,],linewidth = 0.0001,bg = 'grey70',col = 'grey70') +
  geom_sf(data = r_grid2,aes(fill = pollution*richness,col = pollution*richness),linewidth = 0.0001) +
  scale_fill_gradientn('Expected number',colours = color.schemes$Pollution,na.value = NA)+
  scale_colour_gradientn('Expected number',colours = color.schemes$Pollution,na.value = NA)+
  annotate('text',x = 16382470,y = -7053445,label = 'Pollution',size = annotate.size)+
  theme_harith+
  # theme(legend.title = element_text(angle = -90,hjust = 0.5,size = legend.title.size),
  #       legend.key.width = unit(0.5, 'cm'),
  #       legend.key.height = unit(0.75, 'cm'),
  #       panel.background = element_blank(),
  #       axis.title = element_blank(),
  #       legend.text = element_text(size = size_legend_text),
  #       plot.tag = element_text(face='bold'),
  #       legend.background = element_blank(),
  #       plot.background=element_rect(fill="transparent",colour=NA),
  #       legend.key = element_rect(fill = "transparent", colour = "transparent"))+
  guides(fill = guide_colorbar(title.position = "right"))+
  labs(tag = 'b')+
  annotation_custom(factory_img, xmin=-16782470, xmax=-3000000, ymin=-7753445, ymax= -3700000)

p5 = ggplot() +
  geom_sf(data = r_grid2[r_grid2$land_areas == 1,],linewidth = 0.0001,bg = 'grey70',col = 'grey70') +
  geom_sf(data = r_grid2,aes(fill = invasives*richness,col = invasives*richness),linewidth = 0.0001) +
  scale_fill_gradientn('Expected number',colours = color.schemes$Invasives,na.value = NA)+
  scale_colour_gradientn('Expected number',colours = color.schemes$Invasives,na.value = NA)+
  #theme_void()+
  annotate('text',x = 16382470,y = -7053445,label = 'Invasives',size = annotate.size)+
  theme_harith+
  # theme(legend.title = element_text(angle = -90,hjust = 0.5,size = legend.title.size),
  #       legend.key.width = unit(0.5, 'cm'),
  #       legend.key.height = unit(0.75, 'cm'),
  #       panel.background = element_blank(),
  #       axis.title = element_blank(),
  #       legend.text = element_text(size = size_legend_text),
  #       plot.tag = element_text(face='bold'),
  #       legend.background = element_blank(),
  #       plot.background=element_rect(fill="transparent",colour=NA),
  #       legend.key = element_rect(fill = "transparent", colour = "transparent"))+
  guides(fill = guide_colorbar(title.position = "right"))+
  labs(tag = 'd')+
  annotation_custom(cat_img, xmin=-17182470, xmax=-3000000, ymin=-8853445, ymax= -3700000)

p6 = ggplot() +
  geom_sf(data = r_grid2[r_grid2$land_areas == 1,],linewidth = 0.0001,bg = 'grey70',col = 'grey70') +
  geom_sf(data = r_grid2,aes(fill = climate_change*richness,col = climate_change*richness),linewidth = 0.0001) +
  scale_fill_gradientn('Expected number',colours = color.schemes$`Climate change`,na.value = NA)+
  scale_colour_gradientn('Expected number',colours = color.schemes$`Climate change`,na.value = NA)+
  annotate('text',x = 15082470,y = -7053445,label = 'Climate Change',size = annotate.size)+
  theme_harith+
  # theme(legend.title = element_text(angle = -90,hjust = 0.5,size = legend.title.size),
  #       legend.key.width = unit(0.5, 'cm'),
  #       legend.key.height = unit(0.75, 'cm'),
  #       panel.background = element_blank(),
  #       axis.title = element_blank(),
  #       legend.text = element_text(size = size_legend_text),
  #       legend.background = element_rect(fill = "transparent",colour = "transparent"),
  #       plot.tag = element_text(face='bold'),
  #       plot.background=element_rect(fill="transparent",colour=NA),
  #       legend.key = element_rect(fill = "transparent", colour = "transparent"))+
  guides(fill = guide_colorbar(title.position = "right"))+
  labs(tag = 'f')+
  annotation_custom(climate_c_img, xmin=-17182470, xmax=-3000000, ymin=-8853445, ymax= -3700000)

p7 = ggplot() +
  geom_sf(data = r_grid2[r_grid2$land_areas == 1,],linewidth = 0.0001,bg = 'grey70',col = 'grey70') +
  geom_sf(data = r_grid2,aes(fill = urbanization*richness,col = urbanization*richness),linewidth = 0.0001) +
  scale_fill_gradientn('Expected number',colours = color.schemes$Urbanization,na.value = NA)+
  scale_colour_gradientn('Expected number',colours = color.schemes$Urbanization,na.value = NA)+
  annotate('text',x = 15682470,y = -7053445,label = 'Urbanization',size = annotate.size)+
  # theme(legend.title = element_text(angle = -90,hjust = 0.5,size = legend.title.size),
  #       legend.key.width = unit(0.5, 'cm'),
  #       legend.key.height = unit(0.75, 'cm'),
  #       panel.background = element_blank(),
  #       axis.title = element_blank(),
  #       legend.text = element_text(size = size_legend_text),
  #       plot.tag = element_text(face='bold'),
  #       plot.background=element_rect(fill="transparent",colour=NA),
  #       legend.key = element_rect(fill = "transparent", colour = "transparent"))+
  theme_harith+
  guides(fill = guide_colorbar(title.position = "right"))+
  labs(tag = 'g')+
  annotation_custom(urbanization_img, xmin=-17182470, xmax=-3000000, ymin=-7753445, ymax= -3700000)

library(RColorBrewer)
display.brewer.all()


pal_richness = colorRampPalette(rev(brewer.pal(11,'Spectral')))
p8 = ggplot() +
  geom_sf(data = r_grid2[r_grid2$land_areas == 1,],linewidth = 0.0001,bg = 'grey70',col = 'grey70') +
  geom_sf(data = r_grid2[r_grid2$land_areas == 1,],aes(fill = richness,col = richness),linewidth = 0.0001) +
  scale_fill_gradientn('Richness',colours = pal_richness(100),na.value = NA,limits = c(1,max(na.omit(r_grid2$richness))))+
  scale_colour_gradientn('Richness',colours = pal_richness(100),na.value = NA,limits = c(1,max(na.omit(r_grid2$richness))))+
  # theme(legend.title = element_text(angle = -90,hjust = 0.5,size = legend.title.size),
  #       legend.key.width = unit(0.5, 'cm'),
  #       legend.key.height = unit(0.75, 'cm'),
  #       panel.background = element_blank(),
  #       axis.title = element_blank(),
  #       legend.text = element_text(size = size_legend_text),
  #       plot.tag = element_text(face='bold'),
  #       plot.background=element_rect(fill="transparent",colour=NA),
  #       legend.key = element_rect(fill = "transparent", colour = "transparent"))+
  theme_harith+
  guides(fill = guide_colorbar(title.position = "right"))+
  labs(tag = 'h')





library(gridExtra)

figSM1_grid = arrangeGrob(p3,p4,p2,p5,p1,p6,p7,p8,ncol=2)

ggsave(file = 'figureSM1_Sep_26.png',plot = figSM1_grid,width = 9,height = 9,dpi = 2000)




