worldMap <- rnaturalearth::ne_countries(scale = 10, type = "countries", returnclass = 'sf')
worldMap_behrmann = st_transform(worldMap,  
                                 crs = '+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0')
r = raster(st_zm(worldMap_behrmann), resolution = 50000 ,
           crs = '+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0')
sort(unique(worldMap_behrmann$continent))
worldMap_behrmann_no_ant = worldMap_behrmann[worldMap_behrmann$continent != 'Antarctica',]
worldMap_behrmann_no_ant$fake_value = 0
raster_world = fasterize(sf = st_collection_extract(worldMap_behrmann_no_ant, "POLYGON"),raster = r,
                         fun ='sum',
                         field = 'fake_value')

rgrid = st_as_sf(rasterToPolygons(raster_world))

shp1 = st_read('reptiles_13oct.shp')

shp1[shp1$richnss<10,]$richnss = 0

shp1$hunting_p = shp1$hunting/shp1$richnss
shp1$agrcltr_p = shp1$agrcltr/shp1$richnss
shp1$logging_p = shp1$logging/shp1$richnss
shp1$pollutn_p = shp1$pollutn/shp1$richnss
shp1$clmt_ch_p = shp1$clmt_ch/shp1$richnss
shp1$invasiv_p = shp1$invasiv/shp1$richnss

shp1$hunting_t = (shp1$hunting/shp1$richnss)*shp1$richnss
shp1$agrcltr_t = (shp1$agrcltr/shp1$richnss)*shp1$richnss
shp1$logging_t = (shp1$logging/shp1$richnss)*shp1$richnss
shp1$pollutn_t = (shp1$pollutn/shp1$richnss)*shp1$richnss
shp1$clmt_ch_t = (shp1$clmt_ch/shp1$richnss)*shp1$richnss
shp1$invasiv_t = (shp1$invasiv/shp1$richnss)*shp1$richnss


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


legend.title.size = 6
annotate.size = 2
size_legend_text = 5


p1 = ggplot() +
  geom_sf(data = rgrid,size = 0,bg = 'grey90',col = NA) +
  geom_sf(data = shp1,aes(fill = hunting_p),size = 0,col=NA) +
  scale_fill_gradientn('Impact probability',colours = color.schemes$Hunting,na.value = NA)+
  #theme_void()+
  annotate('text',x = 16382470,y = -7053445,label = 'Hunting',size = annotate.size)+
  theme(legend.title = element_text(angle = -90,hjust = 0.5,size = legend.title.size),
        legend.key.width = unit(0.5, 'cm'),
        legend.key.height = unit(0.75, 'cm'),
        panel.background = element_blank(),
        axis.title = element_blank(),
        legend.text = element_text(size = size_legend_text),
        plot.tag = element_text(face='bold'))+
  guides(fill = guide_colorbar(title.position = "right"))+
  labs(tag = 'a.')


p2 = ggplot() +
  geom_sf(data = rgrid,size = 0,bg = 'grey90',col = NA) +
  geom_sf(data = shp1,aes(fill = hunting_t),size = 0,col=NA) +
  scale_fill_gradientn('Number of affected species',colours = color.schemes$Hunting,na.value = NA)+
  #theme_void()+
  annotate('text',x = 16382470,y = -7053445,label = 'Hunting',size = annotate.size)+
  theme(legend.title = element_text(angle = -90,hjust = 0.5,size = legend.title.size),
        legend.key.width = unit(0.5, 'cm'),
        legend.key.height = unit(0.75, 'cm'),
        panel.background = element_blank(),
        axis.title = element_blank(),
        legend.text = element_text(size = size_legend_text),
        plot.tag = element_text(face='bold'))+
  guides(fill = guide_colorbar(title.position = "right"))+
  labs(tag = 'b.')

p3 = ggplot() +
  geom_sf(data = rgrid,size = 0,bg = 'grey90',col = NA) +
  geom_sf(data = shp1,aes(fill = agrcltr_p),size = 0,col=NA) +
  scale_fill_gradientn('Impact probability',colours = color.schemes$Agriculture,na.value = NA)+
  #theme_void()+
  annotate('text',x = 16382470,y = -7053445,label = 'Agriculture',size = annotate.size)+
  theme(legend.title = element_text(angle = -90,hjust = 0.5,size = legend.title.size),
        legend.key.width = unit(0.5, 'cm'),
        legend.key.height = unit(0.75, 'cm'),
        panel.background = element_blank(),
        axis.title = element_blank(),
        legend.text = element_text(size = size_legend_text),
        plot.tag = element_text(face='bold'))+
  guides(fill = guide_colorbar(title.position = "right"))+
  labs(tag = 'c.')


p4 = ggplot() +
  geom_sf(data = rgrid,size = 0,bg = 'grey90',col = NA) +
  geom_sf(data = shp1,aes(fill = agrcltr_t),size = 0,col=NA) +
  scale_fill_gradientn('Number of affected species',colours = color.schemes$Agriculture,na.value = NA)+
  #theme_void()+
  annotate('text',x = 16382470,y = -7053445,label = 'Agriculture',size = annotate.size)+
  theme(legend.title = element_text(angle = -90,hjust = 0.5,size = legend.title.size),
        legend.key.width = unit(0.5, 'cm'),
        legend.key.height = unit(0.75, 'cm'),
        panel.background = element_blank(),
        axis.title = element_blank(),
        legend.text = element_text(size = size_legend_text),
        plot.tag = element_text(face='bold'))+
  guides(fill = guide_colorbar(title.position = "right"))+
  labs(tag = 'd.')

p5 = ggplot() +
  geom_sf(data = rgrid,size = 0,bg = 'grey90',col = NA) +
  geom_sf(data = shp1,aes(fill = logging_p),size = 0,col=NA) +
  scale_fill_gradientn('Logging',colours = color.schemes$Logging,na.value = NA)+
  #theme_void()+
  annotate('text',x = 16382470,y = -7053445,label = 'Logging',size = annotate.size)+
  theme(legend.title = element_text(angle = -90,hjust = 0.5,size = legend.title.size),
        legend.key.width = unit(0.5, 'cm'),
        legend.key.height = unit(0.75, 'cm'),
        panel.background = element_blank(),
        axis.title = element_blank(),
        legend.text = element_text(size = size_legend_text),
        plot.tag = element_text(face='bold'))+
  guides(fill = guide_colorbar(title.position = "right"))+
  labs(tag = 'e.')


p6 = ggplot() +
  geom_sf(data = rgrid,size = 0,bg = 'grey90',col = NA) +
  geom_sf(data = shp1,aes(fill = logging_t),size = 0,col=NA) +
  scale_fill_gradientn('Number of affected species',colours = color.schemes$Logging,na.value = NA)+
  #theme_void()+
  annotate('text',x = 16382470,y = -7053445,label = 'Logging',size = annotate.size)+
  theme(legend.title = element_text(angle = -90,hjust = 0.5,size = legend.title.size),
        legend.key.width = unit(0.5, 'cm'),
        legend.key.height = unit(0.75, 'cm'),
        panel.background = element_blank(),
        axis.title = element_blank(),
        legend.text = element_text(size = size_legend_text),
        plot.tag = element_text(face='bold'))+
  guides(fill = guide_colorbar(title.position = "right"))+
  labs(tag = 'f.')


library(gridExtra)
pdf('figure1v3.pdf',width = 11,height = 7)
grid.arrange(p1,p2,p3,p4,p5,p6,ncol=2)
dev.off()








p1 = ggplot() +
  geom_sf(data = rgrid,size = 0,bg = 'grey90',col = NA) +
  geom_sf(data = shp1,aes(fill = pollutn_p),size = 0,col=NA) +
  scale_fill_gradientn('Impact probability',colours = color.schemes$Pollution,na.value = NA)+
  #theme_void()+
  annotate('text',x = 16382470,y = -7053445,label = 'Pollution',size = annotate.size)+
  theme(legend.title = element_text(angle = -90,hjust = 0.5,size = legend.title.size),
        legend.key.width = unit(0.5, 'cm'),
        legend.key.height = unit(0.75, 'cm'),
        panel.background = element_blank(),
        axis.title = element_blank(),
        legend.text = element_text(size = size_legend_text),
        plot.tag = element_text(face='bold'))+
  guides(fill = guide_colorbar(title.position = "right"))+
  labs(tag = 'a.')
  


p2 = ggplot() +
  geom_sf(data = rgrid,size = 0,bg = 'grey90',col = NA) +
  geom_sf(data = shp1,aes(fill = pollutn_t),size = 0,col=NA) +
  scale_fill_gradientn('Number of affected species',colours = color.schemes$Pollution,na.value = NA)+
  #theme_void()+
  annotate('text',x = 16382470,y = -7053445,label = 'Pollution',size = annotate.size)+
  theme(legend.title = element_text(angle = -90,hjust = 0.5,size = legend.title.size),
        legend.key.width = unit(0.5, 'cm'),
        legend.key.height = unit(0.75, 'cm'),
        panel.background = element_blank(),
        axis.title = element_blank(),
        legend.text = element_text(size = size_legend_text),
        plot.tag = element_text(face='bold'))+
  guides(fill = guide_colorbar(title.position = "right"))+
  labs(tag = 'b.')

p3 = ggplot() +
  geom_sf(data = rgrid,size = 0,bg = 'grey90',col = NA) +
  geom_sf(data = shp1,aes(fill = invasiv_p),size = 0,col=NA) +
  scale_fill_gradientn('Impact probability',colours = color.schemes$Invasives,na.value = NA)+
  #theme_void()+
  annotate('text',x = 16382470,y = -7053445,label = 'Invasives',size = annotate.size)+
  theme(legend.title = element_text(angle = -90,hjust = 0.5,size = legend.title.size),
        legend.key.width = unit(0.5, 'cm'),
        legend.key.height = unit(0.75, 'cm'),
        panel.background = element_blank(),
        axis.title = element_blank(),
        legend.text = element_text(size = size_legend_text),
        plot.tag = element_text(face='bold'))+
  guides(fill = guide_colorbar(title.position = "right"))+
  labs(tag = 'c.')


p4 = ggplot() +
  geom_sf(data = rgrid,size = 0,bg = 'grey90',col = NA) +
  geom_sf(data = shp1,aes(fill = invasiv_t),size = 0,col=NA) +
  scale_fill_gradientn('Number of affected species',colours = color.schemes$Invasives,na.value = NA)+
  #theme_void()+
  annotate('text',x = 16382470,y = -7053445,label = 'Invasives',size = annotate.size)+
  theme(legend.title = element_text(angle = -90,hjust = 0.5,size = legend.title.size),
        legend.key.width = unit(0.5, 'cm'),
        legend.key.height = unit(0.75, 'cm'),
        panel.background = element_blank(),
        axis.title = element_blank(),
        legend.text = element_text(size = size_legend_text),
        plot.tag = element_text(face='bold'))+
  guides(fill = guide_colorbar(title.position = "right"))+
  labs(tag = 'd.')


p5 = ggplot() +
  geom_sf(data = rgrid,size = 0,bg = 'grey90',col = NA) +
  geom_sf(data = shp1,aes(fill = clmt_ch_p),size = 0,col=NA) +
  scale_fill_gradientn('Impact probability',colours = color.schemes$`Climate change`,na.value = NA)+
  #theme_void()+
  annotate('text',x = 16382470,y = -7053445,label = 'Climate change',size = annotate.size)+
  theme(legend.title = element_text(angle = -90,hjust = 0.5,size = legend.title.size),
        legend.key.width = unit(0.5, 'cm'),
        legend.key.height = unit(0.75, 'cm'),
        panel.background = element_blank(),
        axis.title = element_blank(),
        legend.text = element_text(size = size_legend_text),
        plot.tag = element_text(face='bold'))+
  guides(fill = guide_colorbar(title.position = "right"))+
  labs(tag = 'e.')


p6 = ggplot() +
  geom_sf(data = rgrid,size = 0,bg = 'grey90',col = NA) +
  geom_sf(data = shp1,aes(fill = clmt_ch_t),size = 0,col=NA) +
  scale_fill_gradientn('Number of affected species',colours = color.schemes$`Climate change`,na.value = NA)+
  #theme_void()+
  annotate('text',x = 16382470,y = -7053445,label = 'Climate change',size = annotate.size)+
  theme(legend.title = element_text(angle = -90,hjust = 0.5,size = legend.title.size),
        legend.key.width = unit(0.5, 'cm'),
        legend.key.height = unit(0.75, 'cm'),
        panel.background = element_blank(),
        axis.title = element_blank(),
        legend.text = element_text(size = size_legend_text),
        plot.tag = element_text(face='bold'))+
  guides(fill = guide_colorbar(title.position = "right"))+
  labs(tag = 'f.')

library(gridExtra)
pdf('figure2v3.pdf',width = 11,height = 7)
grid.arrange(p1,p2,p3,p4,p5,p6,ncol=2)
dev.off()


