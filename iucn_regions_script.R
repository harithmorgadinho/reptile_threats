sf_world = st_read('/Users/gdt366/Dropbox/Copenhagen_postdoc/world-administrative-boundaries/world-administrative-boundaries.shp')
unique(sf_world$region)

sf_world[is.na(sf_world$region),]
object.size(sf_world)

library(rnaturalearth)
worldmap = ne_countries(returnclass = 'sf',scale = 10,type = 'countries')
worldmap_tiny = ne_countries(returnclass = 'sf',scale = 50,type = 'tiny_countries')

object.size(worldmap)

plot(worldmap$geometry)
plot(worldmap_tiny$geometry,add=T,col = 'blue')


head(worldmap)

sort(worldmap[worldmap$continent == 'Europe',]$sovereignt)
plot(worldmap[worldmap$continent == 'Europe',]$geometry)

table(worldmap[worldmap$continent == 'Europe',]$subregion)

plot(worldmap[worldmap$subregion == 'Eastern Europe',]$geometry)

worldmap[worldmap$subregion == 'Eastern Europe',]$sovereignt
table(worldmap$subregion)

#write.csv(sort(worldmap$sovereignt),'iucn_regions.csv')

iucn_regions = read.csv('iucn_regions_from_website.csv',sep = ';')

worldmap_merge = merge(worldmap,iucn_regions,by.x = 'sovereignt',by.y = 'Country',all.x = T)

setdiff(iucn_regions$Country,worldmap$sovereignt)

unique(worldmap_merge[is.na(worldmap_merge$iucn_region),]$sovereignt)

worldmap_merge[worldmap_merge$sovereignt == 'Somalia',]$iucn_region = 'Sub-Saharan Africa'
worldmap_merge[worldmap_merge$sovereignt == 'Bajo Nuevo Bank (Petrel Is.)',]$iucn_region = 'Caribbean Islands'
worldmap_merge[worldmap_merge$sovereignt == 'Northern Cyprus',]$iucn_region = 'West and Central Asia'
worldmap_merge[worldmap_merge$sovereignt == 'Serranilla Bank',]$iucn_region = 'Caribbean Islands'
worldmap_merge[worldmap_merge$sovereignt == 'Kosovo',]$iucn_region = 'Europe'
worldmap_merge[worldmap_merge$sovereignt == 'eSwatini',]$iucn_region = 'Sub-Saharan Africa'
worldmap_merge[worldmap_merge$sovereignt == 'Cyprus No Mans Area',]$iucn_region = 'West and Central Asia'
worldmap_merge[worldmap_merge$sovereignt == 'Scarborough Reef',]$iucn_region = 'East Asia'
worldmap_merge[worldmap_merge$sovereignt == 'Spratly Islands',]$iucn_region = 'East Asia'
worldmap_merge[worldmap_merge$sovereignt == 'Saint Kitts and Nevis',]$iucn_region = 'Caribbean Islands'
worldmap_merge[worldmap_merge$sovereignt == 'Kashmir',]$iucn_region = 'South and Southeast Asia'

unique(worldmap_merge[is.na(worldmap_merge$iucn_region),]$sovereignt)
library(ggplot2)
p = ggplot()+
  geom_sf(data = worldmap_merge[worldmap_merge$iucn_region == 'Europe',],aes(fill = iucn_region),col = NA)


pdf('test.pdf')
print(p)
dev.off()






worldmap_merge[worldmap_merge$iucn_region == 'Europe',]$region_un

worldmap_merge[worldmap_merge$iucn_region == 'Europe' & worldmap_merge$region_un == 'Oceania',]$iucn_region = 'Oceania'

worldmap_merge[worldmap_merge$iucn_region == 'Europe',]$region_wb

worldmap_merge[worldmap_merge$iucn_region == 'Europe' & worldmap_merge$region_wb == 'Sub-Saharan Africa',]$iucn_region = 'Sub-Saharan Africa'

worldmap_merge[worldmap_merge$iucn_region == 'Europe',]$region_wb

worldmap_merge[worldmap_merge$iucn_region == 'Europe' & worldmap_merge$region_wb == 'Latin America & Caribbean',]$iucn_region = 'Caribbean Islands'

worldmap_merge[worldmap_merge$iucn_region == 'Europe',]$region_wb

worldmap_merge[worldmap_merge$iucn_region == 'Europe' & worldmap_merge$region_wb == 'North America',]$iucn_region = 'North America'
worldmap_merge[worldmap_merge$iucn_region == 'Europe' & worldmap_merge$region_wb == 'Antarctica',]$iucn_region = 'Antarctica'

worldmap_merge[worldmap_merge$iucn_region == 'Europe' & worldmap_merge$region_un == 'Asia',]$iucn_region = 'West and Central Asia'


plot(worldmap_merge[worldmap_merge$adm0_a3_is == 'FRA',][3,]$geometry)



France1=worldmap_merge[worldmap_merge$adm0_a3_is == 'FRA',]
France1 = st_crop(France1,extent(-10.2342,55.8545,-1.37078,51.08754))
plot(France1$geometry)

France1$iucn_region

France_SA=worldmap_merge[worldmap_merge$adm0_a3_is == 'FRA',]
France_SA = st_crop(France_SA,extent(-100.2342,55.8545,-10.37078,30))
plot(France_SA$geometry)

France_SA$iucn_region = 'South America'


France_EA=worldmap_merge[worldmap_merge$adm0_a3_is == 'FRA',]
France_EA = st_crop(France_EA,extent(-0.2342,55.8545,-21.37078,01.08754))
plot(France_EA$geometry)

France_EA$iucn_region = 'Sub-Saharan Africa'


FRANCE_Car = worldmap_merge[worldmap_merge$adm0_a3_is == 'FRA',][4,]

FRANCE_Car$iucn_region
worldmap_merge = worldmap_merge[worldmap_merge$adm0_a3_is != 'FRA',]

worldmap_merge = rbind(worldmap_merge,France1,France_SA,France_EA,FRANCE_Car)


worldmap_merge = worldmap_merge[!is.na(worldmap_merge$geometry),]

unique(worldmap_merge$iucn_region)

worldmap_merge_final <- worldmap_merge %>% filter(!st_is_empty(.))

save(worldmap_merge_final,file = 'worldmap_merge_final.Rdata')

library(ggplot2)
load('worldmap_merge_final.Rdata')
library(sf)
sf_use_s2(F)

worldmap_merge_final_2 = worldmap_merge_final[worldmap_merge_final$iucn_region != 'Antarctica',]
worldmap_merge_final_2 = worldmap_merge_final_2[worldmap_merge_final_2$iucn_region != 'Antartic',]

iucn_regions_pal = c('lightblue','pink','#654321','#A5BE00','firebrick','dodgerblue','purple','gold','#588E29','orange','red','#AA9ABA')

iucn_regions_pal_df = cbind.data.frame(regions = unique(sort(worldmap_merge_final_2$iucn_region)), iucn_regions_pal = iucn_regions_pal)

worldmap_merge_final_2$iucn_region = factor(worldmap_merge_final_2$iucn_region, 
                                            levels = c('North America','Mesoamerica','Caribbean Islands','South America',
                                                       'Europe','North Africa','Sub-Saharan Africa', 'West and Central Asia', 
                                                       'North Asia','East Asia','South and Southeast Asia','Oceania'))


df_cols = cbind.data.frame(var1 = c('North America','Mesoamerica','Caribbean Islands','South America',
                          'Europe','North Africa','Sub-Saharan Africa','West and Central Asia',
                          'North Asia','East Asia','South and Southeast Asia','Oceania'), var2 = NA)

df_cols = merge(df_cols,iucn_regions_pal_df,by.x = 'var1',by.y = 'regions',sort = FALSE)
save(df_cols,file = 'df_cols.Rdata')


p = ggplot()+
  geom_sf(data = worldmap_merge_final_2,aes(fill = iucn_region),col = NA)+
  scale_fill_manual('',values = df_cols$iucn_regions_pal)+
  geom_point(aes(x=-7300000,y=2300000), size=8, shape=1, color="lightblue",stroke = 2)+
  theme_void()+
  theme(panel.grid.major = element_line(colour = 'grey90'),
        legend.position = c(0.5,0))+
  guides(fill=guide_legend(ncol=4))+
  scale_y_continuous(limits = c(-9478435,7750716))





pdf('test.pdf')
print(p)
dev.off()

p0_map = ggplot()+
  #geom_sf(data = worldmap_merge_final_2,fill = 'grey90',col = 'grey90')+
  geom_sf(data = worldmap_merge_final_2,aes(fill = iucn_region),col = NA)+
  scale_fill_manual('',values = df_cols$iucn_regions_pal)+
  theme_void()+
  theme(legend.position = 'none')+
  geom_point(aes(x=-7300000,y=2300000), size=4, shape=1, color="lightblue",stroke = 2)

p1_map = ggplot()+
  geom_sf(data = worldmap_merge_final_2,fill = 'grey90',col = 'grey90')+
  geom_sf(data = worldmap_merge_final_2[worldmap_merge_final_2$iucn_region == df_cols$var1[1],],fill = df_cols$iucn_regions_pal[1],col = NA)+
  #geom_point(aes(x=-7300000,y=2300000), size=8, shape=1, color="lightblue",stroke = 2)+
  theme_void()

p2_map = ggplot()+
  geom_sf(data = worldmap_merge_final_2,fill = 'grey90',col = 'grey90')+
  geom_sf(data = worldmap_merge_final_2[worldmap_merge_final_2$iucn_region == df_cols$var1[2],],fill = df_cols$iucn_regions_pal[2],col = NA)+
  #geom_point(aes(x=-7300000,y=2300000), size=8, shape=1, color="lightblue",stroke = 2)+
  theme_void()

p3_map = ggplot()+
  geom_sf(data = worldmap_merge_final_2,fill = 'grey90',col = 'grey90')+
  geom_sf(data = worldmap_merge_final_2[worldmap_merge_final_2$iucn_region == df_cols$var1[3],],fill = df_cols$iucn_regions_pal[3],col = NA)+
  geom_point(aes(x=-7300000,y=2300000), size=4, shape=1, color="lightblue",stroke = 2)+
  theme_void()

p4_map = ggplot()+
  geom_sf(data = worldmap_merge_final_2,fill = 'grey90',col = 'grey90')+
  geom_sf(data = worldmap_merge_final_2[worldmap_merge_final_2$iucn_region == df_cols$var1[4],],fill = df_cols$iucn_regions_pal[4],col = NA)+
  #geom_point(aes(x=-7300000,y=2300000), size=8, shape=1, color="lightblue",stroke = 2)+
  theme_void()

p5_map = ggplot()+
  geom_sf(data = worldmap_merge_final_2,fill = 'grey90',col = 'grey90')+
  geom_sf(data = worldmap_merge_final_2[worldmap_merge_final_2$iucn_region == df_cols$var1[5],],fill = df_cols$iucn_regions_pal[5],col = NA)+
  #geom_point(aes(x=-7300000,y=2300000), size=8, shape=1, color="lightblue",stroke = 2)+
  theme_void()

p6_map = ggplot()+
  geom_sf(data = worldmap_merge_final_2,fill = 'grey90',col = 'grey90')+
  geom_sf(data = worldmap_merge_final_2[worldmap_merge_final_2$iucn_region == df_cols$var1[6],],fill = df_cols$iucn_regions_pal[6],col = NA)+
  #geom_point(aes(x=-7300000,y=2300000), size=8, shape=1, color="lightblue",stroke = 2)+
  theme_void()

p7_map = ggplot()+
  geom_sf(data = worldmap_merge_final_2,fill = 'grey90',col = 'grey90')+
  geom_sf(data = worldmap_merge_final_2[worldmap_merge_final_2$iucn_region == df_cols$var1[7],],fill = df_cols$iucn_regions_pal[7],col = NA)+
  #geom_point(aes(x=-7300000,y=2300000), size=8, shape=1, color="lightblue",stroke = 2)+
  theme_void()

p8_map = ggplot()+
  geom_sf(data = worldmap_merge_final_2,fill = 'grey90',col = 'grey90')+
  geom_sf(data = worldmap_merge_final_2[worldmap_merge_final_2$iucn_region == df_cols$var1[8],],fill = df_cols$iucn_regions_pal[8],col = NA)+
  #geom_point(aes(x=-7300000,y=2300000), size=8, shape=1, color="lightblue",stroke = 2)+
  theme_void()

p9_map = ggplot()+
  geom_sf(data = worldmap_merge_final_2,fill = 'grey90',col = 'grey90')+
  geom_sf(data = worldmap_merge_final_2[worldmap_merge_final_2$iucn_region == df_cols$var1[9],],fill = df_cols$iucn_regions_pal[9],col = NA)+
  #geom_point(aes(x=-7300000,y=2300000), size=8, shape=1, color="lightblue",stroke = 2)+
  theme_void()

p10_map = ggplot()+
  geom_sf(data = worldmap_merge_final_2,fill = 'grey90',col = 'grey90')+
  geom_sf(data = worldmap_merge_final_2[worldmap_merge_final_2$iucn_region == df_cols$var1[10],],fill = df_cols$iucn_regions_pal[10],col = NA)+
  #geom_point(aes(x=-7300000,y=2300000), size=8, shape=1, color="lightblue",stroke = 2)+
  theme_void()

p11_map = ggplot()+
  geom_sf(data = worldmap_merge_final_2,fill = 'grey90',col = 'grey90')+
  geom_sf(data = worldmap_merge_final_2[worldmap_merge_final_2$iucn_region == df_cols$var1[11],],fill = df_cols$iucn_regions_pal[11],col = NA)+
  #geom_point(aes(x=-7300000,y=2300000), size=8, shape=1, color="lightblue",stroke = 2)+
  theme_void()

p12_map = ggplot()+
  geom_sf(data = worldmap_merge_final_2,fill = 'grey90',col = 'grey90')+
  geom_sf(data = worldmap_merge_final_2[worldmap_merge_final_2$iucn_region == df_cols$var1[12],],fill = df_cols$iucn_regions_pal[12],col = NA)+
  #geom_point(aes(x=-7300000,y=2300000), size=8, shape=1, color="lightblue",stroke = 2)+
  theme_void()

pdf('mapsv2.pdf',width = 3,height = 10)
grid.arrange(p0_map,p1_map,p2_map,p3_map,p4_map,p5_map,p6_map,p7_map,p8_map,p9_map,p10_map,p11_map,p12_map,ncol=1)
dev.off()

#newest version - grouped
worldmap_merge_final_2 = st_transform(worldmap_merge_final_2,  
                                    crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0")

list_regions = c('North America','Mesoamerica','Caribbean Islands','South America',
                 'Europe','North Africa','Sub-Saharan Africa', 'West and Central Asia', 
                 'North Asia','East Asia','South and Southeast Asia','Oceania')


list_regions2 = list()
list_regions2[[1]] = c(list_regions[1])
list_regions2[[2]] = c(list_regions[2],list_regions[3],list_regions[4])
list_regions2[[3]] = c(list_regions[5],list_regions[8],list_regions[9],list_regions[10])
list_regions2[[4]] = c(list_regions[6],list_regions[7])
list_regions2[[5]] = c(list_regions[11],list_regions[12])


p0_map = ggplot()+
  #geom_sf(data = worldmap_merge_final_2,fill = 'grey90',col = 'grey90')+
  geom_sf(data = worldmap_merge_final_2,aes(fill = iucn_region),col = NA)+
  scale_fill_manual('',values = df_cols$iucn_regions_pal)+
  theme_void()+
  theme(legend.position = 'none')+
  geom_point(aes(x=-7300000,y=2300000), size=4, shape=1, color="lightblue",stroke = 1)

pal_temp = df_cols[df_cols$var1 %in% unlist(list_regions2[1]),]$iucn_regions_pal

p1_map = ggplot()+
  geom_sf(data = worldmap_merge_final_2,fill = 'grey90',col = 'grey90')+
  geom_sf(data = worldmap_merge_final_2[worldmap_merge_final_2$iucn_region %in% list_regions2[[1]],],
          aes(fill = iucn_region),col = NA)+
  scale_fill_manual('',values = c(pal_temp))+
  theme_void()+
  theme(legend.position = 'none')
pal_temp = df_cols[df_cols$var1 %in% unlist(list_regions2[2]),]$iucn_regions_pal

p2_map = ggplot()+
  geom_sf(data = worldmap_merge_final_2,fill = 'grey90',col = 'grey90')+
  geom_sf(data = worldmap_merge_final_2[worldmap_merge_final_2$iucn_region %in% list_regions2[[2]],],
          aes(fill = iucn_region),col = NA)+
  scale_fill_manual('',values = c(pal_temp))+
  theme_void()+
  theme(legend.position = 'none')+
  geom_point(aes(x=-7300000,y=2300000), size=4, shape=1, color="lightblue",stroke = 1)
  
pal_temp = df_cols[df_cols$var1 %in% unlist(list_regions2[3]),]$iucn_regions_pal


p3_map = ggplot()+
  geom_sf(data = worldmap_merge_final_2,fill = 'grey90',col = 'grey90')+
  geom_sf(data = worldmap_merge_final_2[worldmap_merge_final_2$iucn_region %in% list_regions2[[3]],],
          aes(fill = iucn_region),col = NA)+
  scale_fill_manual('',values = c(pal_temp))+
  theme_void()+
  theme(legend.position = 'none')

pal_temp = df_cols[df_cols$var1 %in% unlist(list_regions2[4]),]$iucn_regions_pal


p4_map = ggplot()+
  geom_sf(data = worldmap_merge_final_2,fill = 'grey90',col = 'grey90')+
  geom_sf(data = worldmap_merge_final_2[worldmap_merge_final_2$iucn_region %in% list_regions2[[4]],],
          aes(fill = iucn_region),col = NA)+
  scale_fill_manual('',values = c(pal_temp))+
  theme_void()+
  theme(legend.position = 'none')

pal_temp = df_cols[df_cols$var1 %in% unlist(list_regions2[5]),]$iucn_regions_pal


p5_map = ggplot()+
  geom_sf(data = worldmap_merge_final_2,fill = 'grey90',col = 'grey90')+
  geom_sf(data = worldmap_merge_final_2[worldmap_merge_final_2$iucn_region %in% list_regions2[[5]],],
          aes(fill = iucn_region),col = NA)+
  scale_fill_manual('',values = c(pal_temp))+
  theme_void()+
theme(legend.position = 'none')
  

library(gridExtra)

grid_maps = arrangeGrob(p0_map,p1_map,p2_map,p3_map,p4_map,p5_map,ncol=1)
ggsave(plot = grid_maps,filename = 'grid_maps.png',width = 3,height = 4,dpi = 1000)

pdf('mapsv3.pdf',width = 3,height = 4)
grid.arrange(p0_map,p1_map,p2_map,p3_map,p4_map,p5_map,ncol=1)
dev.off()




worldmap_merge_final = st_transform(worldmap_merge_final,  
                            crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0")



sf_land = st_read('/Users/gdt366/Dropbox/african_snakes/land-polygons-complete-4326/land_polygons.shp')
sf_land = st_transform(sf_land,  
                       crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0")
sf_land$fake = 1
#sf_land = sf_land %>% group_by(fake) %>% summarise()


sf_use_s2(F)
r = raster(st_zm(sf_land), resolution = 50000 ,
           crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0")

r_grid = st_as_sf(rasterToPolygons(r))
r_grid$layer = 1:nrow(r_grid)

library(dplyr)
worldmap_merge_final_summ = worldmap_merge_final %>% group_by(iucn_region) %>% summarise()
save(worldmap_merge_final_summ,file = 'worldmap_merge_final_summ.Rdata')
iucn_regions_grids = st_intersects(worldmap_merge_final_summ,r_grid)
test=cbind.data.frame(t(iucn_regions_grids))

plot(r_grid[r_grid$layer %in% test[duplicated(test$row.id),]$row.id,]$geometry)

intersected_cells = r_grid[r_grid$layer %in% test[duplicated(test$row.id),]$row.id,]

worldmap_merge_final_summ$layer = 1:nrow(worldmap_merge_final_summ)
intersected_cells_intersected=st_intersection(worldmap_merge_final_summ,intersected_cells)
intersected_cells_intersected$area = st_area(intersected_cells_intersected)
units(intersected_cells_intersected$area) = NULL

list_grids = sort(unique(intersected_cells_intersected$layer.1))
final_df = data.frame()
for (i in seq_along(list_grids)){
  print(i)
  temp_sf = intersected_cells_intersected[intersected_cells_intersected$layer.1 == list_grids[i],]
  temp_df = cbind.data.frame(cell_id = list_grids[i],
                             larger_realm = temp_sf[temp_sf$area == max(temp_sf$area),]$layer)
  final_df = rbind(final_df,temp_df)
}

iucn_regions_grids_0 = test[!test$row.id %in% list_grids,] 

colnames(iucn_regions_grids_0) = colnames(final_df)

iucn_regions_grids_final = rbind(iucn_regions_grids_0,final_df)

names_iucn_regions = cbind.data.frame(larger_realm = 1:14,
                                names = worldmap_merge_final_summ$iucn_region)



iucn_regions_grids_final2 = merge(iucn_regions_grids_final,names_iucn_regions,by.x = 'larger_realm',by.y = 'larger_realm')

nrow(iucn_regions_grids_final) == nrow(iucn_regions_grids_final2)

write.csv(iucn_regions_grids_final2,'iucn_regions_grids_final2.csv')

