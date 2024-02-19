library(sf)
library(raster)
load('/Users/gdt366/Dropbox/Postdoc_socioeconomic/r_grid_13_dec.Rdata')
load('probability_extinction_risk_exp.Rdata')
load('probability_any_threat.Rdata')
load('probability_extinction_risk_exp_no_dd.Rdata')
r_grid$probability_extinction_risk_exp = probability_extinction_risk_exp
r_grid$any_threat = probability_any_threat
r_grid$probability_extinction_risk_exp_no_dd = probability_extinction_risk_exp_no_dd

r_grid$threatened_richness =  threatened_richness
r_grid$probability_extinction_risk_exp_no_dd_2x = probability_extinction_risk_exp_no_dd_2x
r_grid$probability_extinction_risk_exp_no_dd_log10 = probability_extinction_risk_exp_no_dd_log10
r_grid$probability_extinction_risk_exp_no_dd_cube = probability_extinction_risk_exp_no_dd_cube
r_grid$probability_extinction_risk_exp_no_dd_1_5 = probability_extinction_risk_exp_no_dd_1_5
r_grid$richness_no_dd = richness_no_dd
r_grid$probability_extinction_risk_1_5 = probability_extinction_risk_1_5
r_grid$probability_extinction_risk_exp_no_dd_exp_1 = probability_extinction_risk_exp_no_dd_exp_1
r_grid$probability_extinction_risk_exp_no_dd_exp_2 = probability_extinction_risk_exp_no_dd_exp_2


r_grid$probability_extinction_risk_exp_no_dd_3x = probability_extinction_risk_exp_no_dd_3x


#r_grid$probability_extinction_risk_exp_no_dd = probability_extinction_risk_exp_no_dd_2x


r_grid$probability_extinction_risk_mean = probability_extinction_risk_mean

ggplot()+
  #geom_point(data = r_grid,aes(x = threatened_richness/richness_no_dd,y = probability_extinction_risk_mean))+
  geom_point(data = r_grid,aes(x = threatened_richness/richness_no_dd,y = probability_extinction_risk_exp_no_dd_2x),col = 'red',shape = 21)+
  geom_point(data = r_grid,aes(x = threatened_richness/richness_no_dd,y = probability_extinction_risk_exp_no_dd),col = 'blue',shape = 21)+
  #geom_point(data = r_grid,aes(x = threatened_richness/richness_no_dd,y = probability_extinction_risk_exp_no_dd_3x),col = 'green',shape = 21)+
  geom_point(data = r_grid,aes(x = threatened_richness/richness_no_dd,y = probability_extinction_risk_exp_no_dd_exp_1),col = 'purple',shape = 21)+
geom_point(data = r_grid,aes(x = threatened_richness/richness_no_dd,y = probability_extinction_risk_exp_no_dd_exp_2),col = 'cyan',shape = 21)



ggplot()+
    #geom_point(data = r_grid,aes(x = threatened_richness/richness_no_dd,y = probability_extinction_risk_mean))+
    geom_point(data = r_grid,aes(x = threatened_richness,y = probability_extinction_risk_exp_no_dd_2x),col = 'red',shape = 21)+
    geom_point(data = r_grid,aes(x = threatened_richness,y = probability_extinction_risk_exp_no_dd),col = 'blue',shape = 21)+
  geom_point(data = r_grid,aes(x = threatened_richness,y = probability_extinction_risk_exp_no_dd_3x),col = 'green',shape = 21)
  


ggplot()+
   geom_point(data = r_grid,aes(x = probability_extinction_risk_exp_no_dd_2x,y = probability_extinction_risk_exp_no_dd_exp_2),col = 'blue',shape = 21)
  

r_grid$prop_threat = r_grid$threatened_richness/r_grid$richness_no_dd

  

model = lm(data = r_grid,probability_extinction_risk_exp_no_dd~prop_threat)
summary(model)
AIC(model)
model = lm(data = r_grid,probability_extinction_risk_exp_no_dd_2x~prop_threat)
summary(model)
AIC(model)
model = lm(data = r_grid,probability_extinction_risk_exp_no_dd_exp_2~prop_threat)
summary(model)
AIC(model)
model = lm(data = r_grid,probability_extinction_risk_exp_no_dd_3x~prop_threat)
summary(model)
AIC(model)
model = lm(data = r_grid,probability_extinction_risk_exp_no_dd_exp_2~probability_extinction_risk_exp_no_dd_2x)
summary(model)
AIC(model)


st_geometry(r_grid) = NULL

r_grid = r_grid[r_grid$richness_r >9,]

model = lm(data = r_grid,probability_extinction_risk_exp_no_dd_exp_2~prop_threat)
summary(model)

model = lm(data = r_grid,probability_extinction_risk_exp_no_dd~prop_threat)
summary(model)

df_tests = melt(r_grid[,c('prop_threat','probability_extinction_risk_exp_no_dd_exp_2','probability_extinction_risk_exp_no_dd')],id.vars = 'prop_threat')
colnames(df_tests)
p = ggplot(data = df_tests,aes(x = prop_threat,y = value, col = variable))+geom_point()+
  theme_minimal()+
  theme(legend.position = c(0.75,0.25),
        axis.title.y = element_blank())+
  scale_colour_manual('',values = c('red','blue'),labels = c('e^ER+ER^2','e^ER'))+
  geom_abline(intercept = 0, slope = 1)+
  xlab('Proportion of threatened species')

ggsave(filename = 'plot_models.png',plot = p,width = 6,height = 6,dpi = 1000)



r_grid[is.na(r_grid$hunting_r_unc_med) & !is.na(r_grid$hunting_r) ,]$hunting_r = 0
r_grid[is.na(r_grid$agriculture_r_unc_med) & !is.na(r_grid$agriculture_r) ,]$agriculture_r = 0
r_grid[is.na(r_grid$invasives_r_unc_med) & !is.na(r_grid$invasives_r) ,]$invasives_r = 0
r_grid[is.na(r_grid$logging_r_unc_med) & !is.na(r_grid$logging_r) ,]$logging_r = 0
r_grid[is.na(r_grid$pollution_r_unc_med) & !is.na(r_grid$pollution_r) ,]$pollution_r = 0
r_grid[is.na(r_grid$urbanization_r_unc_med) & !is.na(r_grid$urbanization_r) ,]$urbanization_r = 0
r_grid[is.na(r_grid$climate_change_r_unc_med) & !is.na(r_grid$climate_change_r) ,]$climate_change_r = 0

r_grid[r_grid$threatened_richness %in% 0 & !is.na(r_grid$probability_extinction_risk_exp_no_dd) ,]$probability_extinction_risk_exp_no_dd = 0

p = ggplot()+geom_sf(data = r_grid, aes(fill = probability_extinction_risk_exp_no_dd,
                                        col  = probability_extinction_risk_exp_no_dd),linewidth = 0)+
  scale_fill_gradientn('',colours = rainbow(10))+
  scale_colour_gradientn('',colours = rainbow(10))

colnames(r_grid)
p = ggplot()+geom_sf(data = r_grid, aes(fill = threatened_r,
                                        col  = threatened_r),linewidth = 0)+
  scale_fill_gradientn('',colours = rainbow(10))+
  scale_colour_gradientn('',colours = rainbow(10))

p = ggplot()+geom_sf(data = r_grid, aes(fill = probability_extinction_risk_exp_no_dd_2x,
                                        col  = probability_extinction_risk_exp_no_dd_2x),linewidth = 0)+
  scale_fill_gradientn('',colours = rainbow(10))+
  scale_colour_gradientn('',colours = rainbow(10))


ggplot(data = r_grid,aes(x = probability_extinction_risk_exp_no_dd,y = threatened_r))+geom_point()+
  geom_smooth(method = 'lm')

ggplot(data = r_grid,aes(x = threatened_richness/richness_no_dd,y = probability_extinction_risk_exp_no_dd))+geom_point()

ggplot(data = r_grid,aes(x = threatened_richness/richness_r,y = probability_extinction_risk_exp_no_dd_cube))+geom_point()
  

ggplot(data = r_grid,aes(x = threatened_richness/richness_r,y = probability_extinction_risk_exp_no_dd_log10))+geom_point()+
  geom_smooth(method = 'lm')

ggplot(data = r_grid,aes(x = threatened_richness/richness_no_dd,y = probability_extinction_risk_exp_no_dd_1_5))+geom_point()


ggplot(data = r_grid,aes(x = threatened_richness/richness_no_dd,y = probability_extinction_risk_exp_no_dd_2x))+geom_point()

ggplot(data = r_grid,aes(x = threatened_richness/richness_no_dd,y = probability_extinction_risk_1_5))+geom_point()


model = lm(data = r_grid,probability_extinction_risk_exp_no_dd_2x~probability_extinction_risk_exp_no_dd)
summary(model)
hist(r_grid$hunting_r)

hist(r_grid$probability_extinction_risk_exp_no_dd)
r_grid[r_grid$threatened_richness %in% 0 & r_grid$richness_r>0,]$probability_extinction_risk_exp_no_dd = 0



r_grid$hunting_r = log10(r_grid$hunting_r)/max(log10(r_grid$hunting_r))
r_grid$agriculture_r = log10(r_grid$agriculture_r)/max(log10(r_grid$agriculture_r))
r_grid$invasives_r = log10(r_grid$invasives_r)/max(log10(r_grid$invasives_r))
r_grid$logging_r = log10(r_grid$logging_r)/max(log10(r_grid$logging_r))
r_grid$pollution_r = log10(r_grid$pollution_r)/max(log10(r_grid$pollution_r))
r_grid$urbanization_r = log10(r_grid$urbanization_r)/max(log10(r_grid$urbanization_r))
r_grid$climate_change_r = log10(r_grid$climate_change_r)/max(log10(r_grid$climate_change_r))

r_grid$probability_extinction_risk_exp_no_dd = probability_extinction_risk_exp_no_dd_exp_2

r_grid[is.infinite(r_grid$hunting_r),]$hunting_r = NA
r_grid[is.infinite(r_grid$agriculture_r),]$agriculture_r = NA
r_grid[is.infinite(r_grid$invasives_r),]$invasives_r = NA
r_grid[is.infinite(r_grid$logging_r),]$logging_r = NA
r_grid[is.infinite(r_grid$pollution_r),]$pollution_r = NA
r_grid[is.infinite(r_grid$urbanization_r),]$urbanization_r = NA
r_grid[is.infinite(r_grid$climate_change_r),]$climate_change_r = NA

r_grid[is.infinite(r_grid$probability_extinction_risk_exp_no_dd),]$probability_extinction_risk_exp_no_dd = NA
unique(r_grid$IUCN_REGIONS)

ggplot(data = r_grid[r_grid$IUCN_REGIONS %in% 'Sub-Saharan Africa',],aes(x = climate_change_r,y = probability_extinction_risk_exp_no_dd))+
  geom_point(size = 0.25,alpha = 1,col = 'transparent',shape = 21,stroke=0.01)+
  geom_smooth(method = lm, size = 0.75, color = alpha('black',0.5), se = FALSE)

model1 = lm(data = r_grid[r_grid$IUCN_REGIONS %in% 'Sub-Saharan Africa',],probability_extinction_risk_exp_no_dd~hunting_r+agriculture_r+invasives_r+logging_r+pollution_r+urbanization_r+climate_change_r)
model1 = lm(data = r_grid,probability_extinction_risk_exp_no_dd~hunting_r+agriculture_r+invasives_r+logging_r+pollution_r+urbanization_r+climate_change_r)
model1 = lm(data = r_grid,probability_extinction_risk_exp_no_dd~climate_change_r)

summary(model1)

# r_grid$probability_extinction_risk_exp_no_dd = r_grid$probability_extinction_risk_exp_no_dd*r_grid$richness_r
# 
# r_grid$probability_extinction_risk_exp_no_dd = r_grid$probability_extinction_risk_exp_no_dd/max(na.omit(r_grid$probability_extinction_risk_exp_no_dd))

# r_grid$hunting_r = r_grid$hunting_r*r_grid$richness_r
# r_grid$agriculture_r = r_grid$agriculture_r*r_grid$richness_r
# r_grid$logging_r = r_grid$logging_r*r_grid$richness_r
# r_grid$climate_change_r = r_grid$climate_change_r*r_grid$richness_r
# r_grid$invasives_r = r_grid$invasives_r*r_grid$richness_r
# r_grid$urbanization_r = r_grid$urbanization_r*r_grid$richness_r
# r_grid$pollution_r = r_grid$pollution_r*r_grid$richness_r
# 
# r_grid$hunting_r = r_grid$hunting_r/max(na.omit(r_grid$hunting_r ))
# r_grid$agriculture_r = r_grid$agriculture_r/max(na.omit(r_grid$agriculture_r ))
# r_grid$logging_r = r_grid$logging_r/max(na.omit(r_grid$logging_r ))
# r_grid$climate_change_r = r_grid$climate_change_r/max(na.omit(r_grid$climate_change_r ))
# r_grid$invasives_r = r_grid$invasives_r/max(na.omit(r_grid$invasives_r ))
# r_grid$urbanization_r = r_grid$urbanization_r/max(na.omit(r_grid$urbanization_r ))
# r_grid$pollution_r = r_grid$pollution_r/max(na.omit(r_grid$pollution_r ))



r_grid2 = r_grid
colnames(r_grid2)

r_grid2 = r_grid2[r_grid2$richness_r>9,]

pal = colorRampPalette(c("#3F7F9F","#5AB7F1","#90C15E","#F6CB45","#DA3D31"))

#  p = ggplot()+geom_sf(data = r_grid2[r_grid2$richness>15,], aes(fill = richness),col = NA)+
#    scale_fill_gradientn('',colours =c(pal(30)))+
#    theme_void()
# pdf('test_richness_v2.pdf')
# print(p)
# dev.off()





# list_cells_with_data=r_grid3[!is.na(r_grid3$hunting) | !is.na(r_grid3$agriculture) | !is.na(r_grid3$logging) | !is.na(r_grid3$pollution)
#                              | !is.na(r_grid3$invasives) | !is.na(r_grid3$invasives) | !is.na(r_grid3$climate_change) | !is.na(r_grid3$urbanization),]$layer
# 
# 
# na_sf=r_grid3[r_grid3$layer %in% list_cells_with_data & is.na(r_grid3$names),]
# 
# 
# distance_cells_to_regions=st_distance(na_sf,worldmap_merge_final_summ)
# 
# units(distance_cells_to_regions) = NULL
# final_df_nas = data.frame()
# for(i in 1:nrow(distance_cells_to_regions)){
#   print(i)
#   temp = distance_cells_to_regions[i,]
#   which(temp == min(temp))
#   tempdf=cbind.data.frame(i,region = which(temp == min(temp)))
#   final_df_nas = rbind(final_df_nas,tempdf) 
# }
# unique(final_df_nas$region)
# 
# final_df_nas$layer = na_sf$layer
# final_df_nas_fixed = merge(final_df_nas,names_iucn_regions,by.x = 'region',by.y = 'larger_realm',sort = F)
# final_df_nas_fixed = final_df_nas_fixed[order(final_df_nas_fixed$layer),]
# na_sf$names = final_df_nas_fixed$names
# 
# 
# 
# r_grid3[r_grid3$layer %in% list_cells_with_data & is.na(r_grid3$names),]$names = final_df_nas_fixed$names


# p = ggplot()+geom_sf(data = r_grid3, aes(fill = names),col = NA)
# pdf('test_nas_22_4.pdf')
# print(p)
# dev.off()


library(reshape2)

r_grid4 = r_grid2
st_geometry(r_grid4) = NULL
colnames(r_grid4)[235] = 'iucn_region'

r_grid4_melt0 = melt(r_grid4[,c('layer','hunting_r','agriculture_r','logging_r','pollution_r','invasives_r','climate_change_r','urbanization_r','iucn_region','any_threat')],id = c('layer','iucn_region'))
r_grid4_melt1 = melt(r_grid4[,c('layer','probability_extinction_risk_exp_no_dd')],id = c('layer'))

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
r_grid4_melt$variable.x = factor(r_grid4_melt$variable.x, levels = c('logging_r','pollution_r','agriculture_r','invasives_r','hunting_r','climate_change_r','urbanization_r','any_threat'))

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
    # scale_y_continuous(limits = c(-1,1))+
    # scale_x_continuous(limits = c(0,1))+
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


theme_harith =  theme(
  axis.text.y = element_blank(),
  axis.text.x = element_blank(),
  legend.position = 'none',
  plot.margin = unit(c(0,0,0,0),'cm'),
  panel.background = element_rect(fill = NA,colour = NA),
  panel.grid.major = element_line(colour = 'grey90',linewidth = 0.1),
  strip.text.x = element_blank(),
  panel.spacing = unit(0, "cm"),
  strip.background = element_blank())


for(i in seq_along(list_regions2)){
  test = data.frame(x = 0.5,y = -0.5, lab=finaldf2[finaldf2$region %in% unlist(list_regions2[i]),][,c(1,2,11)])
  pal_temp = df_cols[df_cols$var1 %in% unlist(list_regions2[i]),]$iucn_regions_pal
  
  to_remove = paste0(test[test$lab.symbols ==' ',]$lab.threat,'_',test[test$lab.symbols ==' ',]$lab.region)
  to_plot = r_grid4_melt[r_grid4_melt$iucn_region %in% unlist(list_regions2[i]),]
  to_plot$remove_list = paste0(to_plot$variable.x,'_',to_plot$iucn_region)
  to_plot = to_plot[!to_plot$remove_list %in% to_remove,]
  
  
  if(length(which(table(to_plot$variable.x)<1,))>0){
    
    fake_line=to_plot[1:length(names(which(table(to_plot$variable.x)<1,))),] 
    fake_line[3] = names(which(table(to_plot$variable.x)<1,))
    fake_line[4] = 0
    fake_line[6] = 0
    to_plot = rbind(to_plot, fake_line)
  }
  
  p = ggplot(data = to_plot,aes(x = value.x,y = value.y))+
    annotate("rect", xmin = -Inf, xmax =Inf, ymin = -Inf, ymax = Inf, fill = NA, alpha = 0.25) +
    geom_point(aes(fill = iucn_region),size = 0.25,alpha = 1,col = 'transparent',shape = 21,stroke=0.01)+
    geom_smooth(method = lm, size = 0.75, color = alpha('black',0.5), se = FALSE,aes(group = iucn_region)) + 
    stat_smooth(method='lm',alpha = 0.5,aes(col = iucn_region),size = 0.55)+
    facet_grid(~variable.x)+
    theme_void()+
   theme_harith+
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
  theme_harith+
  scale_y_continuous(limits = c(-0.5,1))+
  scale_x_continuous(limits = c(0,1))


grid_regression = arrangeGrob(p_world,list_plots[[1]],list_plots[[2]],list_plots[[3]],list_plots[[4]],list_plots[[5]],ncol = 1)

#ggsave(plot = grid_regression,filename = 'grid_regression_test_times_richnessv2.png',width = 6,height = 4,dpi = 2000)

ggsave(plot = grid_regression,filename = 'grid_regression_17_DEC_23.png',width = 6,height = 4,dpi = 2000)

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
write.xlsx(finaldf2,file = 'finaldf2_table_regressions_13_dec.xlsx')


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

df_summary_melt = df_summary_melt[!is.na(df_summary_melt$iucn_region),]


df_summary_melt_plot = ggplot(data = df_summary_melt)+
  geom_bar(aes(x = value,y = iucn_region,fill = variable),position = 'dodge',stat = 'identity',col='black',size = 0)+
  facet_wrap(~variable.x,nrow = 1)+
  geom_text(aes(x = value,y = iucn_region,label = round(value,2),group = variable),
            position = position_dodge(width = 0.9),hjust=0,size = 3)+
  theme_minimal()+
  theme(legend.position = 'bottom',axis.title = element_blank(),axis.text.x = element_blank(),
        axis.text.y = element_text(size = 7))+
  theme(plot.margin = margin(0,0,0,0, "cm"))+
  scale_x_continuous(limits = c(0,1.2))+
  scale_fill_manual('',values = c('steelblue','grey70'))


ggsave(filename = 'df_summary_melt_plot.png',width = 10,height = 10,dpi = 1000)



library(openxlsx)

write.xlsx(df_summary,file = 'df_summary.xlsx')
write.xlsx(df_n,file = 'df_n.xlsx')

r_grid4_melt
p1 = ggplot(data = r_grid4_melt)+
  geom_boxplot(aes(fill = variable.x,x = value.x,y  = iucn_region),outlier.size = 0.1)
p2 = ggplot(data = r_grid4_melt)+
  geom_boxplot(aes(fill = variable.y,x = value.y,y  = iucn_region),outlier.size = 0.1)



df_richness = r_grid %>% group_by(IUCN_REGIONS) %>% filter(richness_r>0)%>% summarise(median = median(richness_r),avg = mean(richness_r),sd = sd(richness_r))

df_threat = r_grid %>% group_by(IUCN_REGIONS) %>% filter(richness_r>9)%>% summarise(median = median(any_threat_r),avg = mean(any_threat_r),sd = sd(any_threat_r))

r_grid$probability_extinction_risk_exp_no_dd = probability_extinction_risk_exp_no_dd

df_extinction_risk = r_grid %>% group_by(IUCN_REGIONS) %>% filter(richness_r>9)%>% summarise(median = median(probability_extinction_risk_exp_no_dd),avg = mean(probability_extinction_risk_exp_no_dd),sd = sd(probability_extinction_risk_exp_no_dd))


r_grid$extinction_risk_sum = extinction_risk_sum/max(extinction_risk_sum)

hist(r_grid$extinction_risk_sum)


ggplot()+
  geom_point(data = r_grid,aes(x = extinction_risk_sum,y = probability_extinction_risk_exp_no_dd*richness_r))


p1 = ggplot() +
  #geom_sf(data = sf_land_union,linewidth = 0,bg = 'grey70',col =  'grey70') +
  #geom_sf(data = r_grid2[r_grid2$land %in% 1,],linewidth = 0,bg = 'grey70',col =  'grey70') +
  geom_sf(data = r_grid[r_grid$extinction_risk_sum>0,],aes(fill = extinction_risk_sum,col = extinction_risk_sum),linewidth = 0) +
  scale_fill_gradientn('Impact probability',colours = color.schemes$Hunting,na.value = NA,limits = c(0,1))+
  scale_colour_gradientn('Impact probability',colours = color.schemes$Hunting,na.value = NA,limits = c(0,1))+
  #theme_void()+
  annotate('text',x = 16382470,y = -7053445,label = 'Extinction risk',size = annotate.size)+
  theme_harith+
  guides(fill = guide_colorbar(title.position = "right"),
         plot.tag.position = c(0.1,0.9))+
  labs(tag = 'a')

p2 = ggplot() +
  #geom_sf(data = sf_land_union,linewidth = 0,bg = 'grey70',col =  'grey70') +
  #geom_sf(data = r_grid2[r_grid2$land %in% 1,],linewidth = 0,bg = 'grey70',col =  'grey70') +
  geom_sf(data = r_grid,aes(fill = probability_extinction_risk_exp_no_dd,col = probability_extinction_risk_exp_no_dd),linewidth = 0) +
  scale_fill_gradientn('Impact probability',colours = color.schemes$Hunting,na.value = NA,limits = c(0,1))+
  scale_colour_gradientn('Impact probability',colours = color.schemes$Hunting,na.value = NA,limits = c(0,1))+
  #theme_void()+
  annotate('text',x = 16382470,y = -7053445,label = 'Extinction risk',size = annotate.size)+
  theme_harith+
  guides(fill = guide_colorbar(title.position = "right"),
         plot.tag.position = c(0.1,0.9))+
  labs(tag = 'a')

p3 = ggplot() +
  #geom_sf(data = sf_land_union,linewidth = 0,bg = 'grey70',col =  'grey70') +
  #geom_sf(data = r_grid2[r_grid2$land %in% 1,],linewidth = 0,bg = 'grey70',col =  'grey70') +
  geom_sf(data = r_grid,aes(fill = probability_extinction_risk_exp_no_dd*richness_r,col = probability_extinction_risk_exp_no_dd*richness_r),linewidth = 0) +
  scale_fill_gradientn('Impact probability',colours = color.schemes$Hunting,na.value = NA)+
  scale_colour_gradientn('Impact probability',colours = color.schemes$Hunting,na.value = NA)+
  #theme_void()+
  annotate('text',x = 16382470,y = -7053445,label = 'Extinction risk',size = annotate.size)+
  theme_harith+
  guides(fill = guide_colorbar(title.position = "right"),
         plot.tag.position = c(0.1,0.9))+
  labs(tag = 'a')



ggsave(plot = p1,filename = 'p1.png')

ggsave(plot = p2,filename = 'p2.png')

ggsave(plot = p3,filename = 'p3.png')



df_threat$copy_paste = paste0(round(df_threat$median,2),'(',round(df_threat$sd,2),')')


df_richness$copy_paste = paste0(df_richness$median,2,'(',round(df_richness$sd,2),')')



st_geometry(df_richness) = NULL

st_geometry(df_threat) = NULL

write.xlsx(df_richness,'df_richness_14_dec.xlsx')
write.xlsx(df_threat,'df_threat_14_dec.xlsx')


