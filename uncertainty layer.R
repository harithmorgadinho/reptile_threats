#
library(sf)
sf_use_s2(F)
library(raster)
library(fasterize)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)


worldMap <- rnaturalearth::ne_countries(scale = 10, type = "countries", returnclass = 'sf')


worldMap_moll = st_transform(worldMap,  
                             crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0")

r = raster(st_zm(worldMap_moll), resolution = 50000 ,
           crs = '+proj=moll +lon_0=0 +x_0=0 +y_0=0')

load('sf_reptiles_summarized.Rdata')

sf_reptiles = st_transform(sf_reptiles,  
                           crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0")

worldMap_moll = st_union(worldMap_moll)

sf_reptiles$range_size = st_area(sf_reptiles)

sf_reptiles_large = sf_reptiles[sf_reptiles$range_size > 0.1*max(sf_reptiles$range_size),]

sf_reptiles_small = sf_reptiles[sf_reptiles$range_size < 0.1*max(sf_reptiles$range_size),]

nrow(sf_reptiles) == nrow(sf_reptiles_large) + nrow(sf_reptiles_small)

sf_reptiles_large[sf_reptiles_large$range_size == min(sf_reptiles_large$range_size),]$binomial


sf_reptiles_large = st_buffer(sf_reptiles_large,dist = 0)

sf_reptiles_large_intersected = st_intersection(sf_reptiles_large,worldMap_moll)

nrow(sf_reptiles) == nrow(sf_reptiles_large_intersected) + nrow(sf_reptiles_small)

sf_reptiles = rbind(sf_reptiles_large_intersected,sf_reptiles_small)


save(sf_reptiles,file = 'sf_reptiles_summarized_intersection.Rdata')

setwd("/Users/harith/Dropbox/Copenhagen_postdoc/reptiles_tiffs")

library(terra)
sp_raster = function(x,vect){
  tempsf = vect[x,]
  z <- terra::rasterize(tempsf, r_test, "value",touches = TRUE,background=NA)
  z_test = raster(z)
  raster::writeRaster(z_test, filename=paste0("iteration_",tempsf$binomial,".tif"), format="GTiff", overwrite=TRUE)
}

r_test <- rast(r)
v <- vect(sf_reptiles)
v$value = 1

lapply(1:9827, function(x,vect) (sp_raster(x,vect = v)))

rastlist <- list.files(path = getwd(), pattern='.tif$', all.files= T, full.names= T)
stk2 <- rast(rastlist)

rastlist = sort(rastlist)
sf_reptiles = sf_reptiles[order(sf_reptiles$binomial,decreasing = F),]


stk2[is.nan(stk2)] = 0

rsum = app(stk2[[1:9827]], sum)
plot(rsum)
rsum2 = raster(rsum)
raster::writeRaster(rsum2, filename="reptiles_richness.tif", format="GTiff", overwrite=TRUE)

#rastlist[9000:9010]
#sf_reptiles$binomial[9000:9010]

sf_reptiles$range_size = st_area(sf_reptiles)

cuberoot = function(x){
  if(x < 0)
  { - (-x)^(1/3)}
  else
  {x^(1/3)}
}

units(sf_reptiles$range_size) = NULL

sf_reptiles$uncertainty = 1/cuberoot(sf_reptiles$range_size)


stk2_uncert=stk2*sf_reptiles$uncertainty

stk2_uncertsum = app(stk2_uncert[[1:9827]], sum)

plot(log10(stk2_uncertsum))

stk2_uncertsum2 = raster(stk2_uncertsum)
plot(stk2_uncertsum2)

stk2_uncertsum2[stk2_uncertsum2 == 0] = NA
plot(log10(stk2_uncertsum2))

raster::writeRaster(stk2_uncertsum2, filename="reptiles_uncertainty.tif", format="GTiff", overwrite=TRUE)


### threats

part1 = read.csv('/Users/harith/Dropbox/Copenhagen_postdoc/redlist_species_data_1e6a78a5-c09e-430f-906e-a710a61fb4b0/threats.csv',stringsAsFactors = F)
part2 = read.csv('/Users/harith/Dropbox/Copenhagen_postdoc/redlist_species_data_0e1e13b5-f355-4302-9cf4-822d33b08258/threats.csv',stringsAsFactors = F)

all_threats = rbind(part1,part2)

hunting = c("5.1.1","5.1.2","5.1.3","5.1.4")
agriculture = c("2.1.1","2.1.2","2.1.3","2.1.4","2.2.1","2.2.2","2.2.3","2.3.1",
                "2.3.2","2.3.4","2.4.1","2.4.2","2.4.3")
logging = c("5.3.1", "5.3.2","5.3.3", "5.3.4", "5.3.5")
pollution = c("9.1.1" ,"9.1.2", "9.1.3" ,"9.2.1" ,"9.2.2", "9.2.3", "9.3.1", "9.3.2", "9.3.3", "9.3.4",
              "9.4","9.5.1", "9.5.2","9.5.3","9.5.4" ,"9.6.1","9.6.2","9.6.3","9.6.4")
climate_change = c("11.1", "11.2" ,"11.3" ,"11.4", "11.5")
invasive = c('8.1.1','8.1.2')

list_threats = list(hunting,agriculture,logging,pollution,invasive,climate_change)


rastlist <- list.files(path = getwd(), pattern='.tif$', all.files= T, full.names= T)
names_rasters=cbind.data.frame(rastlist,binomial = sf_reptiles$binomial)
names = c('hunting','agriculture','logging','pollution','invasive','climate_change')


for(i in seq_along(list_threats)){
  print(i)
  sp_list = sort(unique(all_threats[all_threats$code %in% list_threats[[i]],]$scientificName))
  
  temp_rast_list = names_rasters[names_rasters$binomial %in% sp_list,]$rastlist
  
  temp_rast_list <- rast(temp_rast_list)
  
  temp_rast_list[is.nan(temp_rast_list)] = 0
  
  rsum = app(temp_rast_list[[1:dim(temp_rast_list)[3]]], sum)
  
  rsum2 = raster(rsum)
  raster::writeRaster(rsum2, filename=paste0("/Users/harith/Dropbox/Copenhagen_postdoc/",names[i],"_reptiles.tif"), format="GTiff", overwrite=TRUE)

  uncertainty_values=sf_reptiles[sf_reptiles$binomial %in% sp_list,]$uncertainty
  length(uncertainty_values) ==  dim(temp_rast_list)[3]
  
  temp_rast_list_uncert=temp_rast_list*uncertainty_values
  temp_rast_list_uncert_rsum = app(temp_rast_list_uncert[[1:dim(temp_rast_list_uncert)[3]]], sum)
  temp_rast_list_uncert_rsum = raster(temp_rast_list_uncert_rsum)
  raster::writeRaster(temp_rast_list_uncert_rsum, filename=paste0("/Users/harith/Dropbox/Copenhagen_postdoc/",names[i],"_uncertainty_reptiles.tif"), format="GTiff", overwrite=TRUE)
  
  
  }





