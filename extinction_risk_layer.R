##### new

load('/Users/gdt366/Dropbox/postdoc_KU_paper_2/reptiles_sf_summ_all_sps_present_only_terrestrial.Rdata')
unique(reptiles_sf_summ$category)
reptiles_sf_summ = reptiles_sf_summ[reptiles_sf_summ$category %in% c('DD','LC','NT','VU','EN','CR'),]
sf = st_transform(reptiles_sf_summ,'+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs')


library(reshape2)
df = melt(cbind.data.frame( DD = 0.0513,
                            LC = 0.0009,
                            NT = 0.0071, 
                            VU = 0.0513, 
                            EN = 0.4276, 
                            CR = 0.9688))


#df$value = exp(df$value)

#plot(df$value)

colnames(df)[2] = 'extinction_risk'


sf = merge(sf,df,by.x = 'category',by.y = 'variable')

sf = sf[order(sf$binomial,decreasing = F),]


extinction_risk_sum = function(i){
  if(length(x[[i]])==0){
    return(0)
  }else{
    return(sum(y[x[[i]],]$extinction_risk))
  }
}


sf_intersected = st_intersects(sf,r_grid)
x = t(sf_intersected)
y = sf
colnames(sf)
extinction_risk_sum = unlist(lapply(X = 1:length(x),FUN = extinction_risk_sum))

sf = st_transform(reptiles_sf_summ,'+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs')


library(reshape2)
df = melt(cbind.data.frame( DD = 0.0513,
                            LC = 0.0009,
                            NT = 0.0071, 
                            VU = 0.0513, 
                            EN = 0.4276, 
                            CR = 0.9688))


df$value = exp(df$value)


sf = merge(sf,df,by.x = 'category',by.y = 'variable')

sf = sf[order(sf$binomial,decreasing = F),]

sf_intersected = st_intersects(sf,r_grid)
x = t(sf_intersected)
y = sf
colnames(sf)
extinction_risk_sum_exp = unlist(lapply(X = 1:length(x),FUN = extinction_risk_sum))

hist(extinction_risk_sum)
save(extinction_risk_sum,file = 'extinction_risk_sum.Rdata')


