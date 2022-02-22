library(raster)
library(sf)
library(exactextractr)
library(DescTools)

#district polygon -------------------------------------------------------------
dist_shp<- st_read("X:/Spatial Stat/WASH Cholera/new_data/dist_shp.shp")
dist_shp<- st_as_sf(dist_shp)
dist_shp$NL_NAME_1<- NULL
dist_shp$NL_NAME_2<- NULL
dist_shp$VARNAME_2<- NULL

# population data -------------------------------------------------------------
meanPop<- raster("X:/Spatial Stat/WASH Cholera/WorldPop/pop_count/meanPop_count.tifGeoTiff.gri")

# population weighted cholera incidence ---------------------------------------
cholera<- raster("X:/Spatial Stat/WASH Cholera/cholera_incidence_raster_2010-2016/incidence/afro_2010-2016_lambda_mean.gri")
# cases per 1000 
cholera_incidence_in_1000<- raster::calc(
  cholera, function(x) x*1000
)

lambda_resampled_pop<- exactextractr::exact_resample(x= meanPop, y= cholera_incidence_in_1000, fun= "mean")
lambda_resampled_pop[is.na(lambda_resampled_pop)]<- 0 

dist_shp$incidence_in_thousan<- exact_extract(x= cholera_incidence_in_1000, y= dist_shp, 'weighted_mean', weights = lambda_resampled_pop)

# WASH weighted mean per district 
ImpWater.mean<- raster("X:/Spatial Stat/WASH Cholera/LMIC.WASH/data/means/W_IMP_PE.tifGeoTiff.gri")
lambda_resampled_pop_wash<- exactextractr::exact_resample(x= meanPop, y= ImpWater.mean, fun= "mean")
lambda_resampled_pop_wash[is.na(lambda_resampled_pop_wash)]<- 0 

dist_shp$W_Imp<- exact_extract(x= raster("X:/Spatial Stat/WASH Cholera/LMIC.WASH/data/means/W_IMP_PE.tifGeoTiff.gri"), y= dist_shp, 'weighted_mean', weights = lambda_resampled_pop_wash)
#dist_shp$W_ImpOth<- exact_extract(x= raster("X:/Spatial Stat/WASH Cholera/LMIC.WASH/data/means/W_IMP_OT.tifGeoTiff.gri"), y= dist_shp, 'weighted_mean', weights = lambda_resampled_pop_wash)
dist_shp$W_Pip<- exact_extract(x= raster("X:/Spatial Stat/WASH Cholera/LMIC.WASH/data/means/W_PIPED_.tifGeoTiff.gri"), y= dist_shp, 'weighted_mean', weights = lambda_resampled_pop_wash)
dist_shp$W_Sur<- exact_extract(x= raster("X:/Spatial Stat/WASH Cholera/LMIC.WASH/data/means/W_SURFAC.tifGeoTiff.gri"), y= dist_shp, 'weighted_mean', weights = lambda_resampled_pop_wash)
dist_shp$W_Uni<- exact_extract(x= raster("X:/Spatial Stat/WASH Cholera/LMIC.WASH/data/means/W_UNIMP_.tifGeoTiff.gri"), y= dist_shp, 'weighted_mean', weights = lambda_resampled_pop_wash)
dist_shp$S_Imp<- exact_extract(x= raster("X:/Spatial Stat/WASH Cholera/LMIC.WASH/data/means/S_IMP_PE.tifGeoTiff.gri"), y= dist_shp, 'weighted_mean', weights = lambda_resampled_pop_wash)
#dist_shp$S_ImpOth<- exact_extract(x= raster("X:/Spatial Stat/WASH Cholera/LMIC.WASH/data/means/S_IMP_OT.tifGeoTiff.gri"), y= dist_shp, 'weighted_mean', weights = lambda_resampled_pop_wash)
dist_shp$S_OD<- exact_extract(x= raster("X:/Spatial Stat/WASH Cholera/LMIC.WASH/data/means/S_OD_PER.tifGeoTiff.gri"), y= dist_shp, 'weighted_mean', weights = lambda_resampled_pop_wash)
dist_shp$S_Pip<- exact_extract(x= raster("X:/Spatial Stat/WASH Cholera/LMIC.WASH/data/means/S_PIPED_.tifGeoTiff.gri"), y= dist_shp, 'weighted_mean', weights = lambda_resampled_pop_wash)
dist_shp$S_Uni<- exact_extract(x= raster("X:/Spatial Stat/WASH Cholera/LMIC.WASH/data/means/S_UNIMP_.tifGeoTiff.gri"), y= dist_shp, 'weighted_mean', weights = lambda_resampled_pop_wash)

# hotspot ---------------------------------------------------------------------
#high cholera raster
high_cholera_incidence<- raster::calc(
  cholera_incidence_in_1000, function(x) {
    ifelse(x>1, x, NA)
  }
) 

# aggregate population to match with incidence raster
#ag_fact<- res(cholera_incidence_in_1000)/res(meanPop)
#pop_ag<- raster::aggregate(meanPop, fact= ag_fact)
#area_raster<- raster::area(pop_ag)
#pop_ag<- area_raster*pop_ag
#pop_ag<- exactextractr::exact_resample(pop_ag, cholera_incidence_in_1000, fun= 'mean')
# high risk population 
high_risk_pop<- raster::mask(
  lambda_resampled_pop, mask = high_cholera_incidence
)

# high risk population by district
dist_shp$high_risk_pop_dist<- exactextractr::exact_extract(high_risk_pop, dist_shp, 'sum')

#population by district
dist_shp$pop_dist<- exactextractr::exact_extract(lambda_resampled_pop, dist_shp, 'sum')

# hotspot districts
dist_shp$hotspot<- ifelse((dist_shp$high_risk_pop_dist/dist_shp$pop_dist>.1 | dist_shp$high_risk_pop_dist>100000) , 1, 0)

#saving as shapefile ----------------------------------------------------------
write_sf(dist_shp, paste0("X:/Spatial Stat/WASH Cholera/new_data/", "district_wash_incidence_hotspot.shp"))
dist_df<- as.data.frame(dist_shp)
dist_df$CC_2<- NULL
dist_df$HASC_2<- NULL
dist_df$geometry<- NULL
dist_df<- dist_df[complete.cases(dist_df),]
saveRDS(dist_df, "X:/Spatial Stat/WASH Cholera/new_data/dist_df.rds")

dist_df<- readRDS("X:/Spatial Stat/WASH Cholera/new_data/dist_df.rds")

for (i in 10:17) {
  summ<- summary(dist_df[,i])
  print(c(names(dist_df[i]), round(summ, 2)))
}


# plotting --------------------------------------------------------------------
theme_set(theme_minimal()+ theme(legend.position = "bottom"))
options(ggplot2.continuous.colour="viridis")

dist_df_l<- dist_df%>%pivot_longer(cols = starts_with(c("w_", "s_")))


dist_df_l$fact<- ifelse(dist_df_l$name== "S_OD" | dist_df_l$name== "S_Uni" | dist_df_l$name== "W_Sur" | dist_df_l$name== "W_Uni", "Risk Factor", "Protective Factor")
dist_df_l$name<- factor(dist_df_l$name, 
                           levels = c("S_Imp", "S_Pip", "S_OD", "S_Uni", "W_Imp", "W_Pip", "W_Sur", "W_Uni"), 
                           labels = c("Improved Sanitation", "Piped Sanitation", "Open Defecation", "Unimproved Sanitation", "Improved Water", "Piped Water", "Surface Water", "Unimproved Water"))


ggplot(data= dist_df_l, aes(x= log10(incidence_in_thousan), y= value, color= fact))+
  geom_point(alpha= .2, size=.7)+
  geom_smooth(method = lm, se= F, fullrange= T)+ 
  scale_color_viridis_d()+
  facet_wrap(~name, nrow= 2)+
  theme(legend.key = element_rect(fill = "white", colour = "white"), legend.title = element_blank())+
  ylab("Access/reliance (%)")+ 
  xlab(expression(paste(log[10], " of annual incidence in 1000 people")))+ 
  ggpubr::stat_cor(p.accuracy = 0.01, r.accuracy = 0.01, label.x = -4, label.y = 105, size= 3, color= "black")

ggplot(data= dist_df_l, aes(x= log10(incidence_in_thousan), y= value, color= fact))+
  geom_point(alpha= .1, size=.7)+
  geom_smooth(method = lm, se= F, fullrange= T,  color= "#d1495b", size= .6)+ 
  scale_color_manual(values = c("#00798c", "#edae49"))+
  facet_wrap(~name, nrow= 2)+
  theme(legend.key = element_rect(fill = "white", colour = "white"), legend.title = element_blank())+
  ylab("Access/reliance (%)")+ 
  xlab(expression(paste(log[10], " of annual incidence in 1000 people")))+ 
  ggpubr::stat_cor(p.accuracy = 0.01, r.accuracy = 0.01, label.x = -4, label.y = 105, size= 3, color= "black")


ggsave("dist_8wash.jpg", width = 6.5, height = 4, units = "in", dpi= 600)


#correlation at country level 
corSigByCountry<- dist_df%>%
  summarise(
    cor_imWat= cor.test(log10(incidence_in_thousan), W_Imp)$est,
    sig_imWat= cor.test(log10(incidence_in_thousan), W_Imp)$p.value,
    
    cor_pipWat= cor.test(log10(incidence_in_thousan), W_Pip)$est,
    sig_pipWat= cor.test(log10(incidence_in_thousan), W_Pip)$p.value,
    
    #cor_imWatO= cor.test(log10(incidence_in_thousan), W_ImpOth)$est,
    #sig_imWatO= cor.test(log10(incidence_in_thousan), W_ImpOth)$p.value,
    
    cor_imSan= cor.test(log10(incidence_in_thousan), S_Imp)$est,
    sig_imSan= cor.test(log10(incidence_in_thousan), S_Imp)$p.value,
    
    #cor_imSanO= cor.test(log10(incidence_in_thousan), S_ImpOth)$est,
    #sig_imSanO= cor.test(log10(incidence_in_thousan), S_ImpOth)$p.value,
    
    cor_imSanP= cor.test(log10(incidence_in_thousan), S_Pip)$est,
    sig_imSanP= cor.test(log10(incidence_in_thousan), S_Pip)$p.value,
    
    cor_unWat= cor.test(log10(incidence_in_thousan), W_Uni)$est,
    sig_unWat= cor.test(log10(incidence_in_thousan), W_Uni)$p.value,
    
    cor_surWat= cor.test(log10(incidence_in_thousan), W_Sur)$est,
    sig_surWat= cor.test(log10(incidence_in_thousan), W_Sur)$p.value,
    
    
    cor_unSan= cor.test(log10(incidence_in_thousan), S_Uni)$est, 
    sig_unSan= cor.test(log10(incidence_in_thousan), S_Uni)$p.value,
    
    cor_odef= cor.test(log10(incidence_in_thousan), S_OD)$est, 
    sig_odef= cor.test(log10(incidence_in_thousan), S_OD)$p.value
    
  )


#---------------------------------------
#data summary
View(dist_df%>%
       group_by(NAME_0)%>%
       summarise(inc_mean= MeanCI(incidence_in_thousan)[1], 
                 lwr.ci= MeanCI(incidence_in_thousan)[2], 
                 upr.ci= MeanCI(incidence_in_thousan)[3]))


library(boot)
fc<- function(d, i){
  d2<- d[i,]
  return(weighted.mean(d2$incidence_in_thousan, d2$pop_dist))
}
weighted.mean.boot.Sierra <- boot(dist_df[dist_df$NAME_0=="Sierra Leone", ], statistic=fc, R=5000)
weighted.ci.Sierra <- boot.ci(weighted.mean.boot.Sierra, conf=0.95, type="bca")

weighted.mean.boot.Gabon<- boot(dist_df[dist_df$NAME_0=="Gabon", ], statistic=fc, R=5000)
weighted.ci.Gabon<- boot.ci(weighted.mean.boot.Gabon, conf=0.95, type="bca")


#---------------------------------------------- DISTRICT ----------------------------------------
#correlation at district level 
