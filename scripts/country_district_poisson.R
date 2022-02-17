library(raster)
library(exactextractr)
library(sf)
library(lme4)
library(gee)
library(data.table)
library(tidyrules)
library(dplyr)
library(ggplot2)
library(data.table)
library(MASS)
library(AER)

theme_set(theme_minimal())
theme_update(legend.position = "bottom")
options(ggplot2.continuous.colour="viridis")
options(ggplot2.continuous.fill = "viridis")


# ----------------country level -----------------------
# Poisson regression: 
# afro_2010-2016_lambda_mean.gri = mean annual incidence from 2010-16
cholera_incidence<- raster(
  "X:/Spatial Stat/WASH Cholera/cholera_incidence_raster_2010-2016/incidence/afro_2010-2016_lambda_mean.gri")
meanPop<- raster("X:/Spatial Stat/WASH Cholera/WorldPop/pop_count/meanPop_count.tifGeoTiff.gri")
ctry_p<- st_read("X:/Spatial Stat/WASH Cholera/new_data/country.shp")
ctry_0<- st_read("X:/Spatial Stat/WASH Cholera/new_data/country_wash_incidence.shp")
ctry_p$mean_incidence_sum<- exact_extract(x= cholera_incidence, y= ctry_0, 'sum')
ctry_p$mean_pop_sum<- exact_extract(x= meanPop, y= ctry_0, 'sum')
ctry_p$total_cases<- ctry_p$mean_incidence_sum*ctry_p$mean_pop_sum

ctry_0.df<- as.data.frame(ctry_0)
ctry_p.df<- as.data.frame(ctry_p)

ctry_p<- merge(ctry_p.df, ctry_0.df, by= "NAME_0")
ctry_p$total_cases.int<- as.integer(ctry_p$total_cases)

# save and read country data
# saveRDS(ctry_p, "X:/Spatial Stat/WASH Cholera/new_data/country.data.RDS")
# ctry_p<- readRDS("X:/Spatial Stat/WASH Cholera/new_data/country.data.rds")





summary(glm(total_cases.int~W_Imp, data = ctry_p, family = "quasipoisson", offset = log(mean_pop_sum)))
summary(glm(total_cases.int~W_Pip, data = ctry_p, family = "quasipoisson", offset = log(mean_pop_sum)))
summary(glm(total_cases.int~S_Imp, data = ctry_p, family = "quasipoisson", offset = log(mean_pop_sum)))
summary(glm(total_cases.int~S_Pip, data = ctry_p, family = "quasipoisson", offset = log(mean_pop_sum)))

summary(glm(total_cases.int~W_Sur, data = ctry_p, family = "quasipoisson", offset = log(mean_pop_sum)))
summary(glm(total_cases.int~W_Uni, data = ctry_p, family = "quasipoisson", offset = log(mean_pop_sum)))
summary(glm(total_cases.int~S_Uni, data = ctry_p, family = "quasipoisson", offset = log(mean_pop_sum)))
summary(glm(total_cases.int~S_OD, data = ctry_p, family = "quasipoisson", offset = log(mean_pop_sum)))

convert_to_CI<- function(est, stde){
  e<- round((1- exp(est))*100, 2)
  ci<- c(round((1-exp(est - 1.96*stde))*100, 2), round((1- exp(est + 1.96*stde))*100, 2))
  paste(e, ci)
}

convert_to_CI_2<- function(data, var){
  res<- c(
    round((1-exp(coef(glm(data[,"total_cases.int"]~data[,var], data = data, family = "quasipoisson", offset = log(mean_pop_sum)))[2]))*100, 2), 
    suppressMessages(round((1-exp(confint(glm(data[,"total_cases.int"]~data[,var], data = data, family = "quasipoisson", offset = log(mean_pop_sum)))[2,]))*100, 2)))
  print(res)
}
country_result<- NULL

for (i in c(8, 10:13, 15:17)) { # check if the numbers are correct after reading ctry_p to make sure only eight WASH variables were selected
  tmp <- c(print(names(ctry_p)[i]), convert_to_CI_2(ctry_p, i))
  country_result <- rbind(country_result, tmp)
}

# without substructing 1
convert_to_CI_3<- function(data, var){
  res<- c(
    round((exp(coef(glm(data[,"total_cases.int"]~data[,var], data = data, family = "quasipoisson", offset = log(mean_pop_sum)))[2])), 2), 
    suppressMessages(round((exp(confint(glm(data[,"total_cases.int"]~data[,var], data = data, family = "quasipoisson", offset = log(mean_pop_sum)))[2,])), 2)))
  print(res)
}
country_result2<- NULL

for (i in c(8, 10:13, 15:17)) { # check if the numbers are correct after reading ctry_p to make sure only eight WASH variables were selected
  tmp <- c(print(names(ctry_p)[i]), convert_to_CI_3(ctry_p, i))
  country_result2 <- rbind(country_result2, tmp)
}

country_result<- country_result2

country_result<- as.data.frame(country_result)
#country_result<- country_result[-c(2,7), ]
country_result<- country_result[, c(1, as.numeric(2:4))]
colnames(country_result)<- c("var", "est", "upper", "lower")
country_result<- transform(country_result, est= as.numeric(est), upper= as.numeric(upper), lower= as.numeric(lower))

ggplot(data = country_result, aes(x= as.numeric(est), y=var))+geom_pointrange(aes(xmin = as.numeric(upper), xmax = as.numeric(lower)))


summary(glm(total_cases~W_Imp+W_Sur+W_Uni+W_Pip+S_Imp+S_Uni+S_Pip+S_OD, data = ctry_p, family = "quasipoisson", offset = log(mean_pop_sum)))



# ------------------------------- District level ------------------------------------------

cholera_incidence<- raster(
  "X:/Spatial Stat/WASH Cholera/cholera_incidence_raster_2010-2016/incidence/afro_2010-2016_lambda_mean.gri")
dist_p<- st_read("X:/Spatial Stat/WASH Cholera/new_data/dist_shp.shp")
dist_df<- readRDS("X:/Spatial Stat/WASH Cholera/new_data/dist_df.rds")

dist_p$mean_incidence_sum<- exact_extract(x= cholera_incidence, y= dist_p, 'sum')
dist_p$mean_pop_sum<- exact_extract(x= meanPop, y= dist_p, 'sum')
dist_p$total_cases<- dist_p$mean_incidence_sum*dist_p$mean_pop_sum


dist_df<- as.data.frame(dist_df)
dist_p<- as.data.frame(dist_p)
dist_p<- subset(dist_p, select= -geometry)

dist_p<- merge(dist_p, dist_df, by= "NAME_2")
dist_p$total_cases.int<- as.integer(dist_p$total_cases)
dist_p$mean_pop_sum<- dist_p$mean_pop_sum+0.5



summary(glmer(total_cases.int~ I(W_Imp/100)+ (1 | NAME_0.x), data = dist_p , family = poisson(link = "log"), nAGQ = 100, offset=log(mean_pop_sum) ))
summary(glmer(total_cases.int~ I(W_Pip/100)+ (1 | NAME_0.x), data = dist_p , family = poisson(link = "log"), nAGQ = 100, offset=log(mean_pop_sum) ))
summary(glmer(total_cases.int~ I(S_Imp/100)+ (1 | NAME_0.x), data = dist_p , family = poisson(link = "log"), nAGQ = 100, offset=log(mean_pop_sum) ))
summary(glmer(total_cases.int~ I(S_Pip/100)+ (1 | NAME_0.x), data = dist_p , family = poisson(link = "log"), nAGQ = 100, offset=log(mean_pop_sum) ))
summary(glmer(total_cases.int~ I(W_Sur/100)+ (1 | NAME_0.x), data = dist_p , family = poisson(link = "log"), nAGQ = 100, offset=log(mean_pop_sum) ))
summary(glmer(total_cases.int~ I(W_Uni/100)+ (1 | NAME_0.x), data = dist_p , family = poisson(link = "log"), nAGQ = 100, offset=log(mean_pop_sum) ))
summary(glmer(total_cases.int~ I(S_Uni/100)+ (1 | NAME_0.x), data = dist_p , family = poisson(link = "log"), nAGQ = 100, offset=log(mean_pop_sum) ))
summary(glmer(total_cases.int~ I(S_OD/100)+ (1 | NAME_0.x), data = dist_p , family = poisson(link = "log"), nAGQ = 100, offset=log(mean_pop_sum) ))


#GEE
summary(gee(dist_p[,"total_cases.int"]~ I(dist_p[,"W_Uni"]/100), data = dist_p , id= as.factor(NAME_0.x),  family = poisson, silent = T))$coefficients



convert_to_CI_dist<- function(est, stde){
  e<- round((1- exp(est))*100, 4)
  ci<- cbind(upper= round((1-exp(est - 1.96*stde))*100, 4), lower= round((1- exp(est + 1.96*stde))*100, 4))
  cbind.data.frame(e, ci)
}

convert_to_CI_dist(summary(gee(dist_p[,"total_cases.int"]~ I(dist_p[,25]/100), data = dist_p , id= as.factor(NAME_0.x),  family = poisson, silent = T))$coefficients[2,1][1], summary(gee(dist_p[,"total_cases.int"]~ I(dist_p[,25]/100), data = dist_p , id= as.factor(NAME_0.x),  family = poisson, silent = T))$coefficients[2,4][1])

dist_result<- NULL

for (i in 25:32) { 
  temp2<- cbind(names(dist_p)[i], convert_to_CI_dist(summary(gee(dist_p[,"total_cases.int"]~ dist_p[,i], data = dist_p , id= as.factor(NAME_0.x),  family = poisson, silent = T))$coefficients[2,1][1], 
                   summary(gee(dist_p[,"total_cases.int"]~ dist_p[,i], data = dist_p , id= as.factor(NAME_0.x),  family = poisson, silent = T))$coefficients[2,4][1]))
  dist_result <- rbind(dist_result, temp2)
}

# removing *100 and subtract 1
convert_to_CI_dist<- function(est, stde){
  e<- round((exp(est)), 4)
  ci<- cbind(upper= round((exp(est - 1.96*stde)), 4), lower= round((exp(est + 1.96*stde)), 4))
  cbind.data.frame(e, ci)
}

dist_result<- NULL

for (i in 25:32) { 
  temp2<- cbind(names(dist_p)[i], convert_to_CI_dist(summary(gee(dist_p[,"total_cases.int"]~ dist_p[,i], data = dist_p , id= as.factor(NAME_0.x),  family = poisson, silent = T))$coefficients[2,1][1], 
                                                     summary(gee(dist_p[,"total_cases.int"]~ dist_p[,i], data = dist_p , id= as.factor(NAME_0.x),  family = poisson, silent = T))$coefficients[2,4][1]))
  dist_result <- rbind(dist_result, temp2)
}





dist_result<- as.data.frame(dist_result)

colnames(dist_result)<- c("var", "est", "upper", "lower")

ggplot(data = dist_result, aes(x= as.numeric(est), y=var))+geom_pointrange(aes(xmin = as.numeric(upper), xmax = as.numeric(lower)))


ggplot(data = dist_result, aes(x= as.numeric(est), y=var))+geom_pointrange(aes(xmin = as.numeric(upper), xmax = as.numeric(lower)))+geom_errorbar(data= country_result, aes(x= as.numeric(est), y=var, xmin = as.numeric(upper), xmax = as.numeric(lower)), color= 'red', alpha= .5)

# ggplot(data = dist_result, aes(x= as.numeric(est), y=var))+
#   geom_errorbar(aes(xmin = as.numeric(upper), xmax = as.numeric(lower), color= "Country"), size= 1.5, width= .5, show.legend = T)+
#   geom_point(aes(x= as.numeric(est), y=var), size= 3, color= "#2a9d8f")+
#   geom_errorbar(data= country_result, aes(x= as.numeric(est), y=var, xmin = as.numeric(upper), xmax = as.numeric(lower), color= "District"), alpha= .8, size= 1.5, show.legend = T, width= .5)+
#   scale_color_manual(values = c("#2a9d8f", '#f9c74f'))+
#   geom_point(data= country_result, aes(x= as.numeric(est), y=var), size= 3, color= "#f9c74f")+
#   theme(panel.grid.major.y =  element_blank(), legend.title = element_blank())+
#   xlab("Effect size")+ylab("")


# ----------- multivariate model - District leve
# Protective factor 
summary(glm(total_cases~W_Imp+S_Pip, data = ctry_p, family = "quasipoisson", offset = log(mean_pop_sum)))

# Risk factor
summary(glm(total_cases~W_Uni+S_OD, data = ctry_p, family = "quasipoisson", offset = log(mean_pop_sum)))



# combine two data sets
country_result_2<- cbind(country_result, source=rep("Country-level", 8))
rownames(country_result_2)<- NULL
dist_result_2<- cbind(dist_result, source= rep("Second-level", 8))

com_df<- full_join(country_result_2, dist_result_2)
com_df$var<- factor(com_df$var, 
                        levels = c("W_Imp", "W_Pip", "S_Imp", "S_Pip", "W_Uni", "W_Sur", "S_Uni", "S_OD"), 
                        labels = c("Improved Water", "Piped Water", "Improved Sanitation", "Piped Sanitation", "Unimproved Water", "Surface Water" , "Unimproved Sanitation", "Open Defecation"))

### subtructing 1 from protective factors
# without data table com_df[grep("Improved|Piped", com_df$var), ]
com_df<- data.table(com_df)

com_df[var %like% "Improved|Piped", est := 1- est]




ggplot(data = com_df, aes(x= est, y=var, color= source))+
  geom_vline(xintercept = 1)+
  geom_errorbar(aes(xmin = upper, xmax = lower), size= 1.3, width= .5, show.legend = T, position=position_dodge(width=0.5))+
  geom_point(aes(x= as.numeric(est), y=var), size= 3, position=position_dodge(width=0.5))+ 
  scale_color_manual(values = c("#2a9d8f", '#f9c74f'))+
  theme(panel.grid.major.y =  element_blank(),legend.title = element_blank())+
  xlab("Risk Ratio")+ylab("") + 
  #annotate("text", x= c(0.95, 1.05), y= c(8.5, 8.5), label= c("Increase of incidence", "Decrease of incidence"), size= 3)+
  scale_y_discrete(limits= rev(levels(com_df$var)))
  
ggsave("Poisson results.jpg", width = 6, height = 4, units = "in", dpi= 400)

#--------------------- Table S2 ----------------------
View(com_df%>%mutate_if(is.numeric, round, 1)) # results saved in new_data folder Excel








