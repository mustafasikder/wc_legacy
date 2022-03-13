# purpose: to determine the optimal cutpoint of the hotspot ROC curve 

library(cutpointr)
library(Rcpp)
library(plotly)
library(data.table)

rf_hp_full.df<- readRDS("X:/Spatial Stat/WASH Cholera/clean_repo/results/rf_hp_full.df.RDS")
rf_hp_full.df$Predicted.f<- as.numeric(as.character(rf_hp_full.df$Predicted.f))
rf_hp_full.df$Observed.f<- as.numeric(as.character(rf_hp_full.df$Observed.f))

rf_hp_full.dt<- as.data.table(rf_hp_full.df)

p<- rf_hp_full.dt[Observed.f==1, .N]
n<- rf_hp_full.dt[Observed.f==0, .N]
threshold<- seq(0,1, by=.01)
roc.dt<- data.table()
for(i in threshold){
  tpr<- rf_hp_full.dt[vote.1.f>i & Observed.f==1, .N]/p
  fpr<- rf_hp_full.dt[vote.1.f>i & Observed.f==0, .N]/n
  roc.dt<- rbind(roc.dt, cbind(i, tpr, fpr, j= tpr + ((1-fpr)-1)))
}
roc.dt<- round(roc.dt, 2)
ggplotly(ggplot(data= roc.dt, aes(x= fpr, y= tpr, text= paste("cutoff:",i, "\nYouden's J:",j  )))+geom_line()+geom_point())
# since 0;06 was the max youden matrix
# so failed to detect
1-rf_hp_full.dt[vote.1.f>.06 & Observed.f==1, .N]/p
# falsely identified 
rf_hp_full.dt[vote.1.f>.06 & Observed.f==0, .N]/n


#cutp<- cutpointr::cutpointr(rf_hp_full.df, Predicted.f, Observed.f)

#### optimal cutpoint in the GBM data
gbm_hp_full.df<- readRDS("X:/Spatial Stat/WASH Cholera/clean_repo/results/gbm_hp_full.df.RDS")
head(gbm_hp_full.df)
gbm_hp_full.df<- as.data.table(gbm_hp_full.df)

p.gbm<- gbm_hp_full.df[Observed.f==1, .N]
n.gbm<- gbm_hp_full.df[Observed.f==0, .N]

threshold<- seq(0,1, by=.01)
roc.dt.gbm<- data.table()
for(i in threshold){
  tpr.gbm<- gbm_hp_full.df[vote.1.f>i & Observed.f==1, .N]/p.gbm
  fpr.gbm<- gbm_hp_full.df[vote.1.f>i & Observed.f==0, .N]/n.gbm
  roc.dt.gbm<- rbind(roc.dt.gbm, cbind(i, tpr.gbm, fpr.gbm, j= tpr.gbm + ((1-fpr.gbm)-1)))
}

roc.dt.gbm<- round(roc.dt.gbm, 2)
ggplotly(ggplot(data= roc.dt.gbm, aes(x= fpr.gbm, y= tpr.gbm, text= paste("cutoff:",i, "\nYouden's J:",j  )))+geom_line()+geom_point())
# since 0.06 was the max youden matrix [0.07 also had same score]
# so failed to detect
1-gbm_hp_full.df[vote.1.f>.06 & Observed.f==1, .N]/p.gbm
# falsely identified 
gbm_hp_full.df[vote.1.f>.06 & Observed.f==0, .N]/n.gbm

