library(caret)
library(gbm)
library(Metrics)
library(vip)
library(ggplot2)
library(boot)

theme_set(theme_minimal())
theme_update(legend.position = "bottom")
options(ggplot2.continuous.colour="viridis")
options(ggplot2.continuous.fill = "viridis")

dist_df<- readRDS("results/dist_df.rds")
dist_df$log_incidence<- log10(dist_df$incidence_in_thousan)
dist_df<- dist_df[complete.cases(dist_df), ]

##############################################################################
######################## Incidence model with full data ######################
##############################################################################

gbm.inc<- gbm(log_incidence~.,  
              data = incidence_df, 
              distribution="gaussian",  
              n.trees=500, 
              #interaction.depth=5, 
              #n.minobsinnode=10, 
              #shrinkage=0.01, 
              #bag.fraction=0.75, 
              #cv.folds=10, 
              verbose=T)

######################## full model importance plot  ########################## 
vip_full<- vip(gbm.inc)
v_imp_inc_gbm<- data.table(vip_full$data)
setorder(v_imp_inc_gbm, Variable)
v_imp_inc_gbm$var<- c("Sanitation Improved", "Open Defecation", "Sanitation Piped", "Sanitation Unimproved", "Water Improved", "Water Piped", "Water Surface", "Water Unimproved")


mean_impurity_decrease_gbm <-
  ggplot(data = v_imp_inc_gbm,aes( Importance, reorder(var, Importance), fill = var)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#264653", "#2a9d8f", "#F16745", "#FFC65D", "#f4a261", "#4CC3D9", "#93648D", "#457b9d" )) +
  labs(y = NULL, x = "Importance (incidence)") + theme(
    legend.title = element_blank(),
    legend.position = 'none',
    axis.text.y = element_text(size = 8), 
    axis.title.x = element_text(size= 7, face = "plain"),
    axis.text.x = element_text(size = 6),
    #plot.background = element_rect(color = "White"),
    panel.grid.major = element_blank(),
    panel.grid.minor =  element_blank(), 
    panel.grid.minor.x = element_blank())+
  scale_x_continuous(position = "top")#, limits = c(min(v_imp_inc$mean_accuracy_decrease), max(v_imp_inc$mean_accuracy_decrease)), breaks = c(as.vector(summary(v_imp_inc$mean_accuracy_decrease)[c(1,3,6)])))


############### ADM0 as folds for cvRMSE - Incidence model ####################
dist_dt<- as.data.table(dist_df)
adm0<- dist_dt[,.N,by= NAME_0]

folds2<- rep(1:nrow(adm0), adm0$N)
cv_df<- dist_df[,c(10:17, 21)]

test.list<- list()
train.list<- list()
gbm.list<- list()


for(i in 1:nrow(adm0)){
  testIndexes <- which(folds2==i,arr.ind=TRUE)
  testData <- cv_df[testIndexes, ]
  trainData <- cv_df[-testIndexes, ]
  gbm.i<- gbm(
    log_incidence~.,
    data = trainData,
    distribution="gaussian",
    n.trees=500#, 
    # localImp= TRUE, 
    # keep.forest= TRUE, 
    # keep.inbag= TRUE
  )
  gbm.list[[i]]<- gbm.i
  gbm.list[[i]]$pred.newdata<- predict(gbm.i, newdata = testData)
  gbm.list[[i]]$testdata<- testData
  gbm.list[[i]]$rmse<- mean((predict(gbm.i, newdata = testData)-testData$log_incidence)^2)^0.5
  #gbm.list[[i]]$perImp<- permimp::permimp(gbm.list[[i]], conditional = T, do_check = FALSE)
}

(sum(unlist(lapply(gbm.list, function(x) (x$rmse)^2)))/length(gbm.list))^.5


# Bootstrap for cvRMSE with 95% cI
cvRMSE<- data.frame()
for (i in 1:40) {
  cvRMSE<- rbind(cvRMSE, cbind(m_id= paste0("model", i), rmse= gbm.list[[i]]$rmse))
}
cvRMSE$rmse<- as.numeric(cvRMSE$rmse)
fc<- function(d, i){
  d2<- d[i, ]
  return((mean((d2$rmse)^2))^.5)
}

cvRMSE.mean<- boot(cvRMSE, statistic=fc, R=5000)
boot.ci(cvRMSE.mean, conf=0.95, type="bca")




##############################################################################
### ------------------- Hotspot model with full data -------------------####
##############################################################################

dist_dt<- as.data.table(dist_df)
hotspot_df<- dist_df[,c(10:17, 20)]
#hotspot_df$hotspot<- as.factor(hotspot_df$hotspot)

gbm.hp<- gbm(hotspot~.,  
              data = hotspot_df, 
              distribution="bernoulli",  
              n.trees=500, 
              verbose=T)

################## full model importance plot hotspot  ########################
vip_full_hp<- vip(gbm.hp)
v_imp_hp_gbm<- data.table(vip_full_hp$data)
setorder(v_imp_hp_gbm, Variable)
v_imp_hp_gbm$var<- c("Sanitation Improved", "Open Defecation", "Sanitation Piped", "Sanitation Unimproved", "Water Improved", "Water Piped", "Water Surface", "Water Unimproved")

mean_impurity_decrease.hp.gbm <-
  ggplot(data = v_imp_hp_gbm, aes(Importance, reorder(var, Importance), fill = var)) +
  geom_bar(stat = "identity") +
  scale_fill_manual( values = c("#264653","#2a9d8f","#F16745","#FFC65D","#f4a261","#4CC3D9","#93648D","#457b9d")) + 
  labs(y = NULL, x = "Importance (hotspot)") + theme(
    legend.title = element_blank(),
    legend.position = 'none',
    axis.text.y = element_text(size = 8), 
    axis.title.x = element_text(size= 7, face = "plain"),
    axis.text.x = element_text(size = 6),
    #plot.background = element_rect(color = "White"),
    panel.grid.major = element_blank(),
    panel.grid.minor =  element_blank(), 
    panel.grid.minor.x = element_blank())+ 
  scale_x_continuous(position = "top") #, limits = c(min(v_imp_hp$mean_accuracy_decrease), max(v_imp_hp$mean_accuracy_decrease)), breaks = c(as.vector(summary(v_imp_hp$mean_accuracy_decrease)[c(1,3,6)])))

############### ADM0 as folds for cvRMSE - Incidence model ####################

adm0<- dist_dt[,.N,by= NAME_0]
folds.h<- rep(1:nrow(adm0), adm0$N)


test.list.h<- list()
train.list.h<- list()
gbm.list.h<- list()

for(i in 1:nrow(adm0)){
  testIndexes <- which(folds.h==i,arr.ind=TRUE)
  testData <- hotspot_df[testIndexes, ]
  trainData <- hotspot_df[-testIndexes, ]
  trainData<- ovun.sample(hotspot~., data=trainData,
                          N=nrow(trainData),  p= 0.5,
                          seed=1, method="both")$data
  test.list.h[[i]]<- testData
  train.list.h[[i]]<- trainData
  gbm.hp<- gbm(
    hotspot~.,
    data = trainData,
    distribution="bernoulli",  
    n.trees=500, 
    verbose=T
  )
  gbm.list.h[[i]]<- gbm.hp
  gbm.list.h[[i]]$pred.newdata<- predict(gbm.hp, newdata = testData)
  gbm.list.h[[i]]$pred.newdata.vote<- predict(gbm.hp, newdata = testData)
  gbm.list.h[[i]]$testdata<- testData
  gbm.list.h[[i]]$conf.matrix<- table(testData$hotspot ,gbm.list.h[[i]]$pred.newdata)
}

for (i in 1:nrow(adm0)){
  print(table(gbm.list.h[[i]]$testdata$hotspot))
}




actual<- data.table()
predicted<- data.table()
for (i in 1:nrow(adm0)){
  actual<- rbind(actual, gbm.list.h[[i]]$testdata$hotspot)
  predicted<- rbind(predicted, gbm.list.h[[i]]$pred.newdata.vote)
}

auc(as.factor(actual$x), predicted$x)


# Bootstrap for cvAUC for 95% CI
auc_data<- data.frame(actual= as.factor(actual$x), predicted= predicted$x)

fc<- function(d, i){
  d2<- d[i,]
  return(auc(d2$actual, d2$predicted))
}
cvAUC_gbm<- boot(auc_data, statistic=fc, R=5000)
boot.ci(cvAUC_gbm, conf=0.95, type="bca")

## Figure S1
fig_s1<- gridExtra::grid.arrange(mean_impurity_decrease_gbm,
                                 mean_impurity_decrease.hp.gbm, 
                                 ncol= 2)

ggsave("X:/Spatial Stat/WASH Cholera/clean_repo/results/Figure_S1.jpeg", plot= fig_s1, width = 6.0, height = 2.5, units = "in", dpi= 500)


save.image("rf_and_brt_results.RData")

