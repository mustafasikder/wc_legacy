setwd("X:/Spatial Stat/WASH Cholera/new_data")
#setwd("X:/Spatial Stat/WASH Cholera/clean_repo") # will change to this repo once the script is clean

library(randomForest)
library(permimp)
library(tidyverse)
library(plotROC)
library(pROC)
library(ROSE)
library(data.table)
library(viridis)
library(ggthemes)
library()

theme_set(theme_minimal())
theme_update(legend.position = "bottom")
options(ggplot2.continuous.colour="viridis")
options(ggplot2.continuous.fill = "viridis")


dist_df<- readRDS("results/dist_df.rds")
dist_df$log_incidence<- log10(dist_df$incidence_in_thousan)
# colnames(dist_df)[10:17]<- c("Improved Water",  
#                              "Piped Water",  
#                              "Surface Water", 
#                              "Unimproved Water", 
#                              "Improved Sanitation",
#                              "Open Defecation",
#                              "Piped Sanitation", 
#                              "Unimproved Sanitation")

dist_df<- dist_df[complete.cases(dist_df), ]

###############################################################################
################ incidence model with 10 fold cross-validation ################
###############################################################################
# from https://stats.stackexchange.com/questions/61090/how-to-split-a-data-set-to-do-10-fold-cross-validation

incidence_df<- dist_df[,c(10:17, 21)]
set.seed(123)
cv_df<- incidence_df[sample(nrow(incidence_df)),]
folds <- cut(seq(1,nrow(cv_df)),breaks=10,labels=FALSE)

test.list<- list()
train.list<- list()
rf.list<- list()

for(i in 1:10){
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- cv_df[testIndexes, ]
  trainData <- cv_df[-testIndexes, ]
  rf<- randomForest(
    log_incidence~.,
    data = trainData,
    ntree=500,
    #  mtry= 3,
    #  sampsize= 500,
    localImp= TRUE, 
    keep.forest= TRUE, 
    keep.inbag= TRUE
    )
  rf.list[[i]]<- rf
  rf.list[[i]]$pred.newdata<- predict(rf, newdata = testData)
  rf.list[[i]]$testdata<- testData
  rf.list[[i]]$rmse<- mean((predict(rf, newdata = testData)-testData$log_incidence)^2)^0.5
}

# get rmse
for (i in 1:10) {print( paste0("model", i,": ",rf.list[[i]]$rmse))}

# Spearman's rho in training data
cor.test(rf.list[[6]]$predicted, rf.list[[6]]$y, method = "spearman")

# Spearman's rho in test data
cor.test(rf.list[[6]]$pred.newdata, rf.list[[6]]$testdata$log_incidence, method = "spearman")
# to get the Spearman's rho for the full data
all.pred<- cbind(unlist(lapply(rf.list, function(x) x$pred.newdata)))
all.log_incidence<- cbind(unlist(lapply(rf.list, function(x) x$testdata$log_incidence)))

pred_inc<- lapply(rf.list, function(x) cbind("pred"=x$pred.newdata, "incidence"= x$testdata$log_incidence))
pred_inc_mat<- do.call(rbind, pred_inc)
cor.test(pred_inc_mat[,1], pred_inc_mat[,2], method = "spearman")



incidence_predict<- data.table()
full.incidence.data<- data.table()
for (i in 1:length(rf.list)){
  incidence_predict<- data.table() # since the lengths are different 
  temp<- incidence_predict[ , c("cv.number", "Predicted", "Observed") := 
                              list(rep(paste0("cv.", i), length(rf.list[[i]]$pred.newdata)), 
                                   rf.list[[i]]$pred.newdata, rf.list[[i]]$testdata$log_incidence),]
  full.incidence.data<- rbind(full.incidence.data, temp)
}



# ----------------- Random forest with incidence data --------
# rf_fit_randomforest<- randomForest(
#   log_incidence~.,
#   data = model_df,
#   ntree=500,
# #  mtry= 3,
#   #sampsize= 500,
#   localImp= TRUE, 
#   keep.forest= TRUE, 
#   keep.inbag= TRUE
# )

# RMSE
# rmse<- sqrt(mean((rf_fit_randomforest$y - rf_fit_randomforest$predicted)^2))

# rf_df<- as.data.frame(cbind("Predicted"=rf_fit_randomforest$predicted, "Observed"=rf_fit_randomforest$y))

# Conditional Permutation Importance
# need testData to be same as the lowest RMSE model data
i<- 6 #assign i to the model # that has smallest RMSE from for (i in 1:10) {print( paste0("model", i,": ",rf.list[[i]]$rmse))}
folds <- cut(seq(1,nrow(cv_df)),breaks=10,labels=FALSE)
testIndexes <- which(folds==i,arr.ind=TRUE)
testData <- cv_df[testIndexes, ]
trainData <- cv_df[-testIndexes, ]
imp<- permimp::permimp(rf.list[[6]], conditional = T, do_check = FALSE)
# testing threshold value
imp2<- permimp::permimp(rf.list[[6]], conditional = T, threshold= .8, do_check = FALSE)
imp3<- permimp::permimp(rf.list[[6]], conditional = T, threshold= .5, do_check = FALSE)

permimp::plot.VarImp(imp, type = "bar") # use "box"

v_imp_inc<- cbind(imp$values)
rownames(v_imp_inc)<- c("Water Improved", "Water Piped", "Water Surface", "Water Unimproved", "Sanitation Improved", "Open Defecation", "Sanitation Piped", "Sanitation Unimproved")
v_imp_inc<- cbind(rownames(v_imp_inc), v_imp_inc)
v_imp_inc<- as.data.frame(v_imp_inc)
names(v_imp_inc)<- c("var", 'mean_accuracy_decrease')
v_imp_inc$mean_accuracy_decrease<- as.numeric(v_imp_inc$mean_accuracy_decrease)



main1<- ggplot(data= full.incidence.data, aes(Predicted, Observed, color= cv.number))+ 
  geom_point(size= .5, alpha= .5)+
  theme(legend.position = "none", panel.grid = element_line(linetype = 3, size = .5))+#, aspect.ratio = 1)+
  scale_x_continuous(limits = c(-5, 3), breaks = c(-4, -2, 0, 2),labels = c("0.0001", "0.01", "0.0", "100"), position = "bottom")+
  scale_y_continuous(limits = c(-5, 2), breaks = c(-4, -2, 0, 2),labels = c("0.0001", "0.01", "0.0", "100"), position = "left")+
  geom_abline(slope = 1, size= 1, alpha= .5, color= '#a44a3f')+labs(x= "Predicted mean annual incidence", y= "Observed mean annual incidence")+ 
  annotate("text", x= 1, y= 0, label= "atop (italic(R) ^ 2 == ??0.32, RMSE == ??0.95, MAE== ??0.76)", parse = TRUE, size = 2)+ 
  coord_equal()+ scale_color_tableau(palette = "Tableau 10")#scale_color_viridis(discrete=TRUE) 

mean_impurity_decrease <-
  ggplot(data = v_imp_inc,
         aes(
           mean_accuracy_decrease,
           reorder(var, mean_accuracy_decrease),
           fill = var
         )) +
  geom_bar(stat = "identity") +
  scale_fill_manual(
    values = c(
      "#264653",
      "#2a9d8f",
      "#F16745",
      "#FFC65D",
      "#f4a261",
      "#4CC3D9",
      "#93648D",
      "#457b9d"
    )
  ) + labs(y = NULL, x = "Conditional permutation importance") + theme(
    legend.title = element_blank(),
    legend.position = 'none',
    axis.text.y = element_text(size = 8), 
    axis.title.x = element_text(size= 8),
    axis.text.x = element_text(size = 6),
    #plot.background = element_rect(color = "White"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y =  element_blank()
  )

plot3<- main1+annotation_custom(ggplotGrob(mean_impurity_decrease), xmin = -1.4 , xmax = 3.5, ymin = -Inf, ymax = 0)



########################################################################
### -------------------------- Hotspot ------------------------------###
########################################################################

hotspot_df<- dist_df[,c(10:17, 20)]

# --------------- cross-validation -------------
# from https://stats.stackexchange.com/questions/61090/how-to-split-a-data-set-to-do-10-fold-cross-validation
hotspot_df<- hotspot_df[sample(nrow(hotspot_df)),]
hotspot_df$hotspot<- as.factor(hotspot_df$hotspot)
folds.h <- cut(seq(1,nrow(hotspot_df)),breaks=10,labels=FALSE)

test.list.h<- list()
train.list.h<- list()
rf.list.h<- list()


for(i in 1:10){
  testIndexes <- which(folds.h==i,arr.ind=TRUE)
  testData <- hotspot_df[testIndexes, ]
  trainData <- hotspot_df[-testIndexes, ]
  trainData<- ovun.sample(hotspot~., data=trainData,
                          N=nrow(trainData),  p= 0.5,
                          seed=1, method="both")$data
  test.list.h[[i]]<- testData
  train.list.h[[i]]<- trainData
  rf<- randomForest(
    hotspot~.,
    data = trainData,
    ntree=500,
    localImp= TRUE, 
    keep.forest= TRUE, 
    keep.inbag= TRUE
  )
  rf.list.h[[i]]<- rf
  rf.list.h[[i]]$pred.newdata<- predict(rf, newdata = testData)
  rf.list.h[[i]]$pred.newdata.vote<- predict(rf, newdata = testData, type = "vote",norm.votes=TRUE)
  rf.list.h[[i]]$testdata<- testData
  rf.list.h[[i]]$conf.matrix<- table(testData$hotspot ,rf.list.h[[i]]$pred.newdata)
}

# data preparation for the plot RoC plot
hotspot_predict<- data.table()
full.hotspot.data<- data.table()
for (i in 1:length(rf.list.h)){
  hotspot_predict<- data.table() # since the lengths are different
  temp<- hotspot_predict[ , c("cv.number", "Predicted", "Observed", "vote.0", "vote.1") := 
                              list(rep(paste0("cv.", i), length(rf.list.h[[i]]$pred.newdata)), 
                                   rf.list.h[[i]]$pred.newdata, 
                                   rf.list.h[[i]]$testdata$hotspot, 
                                   rf.list.h[[i]]$pred.newdata.vote[,1], 
                                   rf.list.h[[i]]$pred.newdata.vote[,2]),]
  full.hotspot.data<- rbind(full.hotspot.data, temp)
}

full.hotspot.data$Predicted<- as.numeric(as.character(full.hotspot.data$Predicted))
full.hotspot.data$Observed<- as.numeric(as.character(full.hotspot.data$Observed))


## AUC  
for (i in 1:10) {
  print( paste0("model", i,": ",
                auc(rf.list.h[[i]]$testdata$hotspot , rf.list.h[[i]]$pred.newdata.vote[,1], quiet= T)
                ))
}

#95% CI of AUC
ci.auc(auc(rf.list.h[[9]]$testdata$hotspot , rf.list.h[[9]]$pred.newdata.vote[,1]))


## model failed to detect % of hotspots
table(rf.list.h[[9]]$testdata$hotspot)
rf.list.h[[9]]$conf.matrix # conf.matrix for the training data
22/27 # filed to detect 81.5% of the hotspots
8/367 #falsely detected 2.2 % of the non-hotsports as hotspot
# for testdata
rf.list.h[[9]]$confusion
table(rf.list.h[[9]]$predicted)
table(rf.list.h[[9]]$y)
1/1726*100 # falsely detected o.06% of hotsports
31/1726*100 #failed to detect 1.8% hotspots


# variable importance
# model 9 had the highest RCAUC
i<- 9 
folds.h <- cut(seq(1,nrow(hotspot_df)),breaks=10,labels=FALSE)
testIndexes <- which(folds.h==i,arr.ind=TRUE)
testData <- hotspot_df[testIndexes, ]
trainData <- hotspot_df[-testIndexes, ]
imp.h<- permimp::permimp(rf.list.h[[9]], conditional = T, do_check = FALSE)
permimp::plot.VarImp(imp.h, type = "bar") # use "box"
imp.h2<- permimp::permimp(rf.list.h[[9]], conditional = T, threshold=.8,do_check = FALSE)
imp.h3<- permimp::permimp(rf.list.h[[9]], conditional = T, threshold= .5,do_check = FALSE)



v_imp_hp<- cbind(imp.h$values)
rownames(v_imp_hp)<- c("Water Improved", "Water Piped", "Water Surface", "Water Unimproved", "Sanitation Improved", "Open Defecation", "Sanitation Piped", "Sanitation Unimproved")
v_imp_hp<- cbind(rownames(v_imp_hp), v_imp_hp)
v_imp_hp<- as.data.frame(v_imp_hp)
names(v_imp_hp)<- c("var", 'mean_accuracy_decrease')
v_imp_hp$mean_accuracy_decrease<- as.numeric(v_imp_hp$mean_accuracy_decrease)




# ----------------

# hotspot_df<- dist_df[,c(10:17, 20)]
# hotspot_df$hotspot<- as.factor(hotspot_df$hotspot)
# table(hotspot_df$hotspot)
# # balanced data set with both over and under sampling
# hotspot.balanced.ou <- ovun.sample(hotspot~., data=hotspot_df,
#                                 N=nrow(hotspot_df), p=0.5, 
#                                 seed=1, method="both")$data
# table(hotspot.balanced.ou$hotspot)
# 
# 
# rf.hotspot<- randomForest(
#   hotspot~.,
#   data = hotspot.balanced.ou,
#   ntree=500,
#   #  mtry= 3,
#   #sampsize= 500,
#   localImp= TRUE, 
#   keep.forest= TRUE, 
#   keep.inbag= TRUE
# )
# 
# #
# pred_rf<- predict(rf.hotspot) # probably same as rf.hotspot$predicted
# 
# #using class weight instead of up/down sampling
# rf.hotspot.cw<- randomForest(
#   hotspot~.,
#   data = hotspot_df,
#   classwt= c(.1, .9),
#   ntree=500,
#   #  mtry= 3,
#   #sampsize= 500,
#   localImp= TRUE, 
#   keep.forest= TRUE, 
#   keep.inbag= TRUE
# )
# rf.hotspot<- rf.hotspot.cw

# imp.hp<- permimp(rf.hotspot)
# 
# rf.hotspot

# rf.hp_df<- data.frame("Predicted"=rf.hotspot$predicted, "Observed"=rf.hotspot$y)
# rf.hp_df$Predicted<- as.numeric(as.character(rf.hp_df$Predicted))
# rf.hp_df$Observed<- as.numeric(as.character(rf.hp_df$Observed))
# 
# #rf.hp_df$Predicted<- ifelse(rf.hp_df$Predicted==1, "yes", "no")
# #rf.hp_df$Observed<- ifelse(rf.hp_df$Observed==1, "yes", "no")
# rf.hp_df$yes<- rf.hotspot$votes[,1]
# rf.hp_df$no<- rf.hotspot$votes[,2]


# ---------------------

main2<- ggplot(data=full.hotspot.data, aes(m= vote.1, d= Observed, color= cv.number))+ 
  geom_roc(n.cuts=0, linealpha = .5) + 
  coord_equal()+
  theme(legend.position = 'none')+
  scale_color_tableau()+ 
  # scale_color_viridis_d()+
  labs(x= "False positive rate", y= "True positive rate")


mean_impurity_decrease.hp <-
  ggplot(data = v_imp_hp,
         aes(
           mean_accuracy_decrease,
           reorder(var, mean_accuracy_decrease),
           fill = var
         )) +
  geom_bar(stat = "identity") +
  scale_fill_manual(
    values = c(
      "#264653",
      "#2a9d8f",
      "#F16745",
      "#FFC65D",
      "#f4a261",
      "#4CC3D9",
      "#93648D",
      "#457b9d"
    )
  ) + labs(y = NULL, x = "Conditional permutation importance") + theme(
    legend.title = element_blank(),
    legend.position = 'none',
    axis.text.y = element_text(size = 8), 
    axis.title.x = element_text(size= 8),
    axis.text.x = element_text(size = 6),
    #plot.background = element_rect(color = "White"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y =  element_blank()
  )

plot4<- main2+annotation_custom(ggplotGrob(mean_impurity_decrease.hp), xmin = .3 , xmax = 1.5, ymin = -.2, ymax = .7)

plot3.v1<- ggpubr::ggarrange(plot3, plot4, ncol = 1, labels = c("A", "B"),  heights = c(1, 1))

save.image(file = "revised.model.results.RData")


# making ROC manually -----------------------------
# discrimination threshold
d<- seq(0, 1, by= 0.05)

# for detecting  hotspots 
sensetivity<- function(dis_thr){ # TP/TP+FN
  length(full.hotspot.data[cv.number=="cv.9" & Observed == 1 & vote.1>=dis_thr, ]$Predicted)/ # TP= when the vote was >= dis_thr how many were predicted as 1
    (length(full.hotspot.data[cv.number=="cv.9" & Observed == 1 & vote.1>=dis_thr, ]$Predicted) + length(full.hotspot.data[cv.number=="cv.9" & Observed == 1 & vote.1<dis_thr, ]$Predicted))
}

specificity<- function(dis_thr){ # TN/TN+ FP
  length(full.hotspot.data[cv.number=="cv.9" & vote.1<=dis_thr & Observed ==0,]$Predicted)/
    (length(full.hotspot.data[cv.number=="cv.9" & vote.1<=dis_thr & Observed ==0, ]$Predicted) + length(full.hotspot.data[cv.number=="cv.9" & vote.1>dis_thr & Observed==0,]$Predicted))
}
temp_roc<- NULL
for(i in d){
  roc1<- c("FPR"= 1-specificity(i), "TPR"= sensetivity(i) )
  temp_roc<- rbind(temp_roc, roc1)
}
ggplot(data = data.frame(temp_roc), aes(x= FPR, y= TPR))+ geom_point()

## ROC for cv 9 
ggplot(data=full.hotspot.data[cv.number=="cv.9",], aes(m= vote.1, d= Observed, color= cv.number))+
  geom_roc(n.cuts=0, linealpha = .5) +
  coord_equal()+
  theme(legend.position = 'none')+
  scale_color_tableau()+
  # scale_color_viridis_d()+
  labs(x= "False positive rate", y= "True positive rate")

# full plot with cv9 highlighted
ggplot(data=full.hotspot.data, aes(m= vote.1, d= Observed, color= cv.number))+ 
  geom_roc(n.cuts=0, linealpha = .5) + 
  coord_equal()+
  theme(legend.position = 'none', panel.grid = element_line(linetype = 3, size = .5), panel.grid.minor = element_blank())+
  scale_color_tableau(direction = -1)+ 
  # scale_color_viridis_d()+
  labs(x= "False positive rate", y= "True positive rate")+ 
  geom_roc(data = full.hotspot.data[cv.number=="cv.9",], 
           aes(m= vote.1, d= Observed), size= 1.2, 
           cutoffs.at = c(.145), # this is the discrimination threshold 
           cutoff.labels = "FPR= 0.40,     \nTPR= 0.85      ", # from sensetivity(.145) and 1- specificity(.145)
           pointsize = 1, labelsize = 2)
