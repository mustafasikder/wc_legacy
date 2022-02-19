# Incidence random forest model using (1) country (ADM0) as cross-validation fold
# and (2) the full data 
# 
# Paper input from cross-validation model:  1. cvRMSE
#                                           2. 
#                
# Paper input from full data model:         3. Figure 3A scatter plot
#                                           4. Figure 3A variable importance plot
#                                           5. Spearman's rho


rm(list= ls())
setwd("X:/Spatial Stat/WASH Cholera/clean_repo") 

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
library(gridExtra)

theme_set(theme_minimal())
theme_update(legend.position = "bottom")
options(ggplot2.continuous.colour="viridis")
options(ggplot2.continuous.fill = "viridis")

options(digits=2)


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
################ incidence model with country fold cross-validation ################
###############################################################################
# from https://stats.stackexchange.com/questions/61090/how-to-split-a-data-set-to-do-10-fold-cross-validation
# *** Note: CV model used to report cvRMSE and Spearman rho ***


dist_dt<- as.data.table(dist_df)
adm0<- dist_dt[,.N,by= NAME_0]

folds2<- rep(1:nrow(adm0), adm0$N)
cv_df<- dist_df[,c(10:17, 21)]

# incidence_df<- dist_df[,c(10:17, 21)]
# set.seed(123)
# cv_df<- incidence_df[sample(nrow(incidence_df)),]
# folds <- cut(seq(1,nrow(cv_df)),breaks=10,labels=FALSE)

test.list<- list()
train.list<- list()
rf.list<- list()

for(i in 1:nrow(adm0)){
  testIndexes <- which(folds2==i,arr.ind=TRUE)
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
  rf.list[[i]]$perImp<- permimp::permimp(rf.list[[i]], conditional = T, do_check = FALSE)
}

# get rmse
for (i in 1:40) {print( paste0("model", i,": ",rf.list[[i]]$rmse))}
# combined rmse
for (i in 1:40) {print( paste0("model", i,": ",rf.list[[i]]$rmse))}
(sum(unlist(lapply(rf.list, function(x) (x$rmse)^2)))/40)^.5
# Bootstrap for cvRMSE with 95% cI

cvRMSE<- data.frame()
for (i in 1:40) {
  cvRMSE<- rbind(cvRMSE, cbind(m_id= paste0("model", i), rmse= rf.list[[i]]$rmse))
}
cvRMSE$rmse<- as.numeric(cvRMSE$rmse)
fc<- function(d, i){
  d2<- d[i, ]
  return((mean((d2$rmse)^2))^.5)
}

boot.ci(boot(cvRMSE, statistic=fc, R=5000), conf=0.95, type="bca")




# Spearman's rho in training data
# cor.test(rf.list[[6]]$predicted, rf.list[[6]]$y, method = "spearman")

# Spearman's rho in test data
# cor.test(rf.list[[6]]$pred.newdata, rf.list[[6]]$testdata$log_incidence, method = "spearman")
# to get the Spearman's rho for the full data
all.pred<- cbind(unlist(lapply(rf.list, function(x) x$pred.newdata)))
all.log_incidence<- cbind(unlist(lapply(rf.list, function(x) x$testdata$log_incidence)))

pred_inc<- lapply(rf.list, function(x) cbind("pred"=x$pred.newdata, "incidence"= x$testdata$log_incidence))
pred_inc_mat<- do.call(rbind, pred_inc)
cor.test(pred_inc_mat[,1], pred_inc_mat[,2], method = "spearman")

# permutation variable importance 
perImp<- t(rbind(sapply(rf.list, function(x) x$perImp$values)))
fwrite(perImp, file= "results/IncidencePermutationImportance10Fold.csv")



incidence_predict<- data.table()
full.incidence.data<- data.table()
for (i in 1:length(rf.list)){
  incidence_predict<- data.table() # since the lengths are different 
  temp<- incidence_predict[ , c("cv.number", "Predicted", "Observed") := 
                              list(rep(paste0("cv.", i), length(rf.list[[i]]$pred.newdata)), 
                                   rf.list[[i]]$pred.newdata, rf.list[[i]]$testdata$log_incidence),]
  full.incidence.data<- rbind(full.incidence.data, temp)
}


# ----------------- Incidence data full model ----------------
# *** Note: full data model used to produce 1) plot 3A (scatter plot) and 2) variable permutation importance + Fig 3A (importance bar plot) ***

rf_full<- randomForest(
  log_incidence~.,
  data = incidence_df,
  ntree=500,
  #  mtry= 3,
  #  sampsize= 500,
  localImp= TRUE, 
  keep.forest= TRUE, 
  keep.inbag= TRUE
)

rf_full$oob.times

#imp_full<- permimp::permimp(rf_full, conditional = T, do_check = FALSE)
imp_full<- readRDS("results/permutationImpFullData.rds")



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
# i<- 6 #assign i to the model # that has smallest RMSE from for (i in 1:10) {print( paste0("model", i,": ",rf.list[[i]]$rmse))}
# folds <- cut(seq(1,nrow(cv_df)),breaks=10,labels=FALSE)
# testIndexes <- which(folds==i,arr.ind=TRUE)
# testData <- cv_df[testIndexes, ]
# trainData <- cv_df[-testIndexes, ]
# imp<- permimp::permimp(rf.list[[6]], conditional = T, do_check = FALSE)
# # testing threshold value
# imp2<- permimp::permimp(rf.list[[6]], conditional = T, threshold= .8, do_check = FALSE)
# imp3<- permimp::permimp(rf.list[[6]], conditional = T, threshold= .5, do_check = FALSE)

permimp::plot.VarImp(imp, type = "bar") # use "box"

#v_imp_inc<- cbind(imp$values) commented since we are now taking the scores from the full mode
v_imp_inc<- cbind(imp_full$values)
rownames(v_imp_inc)<- c("Water Improved", "Water Piped", "Water Surface", "Water Unimproved", "Sanitation Improved", "Open Defecation", "Sanitation Piped", "Sanitation Unimproved")
v_imp_inc<- cbind(rownames(v_imp_inc), v_imp_inc)
v_imp_inc<- as.data.frame(v_imp_inc)
names(v_imp_inc)<- c("var", 'mean_accuracy_decrease')
v_imp_inc$mean_accuracy_decrease<- as.numeric(v_imp_inc$mean_accuracy_decrease)



main1<- ggplot(data= full.incidence.data, aes(Predicted, Observed))+ 
  geom_point(size= .5, alpha= .5, color= 'grey40')+
  theme(legend.position = "none", panel.grid = element_line(linetype = 3, size = .5))+#, aspect.ratio = 1)+
  scale_x_continuous(limits = c(-5, 3), breaks = c(-4, -2, 0, 2),labels = c("0.0001", "0.01", "0.0", "100"), position = "bottom")+
  scale_y_continuous(limits = c(-5, 2), breaks = c(-4, -2, 0, 2),labels = c("0.0001", "0.01", "0.0", "100"), position = "left")+
  geom_abline(slope = 1, size= 1, alpha= .5, color= '#a44a3f')+labs(x= "Predicted mean annual incidence", y= "Observed mean annual incidence")+ 
  annotate("text", x= 1, y= 0, label= "atop (italic(R) ^ 2 == ??0.32, RMSE == ??0.95, MAE== ??0.76)", parse = TRUE, size = 2)+ 
  coord_equal()#+ scale_color_tableau(palette = "Tableau 10")#scale_color_viridis(discrete=TRUE) 

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
    panel.grid.major = element_blank(),
    panel.grid.minor =  element_blank()
  )+ 
  scale_x_continuous(position = "top")

plot3<- main1+annotation_custom(ggplotGrob(mean_impurity_decrease), xmin = -1.4 , xmax = 3.5, ymin = -Inf, ymax = 0)



# ########################################################################
# ### -------------------------- Hotspot ------------------------------###
# ########################################################################
# 
# #hotspot_df<- dist_df[,c(10:17, 20)]
# dist_dt<- as.data.table(dist_df)
# adm0<- dist_dt[,.N,by= NAME_0]
# 
# folds.h<- rep(1:nrow(adm0), adm0$N)
# hotspot_df<- dist_df[,c(10:17, 20)]
# 
# 
# 
# 
# # --------------- cross-validation -------------
# # from https://stats.stackexchange.com/questions/61090/how-to-split-a-data-set-to-do-10-fold-cross-validation
# #hotspot_df<- hotspot_df[sample(nrow(hotspot_df)),]
# hotspot_df$hotspot<- as.factor(hotspot_df$hotspot)
# #folds.h <- cut(seq(1,nrow(hotspot_df)),breaks=10,labels=FALSE)
# 
# test.list.h<- list()
# train.list.h<- list()
# rf.list.h<- list()
# 
# 
# for(i in 1:nrow(adm0)){
#   testIndexes <- which(folds.h==i,arr.ind=TRUE)
#   testData <- hotspot_df[testIndexes, ]
#   trainData <- hotspot_df[-testIndexes, ]
#   trainData<- ovun.sample(hotspot~., data=trainData,
#                           N=nrow(trainData),  p= 0.5,
#                           seed=1, method="both")$data
#   test.list.h[[i]]<- testData
#   train.list.h[[i]]<- trainData
#   rf<- randomForest(
#     hotspot~.,
#     data = trainData,
#     ntree=500,
#     localImp= TRUE, 
#     keep.forest= TRUE, 
#     keep.inbag= TRUE
#   )
#   rf.list.h[[i]]<- rf
#   rf.list.h[[i]]$pred.newdata<- predict(rf, newdata = testData)
#   rf.list.h[[i]]$pred.newdata.vote<- predict(rf, newdata = testData, type = "vote",norm.votes=TRUE)
#   rf.list.h[[i]]$testdata<- testData
#   rf.list.h[[i]]$conf.matrix<- table(testData$hotspot ,rf.list.h[[i]]$pred.newdata)
# }
# 
# # data preparation for the plot RoC plot
# hotspot_predict<- data.table()
# full.hotspot.data<- data.table()
# for (i in 1:length(rf.list.h)){
#   hotspot_predict<- data.table() # since the lengths are different
#   temp<- hotspot_predict[ , c("cv.number", "Predicted", "Observed", "vote.0", "vote.1") := 
#                               list(rep(paste0("cv.", i), length(rf.list.h[[i]]$pred.newdata)), 
#                                    rf.list.h[[i]]$pred.newdata, 
#                                    rf.list.h[[i]]$testdata$hotspot, 
#                                    rf.list.h[[i]]$pred.newdata.vote[,1], 
#                                    rf.list.h[[i]]$pred.newdata.vote[,2]),]
#   full.hotspot.data<- rbind(full.hotspot.data, temp)
# }
# 
# full.hotspot.data$Predicted<- as.numeric(as.character(full.hotspot.data$Predicted))
# full.hotspot.data$Observed<- as.numeric(as.character(full.hotspot.data$Observed))
# 
# 
# 
# 
# 
# ### Bootstrap for cvACU and 95% CI
# actual<- data.table()
# predicted<- data.table()
# for (i in 1:nrow(adm0)){
#   actual<- rbind(actual, rf.list.h[[i]]$testdata$hotspot)
#   predicted<- rbind(predicted, rf.list.h[[i]]$pred.newdata.vote[,2])
# }
# 
# auc(as.factor(actual$x), predicted$x)
# 
# 
# # Bootstrap for cvAUC for 95% CI
# auc_data<- data.frame(actual= as.factor(actual$x), predicted= predicted$x)
# 
# fc<- function(d, i){
#   d2<- d[i,]
#   return(auc(d2$actual, d2$predicted))
# }
# cvAUC<- boot(auc_data, statistic=fc, R=5000)
# boot.ci(cvAUC, conf=0.95, type="bca")
# 
# 
# 
# # --------------- hotspot full model ------------------
# 
# # hotspot_df_ovun<- ovun.sample(hotspot~., data=hotspot_df,
# #               p= 0.5,
# #             seed=1, method="over")$data
# # Perhaps, upsampling is not fight approach when the objective is to understand importance of the variables
# # upsampling will change the ratio of hotspot and non-hotspot that we are likely to see in real data 
# 
# 
# rf_hp_full<- randomForest(
#   hotspot~.,
#   data = hotspot_df,#_ovun,
#   ntree=500,
#   localImp= TRUE, 
#   keep.forest= TRUE, 
#   keep.inbag= TRUE
# )
# 
# 
# rf_hp_full.df<- data.frame(Predicted.f= rf_hp_full$predicted, Observed.f= rf_hp_full$y, vote.0.f= rf_hp_full$votes[,1], vote.1.f= rf_hp_full$votes[,2])
# saveRDS(rf_hp_full.df, file = "X:/Spatial Stat/WASH Cholera/clean_repo/results/rf_hp_full.df.RDS")
# rf_hp_full.df<- readRDS("X:/Spatial Stat/WASH Cholera/clean_repo/results/rf_hp_full.df.RDS")
# 
# 
# 
# #imp.h_full<- permimp::permimp(rf_hp_full, conditional = T, do_check = FALSE)
# imp.h_full<- readRDS("results/permutationImpFullData_hotspot.rds")
# 
# ## AUC  for full data model
# auc(rf_hp_full$y , rf_hp_full$votes[,1], quiet= T)
# 
# #95% CI of AUC
# ci.auc(auc(rf_hp_full$y , rf_hp_full$votes[,1], quiet= T))
# 
# 
# #### combining the two data frames for hte ROC plot
# combined_df_hp<- data.frame(full.hotspot.data, rf_hp_full.df)
# ### to make sure combined_df_hp$Observed!=combined_df_hp$Observed.f to use in the main2 plot 
# sum(combined_df_hp$Observed!=combined_df_hp$Observed.f)
# 
# 
# 
# 
# # variable importance
# 
# 
# 
# # v_imp_hp<- cbind(imp.h$values) commented since we are using full data model now 12/28/2021
# v_imp_hp<- cbind(imp.h_full$values)
# rownames(v_imp_hp)<- c("Water Improved", "Water Piped", "Water Surface", "Water Unimproved", "Sanitation Improved", "Open Defecation", "Sanitation Piped", "Sanitation Unimproved")
# v_imp_hp<- cbind(rownames(v_imp_hp), v_imp_hp)
# v_imp_hp<- as.data.frame(v_imp_hp)
# names(v_imp_hp)<- c("var", 'mean_accuracy_decrease')
# v_imp_hp$mean_accuracy_decrease<- as.numeric(v_imp_hp$mean_accuracy_decrease)
# 
# 
# 
# 
# 
# # ---------------------
# 
# main2<- ggplot(data=combined_df_hp)+ 
#   geom_roc(aes(m= vote.1, d= Observed, color= cv.number), n.cuts=0, linealpha = .3) + 
#   geom_roc(aes(m= vote.1.f, d= Observed), n.cuts=0, linealpha = 1) + 
#   coord_equal()+
#   theme(legend.position = 'none')+
#   scale_color_grey(end= 0) +
#   #scale_color_tableau()+ 
#   # scale_color_viridis_d()+
#   labs(x= "False positive rate", y= "True positive rate")
# 
# 
# mean_impurity_decrease.hp <-
#   ggplot(data = v_imp_hp,
#          aes(
#            mean_accuracy_decrease,
#            reorder(var, mean_accuracy_decrease),
#            fill = var
#          )) +
#   geom_bar(stat = "identity") +
#   scale_fill_manual(
#     values = c(
#       "#264653",
#       "#2a9d8f",
#       "#F16745",
#       "#FFC65D",
#       "#f4a261",
#       "#4CC3D9",
#       "#93648D",
#       "#457b9d"
#     )
#   ) + labs(y = NULL, x = "Conditional permutation importance") + theme(
#     legend.title = element_blank(),
#     legend.position = 'none',
#     axis.text.y = element_text(size = 8), 
#     axis.title.x = element_text(size= 8),
#     axis.text.x = element_text(size = 6),
#     #plot.background = element_rect(color = "White"),
#     panel.grid.major = element_blank(),
#     panel.grid.minor =  element_blank()
#   )+ 
#   scale_x_continuous(position = "top")
# 
# plot4<- main2+annotation_custom(ggplotGrob(mean_impurity_decrease.hp), xmin = .3 , xmax = 1.5, ymin = -.2, ymax = .7)
# 
# plot3.v1<- ggpubr::ggarrange(plot3, plot4, ncol = 1, labels = c("A", "B"),  heights = c(1, 1))
# 
# save.image(file = "revised.model.results.RData")
# 
# 
# # making ROC manually -----------------------------
# # discrimination threshold
# d<- seq(0, 1, by= 0.05)
# 
# # for detecting  hotspots 
# sensetivity<- function(dis_thr){ # TP/TP+FN
#   length(full.hotspot.data[cv.number=="cv.9" & Observed == 1 & vote.1>=dis_thr, ]$Predicted)/ # TP= when the vote was >= dis_thr how many were predicted as 1
#     (length(full.hotspot.data[cv.number=="cv.9" & Observed == 1 & vote.1>=dis_thr, ]$Predicted) + length(full.hotspot.data[cv.number=="cv.9" & Observed == 1 & vote.1<dis_thr, ]$Predicted))
# }
# 
# specificity<- function(dis_thr){ # TN/TN+ FP
#   length(full.hotspot.data[cv.number=="cv.9" & vote.1<=dis_thr & Observed ==0,]$Predicted)/
#     (length(full.hotspot.data[cv.number=="cv.9" & vote.1<=dis_thr & Observed ==0, ]$Predicted) + length(full.hotspot.data[cv.number=="cv.9" & vote.1>dis_thr & Observed==0,]$Predicted))
# }
# temp_roc<- NULL
# for(i in d){
#   roc1<- c("FPR"= 1-specificity(i), "TPR"= sensetivity(i) )
#   temp_roc<- rbind(temp_roc, roc1)
# }
# ggplot(data = data.frame(temp_roc), aes(x= FPR, y= TPR))+ geom_point()
# 
# ## ROC for cv 9 
# ggplot(data=full.hotspot.data[cv.number=="cv.9",], aes(m= vote.1, d= Observed, color= cv.number))+
#   geom_roc(n.cuts=0, linealpha = .5) +
#   coord_equal()+
#   theme(legend.position = 'none')+
#   scale_color_tableau()+
#   # scale_color_viridis_d()+
#   labs(x= "False positive rate", y= "True positive rate")
# 
# # full plot with cv9 highlighted
# ggplot(data=full.hotspot.data, aes(m= vote.1, d= Observed, color= cv.number))+ 
#   geom_roc(n.cuts=0, linealpha = .5) + 
#   coord_equal()+
#   theme(legend.position = 'none', panel.grid = element_line(linetype = 3, size = .5), panel.grid.minor = element_blank())+
#   scale_color_tableau(direction = -1)+ 
#   # scale_color_viridis_d()+
#   labs(x= "False positive rate", y= "True positive rate")+ 
#   geom_roc(data = full.hotspot.data[cv.number=="cv.9",], 
#            aes(m= vote.1, d= Observed), size= 1.2, 
#            cutoffs.at = c(.145), # this is the discrimination threshold 
#            cutoff.labels = "FPR= 0.40,     \nTPR= 0.85      ", # from sensetivity(.145) and 1- specificity(.145)
#            pointsize = 1, labelsize = 2)
# 
# 
save.image("Jan312022.RData")
