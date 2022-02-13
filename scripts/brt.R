library(caret)
library(gbm)
library(Metrics)

# Incidence model with full data

ctrl <- trainControl(method="cv", number=10)

brt.inc<- train(
  log_incidence~.,
  data = incidence_df, distribution="gaussian",
  method= 'gbm',  
  trControl = ctrl, 
  verbose= F
)

varImp(brt.inc, scale = T)
plot(varImp(brt.inc, scale = T))



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

gbm.perf(gbm.inc, method = "cv")

# find index for number trees with minimum CV error
best <- which.min(gbm.inc$cv.error)

# get MSE and compute RMSE
sqrt(gbm.inc$cv.error[best])


####### fold as adm0
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

(sum(unlist(lapply(gbm.list, function(x) (x$rmse)^2)))/10)^.5





