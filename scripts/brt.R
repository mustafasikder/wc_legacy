library(caret)

# Incidence model with full data

ctrl <- trainControl(method="cv", number=10)

brt.inc<- train(
  log_incidence~.,
  data = incidence_df, distribution="gaussian",
  method= 'gbm',  
  trControl = ctrl
)

varImp(rfFit.h, scale = T)
plot(varImp(rfFit.h, scale = T))

