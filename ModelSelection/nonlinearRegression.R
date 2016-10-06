nonlinear_regression<-function(features,target,trackingState){
  # Identify the algorithm with lowest RMSE for non-linear regression.
  #
  # Args:
  #	 features: the input feature of the data
  #	 target: the target of classification problem
  #  trackingState: boolean, when the value is true, test results from selected 
  #                 algorithms will be displayed; its default value is false
  # Returns:
  #  the algorithm that offers the lowest or very closed to the lowest RMSE  
  
  set.seed(0)
  
  trainY<-target
  trainX<-features
  
  if(missing(trackingState)){
    trackingState=F
  }
  
  indx <- createFolds(trainY, returnTrain = TRUE)
  ctrl <- trainControl(method = "boot", index = indx)
  
  names<-c('SVM','MARS','Model Tree','Rule Based Model','Random Forest','Boosting','Min')
  value<-rep(Inf,length(names))
  result<-data.frame(names,value)
  result$names<-as.character(result$names)
  
  ############################non-linear regression############################
  #MARS
  marsTune <- train(x = trainX, y = trainY,
                    method = "earth",
                    #tuneGrid = expand.grid(degree = 1, nprune = 2:10),
                    trControl = ctrl)
  Result_MARS<-min(marsTune$results$RMSE)
  result[2,2]<-Result_MARS
  if(trackingState){
    cat ('Try MARS with RMSE ',Result_MARS,'/n')}
  
  #SVM with RBF kernel
  svmTune <- train(x = trainX, y = trainY,
                   method = "svmRadial",                 
                   trControl = ctrl)
  Result_SVM<-min(svmTune $results$RMSE)
  result[1,2]<-Result_SVM
  if(trackingState){
    cat ('Try SVM(RBF) with RMSE ',Result_SVM,'/n')}
  
  if (Result_SVM<Result_MARS){
    
    #interpretable tree model
    #Model Tree
    m5Tune <- train(x = trainX, y = trainY,
                    method = "M5",
                    trControl = ctrl,
                    control = Weka_control(M = 10))  
    Result_m5<-min(m5Tune$results$RMSE) 
    if(trackingState){
     cat ('Try Model Tree with RMSE ',Result_m5,'/n')}
    result[3,2]<-Result_m5
    
    
    #non-interpretable tree model
    #Random forest
    mtryGrid <- data.frame(mtry = floor(seq(10, ncol(trainX), length = 10)))
    rfTune <- train(x = trainX, y = trainY,
                    method = "rf",
                    ntree = 1000,
                    importance = TRUE,
                    trControl = ctrl)
    Result_rf<-min(rfTune$results$RMSE)
    if(trackingState){
      cat ('Try Random forest with RMSE ',Result_rf,'\n')}
    result[5,2]<-Result_rf
    #boosting
    gbmGrid <- expand.grid(interaction.depth = seq(1, 7, by = 2),
                           n.trees = seq(100, 1000, by = 50),
                           shrinkage = c(0.01, 0.1),
                           n.minobsinnode=10)
    gbmTune <- train(x = trainX, y = trainY,
                     method = "gbm",
                     trControl = ctrl,
                     verbose = FALSE)
    Result_gbm<-min(gbmTune$results$RMSE)
    if(trackingState){
      cat ('Try Boosting with RMSE ',Result_gbm,'\n')}
    result[6,2]<-Result_gbm
    
    
    cat('Select ',result[result$value== min(result$value),]$name,
        'with RMSE ',result[result$value== min(result$value),]$value,'\n')
  
  
  
  }else { 
    cat("Select MARS with RMSE ",Result_MARS,'\n') 
  }

}