linear_regression<-function(features,target,trackingState){
  # Identify the algorithm with lowest RMSE for linear regression.
  #
  # Args:
  #	 features: the input feature of the data
  #	 target: the target of classification problem
  #  trackingState: boolean, when the value is true, test results from selected 
  #                 algorithms will be displayed; its default value is false
  # Returns:
  #  the algorithm that offers the lowest or very closed to the lowest RMSE  

  #the threshold for small and medium/large
  threshold=10
  #the threshold for correlation
  corr.thresold=2
  #the threshold for high dim
  dim.threshold=100
  
  trainY<-target
  trainX<-features
  
  if(missing(trackingState)){
    trackingState=F
  }
  
  set.seed(0)
  indx <- createFolds(trainY, returnTrain = TRUE)
  ctrl <- trainControl(method = "boot",
                       index = indx,
                       savePredictions = TRUE)
  
  ############################linear regression############################
  names<-c('SVM','PLS','Elastic Net','PCR','OLR','Lasso','Ridge','Min')
  value<-rep(Inf,length(names))
  result<-data.frame(names,value)
  result$names<-as.character(result$names)
  
  pdata <- preProcess(trainX, method="pca")
  if (corr.thresold*pdata$numComp<ncol(data)){  #highly correlated
  
      if (nrow(data)>ncol(data)){ #number of samples(N)>number of features(F)
        #PLS
        plsTune <- train(x = trainX, y = trainY,
                         method = "pls",
                         trControl = ctrl)
        Result_plsTune<-min(plsTune$results$RMSE) 
        if(trackingState){
          cat ('Try PLS with RMSE ',Result_plsTune,'\n')}
        result[2,2]<-Result_plsTune   
      }else{
        #Elastic Net
        enetGrid <- expand.grid(lambda = c(0, 0.01, .1), 
                                fraction = seq(.05, 1, length = 20))
        enetTune <- train(x = trainX, y = trainY,
                          method = "enet",
                          trControl = ctrl,
                          preProc = c("center", "scale"))
        Result_enetTune<-min(enetTune$results$RMSE) 
        if(trackingState){
          cat ('Try Elastic Net with RMSE ',Result_enetTune,'\n')}
        result[3,2]<-Result_plsTune  
        #PCR
        pcrTune <- train(x = trainX, y = trainY,
                         method = "pcr",
                         trControl = ctrl)
        Result_pcrTune<-min(pcrTune$results$RMSE) 
        if(trackingState){
          cat ('Try PCR with RMSE ',Result_pcrTune,'\n')}
        result[4,2]<-Result_pcrTune 
      }
  }else {#not highly correlated
     if (nrow(data)>ncol(data)){ #number of samples(N)>number of features(F) 
       #OLR
       lmTune <- train(x = trainX, y = trainY,
                       method = "lm",
                       trControl = ctrl)
       Result_lmTune<-min(lmTune$results$RMSE)
       if(trackingState){
         cat ('Try OLR with RMSE ',Result_lmTune,'\n')}
       result[5,2]<-Result_lmTune      
      }else{
        if (ncol(trainX)>dim.threshold){
          #high dim
          #lasso
          lassoTune <- train(x = trainX, y = trainY,
                          method = "lasso",
                          trControl = ctrl)
          Result_lassoTune<-min(lassoTune$results$RMSE)
          if(trackingState){
            cat ('Try Lasso with RMSE ',Result_lassoTune,'\n')}
          result[6,2]<-Result_lassoTune              
        }else{
          #ridge
          ridgeGrid <- expand.grid(lambda = seq(0, .1, length = 15))
          ridgeTune <- train(x = trainX, y = trainY,
                             method = "ridge",
                             trControl = ctrl,
                             preProc = c("center", "scale"))
          Result_ridgeTune<-min(ridgeTune$results$RMSE)
          if(trackingState){
            cat ('Try Ridge with RMSE ',Result_ridgeTune,'\n')}
          result[7,2]<-Result_ridgeTune 
          
          }  
        
      }
  }
  
  #SVM with linear kernel
  svmTune <- train(x = trainX, y = trainY,
                   method = "svmLinear",                 
                   trControl = ctrl)
  Result_SVM<-min(svmTune$results$RMSE)
  result[1,2]<-Result_SVM
  if(trackingState){
    cat ('Try SVM with RMSE ',Result_SVM,'\n')}
  
  cat('Select ',result[result$value== min(result$value),]$name,
      'with RMSE ',result[result$value== min(result$value),]$value)

}
