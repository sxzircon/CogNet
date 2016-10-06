linear_classification<-function(features,target,trackingState){
  # Identify the algorithm with highest accuracy for non-linear classification.
  #
  # Args:
  #	 features: the input feature of the data
  #	 target: the target of classification problem
  #  trackingState: boolean, when the value is true, test results from selected 
  #                 algorithms will be displayed; its default value is false
  # Returns:
  #  the algorithm that offers the highest or very closed to the highest accuracy  

  #the threshold for small and medium/large
  threshold=10
  
  trainY<-target
  trainX<-features

  if(missing(trackingState)){
    trackingState=F
  }

  levels(trainY)<-make.names(levels(trainY))
  set.seed(0)
  indx <- createFolds(trainY, returnTrain = TRUE)
  ctrl <- trainControl(method = "boot",
                       classProbs = TRUE,
                       index = indx,
                       savePredictions = TRUE)
  ################################non-linear classification##############################

  treeClassifier <- function(result,trainX,trainY) {
    #non-interpretable tree model
    #Random forest
    mtryGrid <- data.frame(mtry = floor(seq(10, ncol(trainX), length = 10)))
    rfTune <- train(x = trainX, y = trainY,
                    method = "rf",
                    ntree = 1000,
                    importance = TRUE,
                    trControl = ctrl)
    Result_rf<-max(rfTune$results$Accuracy) 
    result[4,2]<-Result_rf
    #boosting
    gbmGrid <- expand.grid(interaction.depth = seq(1, 7, by = 2),
                           n.trees = seq(100, 1000, by = 50),
                           shrinkage = c(0.01, 0.1),
                           n.minobsinnode=10)
    gbmTune <- train(x = trainX, y = trainY,
                     method = "gbm",
                     #tuneGrid = gbmGrid,
                     trControl = ctrl,
                     verbose = FALSE)
    Result_gbm<-min(gbmTune$results$Accuracy) 
    result[5,2]<-Result_gbm
    
    #interpretable tree model
    #CART -- no tuning here, to do
    CARTTune <- train(x = trainX, y = trainY,
                      method = "rpart",
                      #tuneLength = 30,
                      trControl = ctrl)  
    Result_CART<-max(CARTTune$results$Accuracy) 
    result[6,2]<-Result_CART
    
    #C45 
    C45Tune <- train(x = trainX, y = trainY,
                     method = "J48",
                     trControl = ctrl)  
    Result_C45<-max(C45Tune$results$Accuracy) 
    result[7,2]<-Result_C45
    
    return(result)
  }


  ############################non-linear classification############################
  names<-c('SVM','FDA','Native Bayes','Random Forest','Boosting','CART','C45','RDA','MDA','Max')
  value<-rep(-1,length(names))
  result<-data.frame(names,value)
  
  #SVM with RBF kernel
  svmTune <- train(x = trainX, y = trainY,
                   method = "svmRadial",
                   trControl = ctrl)
  Result_SVM<-max(svmTune$results$Accuracy)
  result[1,2]<-Result_SVM
  if(trackingState){
    cat ('Try SVM(RBF) with Accuracy ',Result_SVM,'\n')}
  
  fdaFit <- train(x = trainX, 
                  y = trainY,
                  method = "fda",
                  trControl = ctrl)
  Result_FDA<-max(fdaFit$results$Accuracy)
  if(trackingState){
    cat ('Try FDA with Accuracy ',Result_FDA,'\n')}
  result[2,2]<-Result_FDA
  
  if (Result_SVM>=Result_FDA){
    #N<F 
    if (nrow(trainX)<ncol(trainX)){
      #if the target is categorical
      if (cateTag){
       
        result<-treeClassifier(result,trainX,trainY)
        
      }else{
        #naive bayes
        nBayesFit <- train(x = trainX,
                           y = trainY,
                           method = "nb",
                           trControl = ctrl)
        Result_nBayes<-max(nBayesFit$results$Accuracy)
        result[3,2]<-Result_nBayes
      }
   
    }else{
      #N>F
  
      if (nrow(trainX)> threshold*ncol(trainX)){
        #large/medium data set
        #has not been turned, to do 
        rdaFit <- train(x = trainX, 
                        y = trainY,
                        method = "rda",
                        trControl = ctrl)   
        Result_rda<-max(rdaFit$results$Accuracy)
        result[8,2]<-Result_rda      
      }else{
        #small data set 
        #has not been turned, to do 
        mdaFit <- train(x = trainX, 
                        y = trainY,
                        method = "mda",
                        trControl = ctrl)   
        Result_mda<-max(mdaFit$results$Accuracy)
        result[9,2]<-Result_mda
      }
      result<-treeClassifier(result,trainX,trainY)
    }
  
    cat('Select ',as.character(result[result$value== max(result$value),]$name),
        'with Accuracy ',result[result$value== max(result$value),]$value,'\n')  
    
  }else { 
    if(trackingState){
      cat("Select FDA with Accuracy ",Result_FDA,'\n') }
  
  }
}
