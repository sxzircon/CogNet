linear_classification<-function(features,target,trackingState){
  # Identify the algorithm with highest accuracy for linear classification.
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
  #the threshold for correlation
  corr.thresold=2
  #the threshold for high dim
  dim.threshold=100
  
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
  
  ############################linear classification############################
  names<-c('SVM','PLSDA','LDA','Logistic Regression','Nearest Shrunken Centroid','Max')
  value<-rep(-Inf,length(names))
  result<-data.frame(names,value)
  result$names<-as.character(result$names)
  
  
  pdata <- preProcess(trainX, method="pca")
  if (corr.thresold*pdata$numComp>ncol(data)){  #not highly correlated
    
    if (nrow(data)>ncol(data)){ #number of samples(N)>number of features(F)
      
      if(nrow(data)>5*ncol(data)){
        #LDA
        ldaFit <- train(x = trainX, 
                        y = trainY,
                        method = "lda",
                        preProc = c("center","scale"),
                        trControl = ctrl)
        Result_ldaFit<-max(ldaFit$results$Accuracy) 
        if (trackingState) {
          cat ('Try LDA with Accuracy ',Result_ldaFit,'\n')}
        result[3,2]<-Result_ldaFit
        
      }else{
        #logistic regression
        lgFit <- train(x = trainX, 
                       y = trainY,
                       method = "LogitBoost", #glm only suitable for binary classification, so use Boosted Logistic Regression
                       trControl = ctrl)
        Result_lgFit<-max(lgFit$results$Accuracy) 
        if (trackingState) {
         cat ('Try logistic regression with Accuracy ',Result_lgFit,'\n')}
        result[4,2]<-Result_lgFit
        
      }    
      
      
    }else{
      
      #if (nrow(trainX)> threshold*ncol(trainX)){
      if (ncol(trainX)>dim.threshold){#high dim
        nscFit <- train(x = trainX, 
                        y = trainY,
                        method = "pam",
                        preProc = c("center", "scale"),
                        #tuneGrid = data.frame(threshold = seq(0, 25, length = 10)),
                        trControl = ctrl)
        Result_nscFit<-max(nscFit$results$Accuracy) 
        if (trackingState) {
         cat ('Try Nearest Shrunken Centroid with Accuracy ',Result_nscFit,'\n')}
        result[5,2]<-Result_nscFit
      }else{
        #PLSDA
        plsFit <- train(x = trainX, 
                        y = trainY,
                        method = "pls",
                        # tuneGrid = expand.grid(ncomp = 1:10),
                        preProc = c("center","scale"),
                        trControl = ctrl)
        Result_plsda<-max(plsFit$results$Accuracy) 
        if (trackingState) {
         cat ('Try PLSDA with accuracy ',Result_plsda,'\n')}
        result[2,2]<-Result_plsda       
        
        
      }
      
      
    }
    
  }else {# highly correlated
    #PLSDA
    plsFit <- train(x = trainX, 
                    y = trainY,
                    method = "pls",
                    #tuneGrid = expand.grid(ncomp = 1:10),
                    preProc = c("center","scale"),
                    trControl = ctrl)
    Result_plsda<-max(plsFit$results$Accuracy) 
    if (trackingState) {
      cat ('Try PLSDA with accuracy ',Result_plsda,'\n')}
    result[2,2]<-Result_plsda  
  }
  
  #SVM with linear kernel
  svmTune <- train(x = trainX, y = trainY,
                   method = "svmLinear",                 
                   trControl = ctrl)
  Result_SVM<-max(svmTune $results$Accuracy)
  if (trackingState) {
    cat ('Try SVM with accuracy ',Result_SVM,'\n')}
  result[1,2]<-Result_SVM
  

  res<-cat('Select ',result[result$value== max(result$value),]$name,
      'with accuracy ',result[result$value== max(result$value),]$value)
  
}
