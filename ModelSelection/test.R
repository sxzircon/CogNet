rm(list=ls())
setwd("/Users/Lei/Desktop/work in IBM/Data/TEST_FOR_PAPER/D3.2")

data<-read.csv("data/regression/Airfoil.csv",header=F,sep='\t')
data<-data[complete.cases(data),]
trainY<-data$V6
trainX<-data
trainX$V6<-NULL

nonlinearRegression <- dget("nonlinearRegression.R")

nonlinearRegression(trainX,trainY)