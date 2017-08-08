## Problem 1

# A function that evaluates the predictions of a classifier
# Inputs:   yPred - predicted values from a classifier
#           yTrue - actuall outcomes
#           probThresh - set probability threshold (can vary for optimization)
#           
#
# Outputs:  # true positives, # false positives, # true negatives,
#           # false negatives, true positive rate, true negative rate,
#           sensitivity, specificity, precision, recall, accuracy,
#           error rate
# Outputs are given in the form of a list 

evalClassifier<-function(yPred,yTrue,probThresh){
  
  # test for two class scenario
  if (length(unique(yTrue))>2){
    print('more than two class outcomes')
  return(unique(yTrue))
  }
  
  # if yPred is a probability, classify as 0's or 1's based on probThresh
  yPredClass<-rep(NA,length(yPred))
  if (length(unique(yPred))>2){
    
    yPredClass[yPred >= probThresh]<-1
    yPredClass[yPred < probThresh]<-0
    
  } else if (length(unique(yPred <= 2))){
    
    yPredClass<-yPred
  }
  
  CM<-matrix(NA,2,2)
  CM[1,1]<-length(yPredClass[yPredClass==1 & yTrue == 1])
  CM[1,2]<-length(yPredClass[yPredClass==1 & yTrue ==0])
  CM[2,1]<-length(yPredClass[yPredClass==0 & yTrue ==1])
  CM[2,2]<-length(yPredClass[yPredClass==0 & yTrue ==0])
  
 
  # number of true and false positives
  truePos<-CM[1,1]
  falsePos<-CM[1,2]
  # number of true and false negatives
  trueNeg<-CM[2,2]
  falseNeg<-CM[2,1]
  
  # true positive rate and true negative rate
  TPR<-truePos/sum(truePos,falseNeg)
  TNR<-trueNeg/sum(trueNeg,falsePos)
  
  # sensitivity and specificity
  sensitivity<-TPR
  specificity<-TNR
  
  # precision and recalll
  precision<-truePos/(truePos+falsePos)
  recall<-truePos/(truePos+falseNeg)
  
  # accuracy and error rate
  accuracy<-sum(CM[1,1],CM[2,2])/sum(CM[1,1],CM[1,2],CM[2,1],CM[2,2])
  errorRate<-1-accuracy
  
  # create list of output info
  eval_List<-list("yTrue" = yTrue,
                  "yPred" = yPred,
                  "yPredClass" = yPredClass,
                  "probThresh" = probThresh,
                  "truePos" = truePos,
                  "falsePos" = falsePos,
                  "trueNeg" = trueNeg,
                  "falseNeg" = falseNeg,
                  "TPR" = TPR,
                  "TNR" = TNR,
                  "sensitivity" = sensitivity,
                  "specificity" = specificity,
                  "precision" = precision,
                  "recall" = recall,
                  "accuracy" = accuracy,
                  "errorRate" = errorRate,
                  "confusion" = CM)
  return(eval_List)
  
}