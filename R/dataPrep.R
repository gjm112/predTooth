dataPrep <- function(trainingData,numPCs=30){
  ind <- names(trainingData)%in%paste("f",1:60,sep="")
  Sigma<-as.matrix(t(trainingData[,ind]))%*%as.matrix(trainingData[,ind])
  pcs<-as.data.frame(as.matrix(trainingData[,ind])%*%eigen(Sigma)$vectors[,1:numPCs])
  names(pcs)<-paste("pc",1:numPCs,sep="")
  outData<-cbind(trainingData,pcs)
  outData
  
}