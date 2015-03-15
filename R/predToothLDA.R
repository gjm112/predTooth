#LDAtribe<-trainPredToothLDA(cleanData,resp="Species",cond=TRUE)
#model<-LDAtribe
#predToothLDA(model,predData,cond=TRUE)


predToothLDA <- function(model,predData,cond=FALSE){
  
  if (cond==TRUE){
    tribeVec <- unique(predData[["Tribe"]])
    predList<-list()
    for (ttt in as.character(tribeVec)){print(ttt)
      predDataTemp <- predData[predData$Tribe==ttt,]           
      predList[[ttt]]<-list()
      if (length(unique(predDataTemp$Species))==1){
        predList[[ttt]]$posterior <- data.frame(x=rep(1,length(predDataTemp$Species)))
        names(predList[[ttt]]$posterior)<-as.character(unique(predDataTemp$Species))
        
        predList[[ttt]]$class <- rep(predDataTemp$Species,length(predDataTemp$Species))
      }
      
      if (length(unique(predDataTemp$Species))>1){
      predList[[ttt]]$posterior <- predict(model[[ttt]],predData[predData$Tribe==ttt,])$posterior
      predList[[ttt]]$class <- predict(model[[ttt]],predData[predData$Tribe==ttt,])$class
      }
      
    
      
    }
    
    
    out<-predList
  }
  
  
  if (cond==FALSE){
  out <- predict(model,predData)
  out
  }
out
}