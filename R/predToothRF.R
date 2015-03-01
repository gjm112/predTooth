RFtribe<-trainPredToothRF(cleanData,resp="Species",cond=TRUE)
model<-RFtribe
test<-predToothRF(model,predData,cond=TRUE,type="response")


predToothRF <- function(model,predData,cond=FALSE,type="response"){
  
  if (cond==TRUE){
    tribeVec <- unique(predData[["Tribe"]])
    predList<-list()
    for (ttt in as.character(tribeVec)){
      if (!is.null(model[[ttt]])){    
         predList[[ttt]] <- predict(model[[ttt]],predData[predData$Tribe==ttt,],type=type)
      }
      
    }
    out<-predList                                 
                                        
     }
    
    
    
  
  
  
  if (cond==FALSE){
    out <- predict(model,predData,type=type)
    out
  }
  out
}