#RFtribe<-trainPredToothRF(cleanData,resp="Species",cond=TRUE)
#model<-RFtribe
#test<-predToothRF(model,predData,cond=TRUE,type="response")

predToothRF2 <- function(model,predData,type="prob"){
  
  #first predict Tribe
    predTribe <- predict(models[["tribe"]],predData,type=type)
      
    predSpeciesGivenTribe<-list()  
    for (ttt in names(models$species)){
    predSpeciesGivenTribe[[ttt]] <- predict(models[["species"]][[ttt]],predData,type=type)
        }
    
    predSpecies<-list()  
    for (ttt in names(models$species)){
    predSpecies[[ttt]]<-predSpeciesGivenTribe[[ttt]]*predTribe[,colnames(predTribe)==ttt]
    }
    predSpecies[["Antilopini"]]<-predTribe[,"Antilopini"]
    predSpecies[["Bovini"]]<-predTribe[,"Bovini"]
    
    pred<-do.call(cbind,predSpecies)
    colnames(pred)[colnames(pred)=="Bovini"]<-"9"
    colnames(pred)[colnames(pred)=="Antilopini"]<-"5"
    pred<-pred[,as.character(c(1:20))]
  out<-cbind(pred,predTribe)
  out
}
  