test<-trainPredToothRF(cleanData,resp="Species",cond=TRUE)

trainPredToothRF <- function(cleanData,resp="Tribe",cond=FALSE){
  library(randomForest)
  cleanData[[resp]]<-as.character(cleanData[[resp]])
  
  if (cond==FALSE){
    #Create the formula
    cleanData[[resp]]<-as.factor(cleanData[[resp]])
    form<-formula(paste(resp,"~",paste(paste("pc",1:30,sep=""),collapse="+")))
    outRF<-randomForest(form,data=cleanData)
  }
  
  
  if (cond==TRUE){
    modList<-list()
    tribesVec<-unique(cleanData[["Tribe"]])
    for (ttt in as.character(tribesVec)){print(ttt)
                                         tribeDatTemp<-cleanData[cleanData[["Tribe"]]==ttt,]
                                         
                                         #subset the data 
                                         #Create the formula
                                         if (length(unique(tribeDatTemp$Species))>1){
                                           tribeDatTemp[[resp]]<-as.factor(tribeDatTemp[[resp]])
                                           form<-formula(paste(resp,"~",paste(paste("pc",1:30,sep=""),collapse="+")))
                                           modList[[ttt]]<-randomForest(form,data=tribeDatTemp)
                                         }
    }
    outRF<-modList
  }
  outRF
  
  
}