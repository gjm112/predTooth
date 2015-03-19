#test<-trainPredToothRF(cleanData,resp="Species",cond=TRUE)


trainPredToothRF2 <- function(cleanData,resp="Tribe",resp2="Species",ntree=500,mtry=15){
  library(randomForest)
  cleanData[[resp]]<-as.character(cleanData[[resp]])
  
  
    #Create the formula
    cleanData[[resp]]<-as.factor(cleanData[[resp]])
    form<-formula(paste(resp,"~",paste(paste("pc",1:30,sep=""),collapse="+")))
    outRFtribe<-randomForest(form,data=cleanData)
  
  
  #Predict Species conditional on Tribe
    modList<-list()
    tribesVec<-unique(cleanData[[resp]])
    for (ttt in as.character(tribesVec)){print(ttt)
                                         tribeDatTemp<-cleanData[cleanData[[resp]]==ttt,]
                                         
                                         #subset the data 
                                         #Create the formula
                                         if (length(unique(tribeDatTemp$Species))>1){
                                           tribeDatTemp[[resp2]]<-as.factor(tribeDatTemp[[resp2]])
                                           form<-formula(paste(resp2,"~",paste(paste("pc",1:30,sep=""),collapse="+")))
                                           modList[[ttt]]<-randomForest(form,data=tribeDatTemp,ntree=ntree)
                                         }
                                         outRF<-list(tribe=outRFtribe,species=modList)
    }
  outRF  
  }
  
  
  
