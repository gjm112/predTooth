trainPredToothLDA <- function(cleanData,resp="Tribe",cond=FALSE){
  library(MASS)
  cleanData[[resp]]<-as.character(cleanData[[resp]])
  
  if (cond==FALSE){
    #Create the formula
    form<-formula(paste(resp,"~",paste(paste("pc",1:30,sep=""),collapse="+")))
    outLDA<-lda(form,data=cleanData)
  }
  
  
  if (cond==TRUE){
    modList<-list()
    tribesVec<-unique(cleanData[["Tribe"]])
    for (ttt in as.character(tribesVec)){print(ttt)
                                         tribeDatTemp<-cleanData[cleanData[["Tribe"]]==ttt,]
                                         
                                         if (length(unique(tribeDatTemp$Species))==1){
                                           modList[[ttt]]<-as.character(unique(tribeDatTemp$Species))
                                         }
                                         #subset the data 
                                         #Create the formula
                                         if (length(unique(tribeDatTemp$Species))>1){
                                           
                                           form<-formula(paste(resp,"~",paste(paste("pc",1:30,sep=""),collapse="+")))
                                           modList[[ttt]]<-lda(form,data=tribeDatTemp)
                                         }
    }
    outLDA<-modList
  }
  outLDA
  
  
}