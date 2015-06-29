library(bgmm)
lm1<-read.csv("/Users/gregorymatthews/Dropbox/brophyTeeth/data/LM1.csv")
lm2<-read.csv("/Users/gregorymatthews/Dropbox/brophyTeeth/data/LM2.csv")
lm3<-read.csv("/Users/gregorymatthews/Dropbox/brophyTeeth/data/LM3.csv")
um1<-read.csv("/Users/gregorymatthews/Dropbox/brophyTeeth/data/UM1.csv")
um2<-read.csv("/Users/gregorymatthews/Dropbox/brophyTeeth/data/UM2.csv")
um3<-read.csv("/Users/gregorymatthews/Dropbox/brophyTeeth/data/UM3.csv")

names(lm1)[10:69]<-names(lm2)[10:69]<-names(lm3)[10:69]<-names(um1)[10:69]<-names(um2)[10:69]<-names(um3)[10:69]<-paste("f",1:60,sep="")

dat<-lm1[,c(2,6,10:69)]
dat$Tribe<-as.character(dat$Tribe)
#dat<-dat[dat$Tribe!="Unknown",]

cleanData <- dataPrep(dat)
#Split the data into training and pred data sets
knowns <- cleanData[cleanData$Tribe!="Unknown",c("pc1","pc2")]
X <- cleanData[cleanData$Tribe=="Unknown",c("pc1","pc2")]
labels<-as.numeric(as.factor(cleanData[cleanData$Tribe!="Unknown",2]))

a<-list(X=X,knowns=knowns,labels=labels)
greg<-semisupervised(X=X,knowns=a$knowns,class=a$labels,k=21)
plot(greg)
text(knowns[,1],knowns[,2],a$labels)














cleanDataTrain<-cleanData[-c(178,179),]
cleanDataPred<-cleanData[c(178:179),]
#cleanDataPred$Tribe<-NA
#cleanDataPred$Species<-NA

models<-trainPredToothRF2(cleanDataTrain,ntree=500)
check<-predToothRF2(models,cleanDataPred)




