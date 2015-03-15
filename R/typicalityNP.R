cleanData <- dataPrep(dat)
table(cleanData$Tribe)
col<-rep("black",length(cleanData$Tribe))
col[cleanData$Tribe=="Alcelaphini"]<-"red"
col[cleanData$Tribe=="Antilopini"]<-"orange"
col[cleanData$Tribe=="Bovini"]<-"yellow"
col[cleanData$Tribe=="Hippotragini"]<-"darkgreen"
col[cleanData$Tribe=="Neotragini"]<-"blue"
col[cleanData$Tribe=="Reduncini"]<-"purple"
col[cleanData$Tribe=="Tragelaphini"]<-"pink"
plot(cleanData$pc1,cleanData$pc2,pch=16,col=col)


numPCs<-2

ind<-names(cleanData)%in%paste("pc",1:numPCs,sep="")
temp<-cleanData[cleanData$Tribe=="Tragelaphini",ind]
mu<-apply(temp,2,mean)
sigma<-apply(temp,2,sd)
temp<-as.data.frame(scale(temp))


null<-rep(NA,dim(temp)[1])
for (i in 1:dim(temp)[1]){print(i)
typ<-temp[i,]
tempData<-temp[-i,]
test<-apply(tempData,1,function(x){sqrt(sum(x-typ)^2)})
null[i]<-mean(sort(test)[1:20])
}

typicalityList<-list()
dat<-data.frame(pc1=c(rep(seq(-1.5,1,.5),7)),pc2=rep(seq(-2,1,0.5),each=6))
#dat<-as.data.frame((scale(dat,center=mu,scale=sigma)))
for (i in 1:dim(dat)[1]){print(i)
test<-apply(tempData,1,function(x){sqrt(sum(x-dat[i,])^2)})
stat<-mean(sort(test)[1:20])

if (sum(stat<=density(null)$x)>0){
  loc <- which(diff(stat<=density(null)$x)==1)
step<-diff(density(null)$x)[1]
typicalityList[[ttt]][i]<-sum(density(null)$y[density(null)$y<=density(null)$y[loc]])*step
      }
if (sum(stat<=density(null)$x)==0){typicalityList[[ttt]][i]<-0}

}

plot(temp$pc1,temp$pc2,pch=16,cex=.5)
points(dat$pc1,dat$pc2,pch=16,cex=2*typicalityList[[ttt]]+0.25,col="red")
points(tempData$pc1,tempData$pc2,pch=16,cex=1)

typ <- typicalityNP(train=cleanData,pred=cleanData,resp="Tribe")

typicalityNP <- function(train=cleanData,pred=cleanData,resp="Tribe",numPCs=30){

  typicalityList<-list()
for (ttt in unique(train[[resp]])){

tempData <- train[train==ttt,]
predTemp <- pred[pred==ttt,]
ind<-names(tempData)%in%paste("pc",1:numPCs,sep="")
centroid<-apply(tempData[,ind],2,mean)
dists<-apply(tempData[,ind],1,function(x){sqrt(sum(x-centroid)^2)})
typicalityList[[ttt]]<-rep(NA,dim(pred)[1])
for (i in 1:dim(pred)[1]){print(i)
distPoint<-apply(pred[i,ind],1,function(x){sqrt(sum(x-centroid)^2)})
loc <- which(diff(distPoint<=density(dists)$x)==1)
step<-diff(density(dists)$x)[1]
typicalityList[[ttt]][i]<-sum(density(dists)$y[density(dists)$y<=density(dists)$y[loc]])*step
}

}
typicalityList
}

plot(pred$pc1[1:321],pred$pc2[1:321],cex=1.5*typicalityList[[ttt]],pch=16)
