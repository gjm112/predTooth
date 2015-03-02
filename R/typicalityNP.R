cleanData <- dataPrep(dat)

typicalityNP <- function(train=cleanData[1:105,],pred=cleanData[106,],resp="Tribe",numPCs=30)

ttt <- unique(train[[resp]])[1]
tempData <- train[train==ttt,]
ind<-names(tempData)%in%paste("pc",1:numPCs,sep="")
centroid<-apply(tempData[,ind],2,mean)
dists<-apply(tempData[,ind],1,function(x){sqrt(sum(x-centroid)^2)})
distPoint<-apply(pred[,ind],1,function(x){sqrt(sum(x-centroid)^2)})

loc <- which(diff(distPoint<=density(dists)$x)==1)
step<-diff(density(dists)$x)[1]
out<-sum(density(dists)$y[density(dists)$y<=density(dists)$y[loc]])*step
out
}