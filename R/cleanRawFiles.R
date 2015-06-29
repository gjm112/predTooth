cleanRawFile<-function(fileName="/Users/gregorymatthews/Dropbox/brophyTeeth/DSCN3018LA"){
x<-read.table(fileName)
x<-x[,-c(1:20)]
x<-x[,-seq(13,150,13)]
x<-unlist(x)
x<-matrix(x,ncol=2,byrow=TRUE)
x<-rbind(x,x[1,])
x
}


