source("/Users/gregorymatthews/Dropbox/predTooth/R/cleanRawFiles.R")

thing<-cleanRawFile("/Users/gregorymatthews/Dropbox/brophyTeeth/DSCN3018LA")
plot(thing,type='l')

tribes<-list.files("/Users/gregorymatthews/Dropbox/brophyTeeth/Raw data/")
filesList<-datList<-list()
for (t in tribes){print(t)
  species<-list.files(paste("/Users/gregorymatthews/Dropbox/brophyTeeth/Raw data/",t,sep=""))
  for (s in species){
  location<-list.files(paste("/Users/gregorymatthews/Dropbox/brophyTeeth/Raw data/",t,"/",s,sep=""))
  for (l in location){
  filesList[[t]][[s]][[l]]<-list.files(paste("/Users/gregorymatthews/Dropbox/brophyTeeth/Raw data/",t,"/",s,"/",l,sep=""))
  
  files<-filesList[[t]][[s]][[l]]
  files <- files[grep("DSCN",files)]
  for (f in files){print(f)
  datList[[t]][[s]][[l]][[f]]<-cleanRawFile(paste("/Users/gregorymatthews/Dropbox/brophyTeeth/Raw data/",t,"/",s,"/",l,"/",f,sep=""))
            }
  
  }
  }
}


#clean up the tooth names
for (t in tribes){
  species<-names(datList[[t]])
  for (s in species){
    names(datList[[t]][[s]])<-substring(names(datList[[t]][[s]]),1,3)
}}

aaa<-array(NA,c(61,2,346))
i<-1
for (t in tribes){
  species<-names(datList[[t]])
  for (s in species){
    files<-names(datList[[t]][[s]][["LM1"]])
    for (f in files){
      print(i)
    temp<-datList[[t]][[s]][["LM1"]][[f]]
    aaa[,,i]<-temp
    i<-i+1
  }}}

library(shapes)
test<-procGPA(aaa)
shapepca(test,type="v",mag=3)

plot(test$rotated[,,1],type="l",col=rgb(0,0,0,0.25),xlim=c(-500,500),ylim=c(-300,300),main="Procrustes modified shapes using shapes::procGPA")
for (i in 1:346){
  col<-rgb(runif(1),runif(1),runif(1),0.25)
polygon(test$rotated[,,i],bor=col,lwd=1)
}

polygon(test$mshape,bor="black",lwd=2)


########################################################
##Using shapes package
########################################################
library(shapes)
greg<-array(c(proY$X,proY$Yrot,proZ$Yrot),dim=c(4,2,3))
test<-procGPA(greg)
shapepca(test,type="v",mag=3)
shapepca(test,type="r",mag=3)


plot(0,0,xlim=c(-2,2),ylim=c(-5,5),col="white",main="Procrustes modified shapes using shapes::procGPA")
polygon(test$rotated[,,1],bor="red",lwd=5)
polygon(test$rotated[,,2],bor="blue",lwd=5)
polygon(test$rotated[,,3],bor="gold",lwd=5)


      temp<-datList[[t]][[s]][[l]][[f]]
    plot(temp,type='l')
