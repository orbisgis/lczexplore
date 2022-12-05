confidSensib<-function(file, filePath="", nPoints=5,
                       wf1="bdtopo_2_2", wf2="osm",
                       geomID1="ID_RSU", column1="LCZ_PRIMARY", confid1="LCZ_UNIQUENESS_VALUE",
                       geomID2="ID_RSU.1",column2="LCZ_PRIMARY.1", confid2="LCZ_UNIQUENESS_VALUE.1",
                       sep=";", repr="brut",
                       niveaux=levLCZ, plot=TRUE){
library(dplyr)
library(ggplot2)

  colonnes<-c(geomID1,column1,confid1,geomID2,column2,confid2)
  colonnes<-colonnes[sapply(colonnes,nchar)!=0] %>% c("accord","aire","location")

  # Import the data if they are in a csv file or in a R object
  if(filePath!=""){
    echInt<-read.csv(filePath,sep,header=T,stringsAsFactors = T)
    names(echInt)<-colonnes
  } else {echInt<-file[,colonnes]}


  echInt$confidMax<-pmax(echInt[,confid1],echInt[,confid2])



# What is the agreement between LCZ classifications when no confidence value is available on any of them ?
  echIntNoconf<-subset(echInt,is.na(echInt$confidMax))

  NAPercAgr<-matConfLCZGlob(file=echIntNoconf, wf1=wf1, wf2=wf2,
                 geomID1=geomID1, column1=column1, confid1=confid1,
                 geomID2=geomID2, column2=column2, confid2=confid2,
                 sep=";", repr="brut",niveaux="", plot=F)$pourcAcc

  #How does the max of the confidence value of the LCZ classifs influences the agreement
  # between LCZ classifications

  echIntConf<-subset(echInt,!is.na(echInt$confidMax))
  print("séparation");(print(nrow(echIntNoconf)));(print(nrow(echIntConf)));(print(nrow(echInt)))

   confSeq<-quantile(echIntConf$confidMax,probs=seq(0,1,length.out=nPoints),na.rm=T)
   print("confSeq") ; print(confSeq)

   percAgrKeep<-NULL
   confKeep<-NULL
   percAgrDrop<-NULL
   confDrop<-NULL

    for (i in confSeq){
     print(i)
     echIntKeep<-subset(echIntConf,confidMax>=i)
     if(nrow(echIntKeep)>0){print(nrow(echIntKeep))
       percAgrKeep<-c(percAgrKeep,
                    matConfLCZGlob(file=echIntKeep, wf1=wf1, wf2=wf2,
                                   geomID1=geomID1, column1=column1, confid1=confid1,
                                   geomID2=geomID2, column2=column2, confid2=confid2,
                                   sep=";", repr="brut",niveaux="", plot=F)$pourcAcc)
       }else{percAgrKeep<-c(percAgrKeep,NA)}

     echIntDrop<-subset(echIntConf,confidMax<=i)
     if(nrow(echIntDrop)>0){print(nrow(echIntDrop))
       percAgrDrop<-c(percAgrDrop,
                      matConfLCZGlob(file=echIntDrop, wf1=wf1, wf2=wf2,
                                     geomID1=geomID1, column1=column1, confid1=confid1,
                                     geomID2=geomID2, column2=column2, confid2=confid2,
                                     sep=";", repr="brut",niveaux="", plot=F)$pourcAcc)
     }else{percAgrDrop<-c(percAgrDrop,NA)}

 }
 #   summary(echInt)

data<-data.frame(Confidence=c(confSeq,confSeq),
                 Agreement=c(percAgrKeep,percAgrDrop),
                 Kept=rep(c("Greater","Lesser"),each=nPoints))

# graphics

etiquette<-paste0("average agreement percentage for LCZ with no confidence value : ",NAPercAgr)

ggplot(data=data, aes(x=Confidence, y=Agreement, color=Kept, shape=Kept))+
  geom_point() +
  geom_hline(yintercept=NAPercAgr,linetype='dashed',color='grey')+
  geom_text(aes(x=0.50,y=NAPercAgr,label=etiquette,vjust=1.5),inherit.aes=F,color='grey')


}

test<-read.csv("bdtopo_2_2_osm.csv",sep=";")

confidSensib(file=test,filePath="", nPoints=6,
                       wf1="bdtopo_2_2", wf2="osm",
                       geomID1="ID_RSU", column1="LCZ_PRIMARY", confid1="LCZ_UNIQUENESS_VALUE",
                       geomID2="ID_RSU.1",column2="LCZ_PRIMARY.1", confid2="LCZ_UNIQUENESS_VALUE.1",
                       sep=";", repr="brut",
                       niveaux=levLCZ, plot=TRUE)

