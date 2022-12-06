#' Computes the agreement between LCZ classification on a range of values of
#' an indicator of confidence granted to each LCZ classification
#'
#' @param file is an R file with geom IDs, LCZ classifications and
#' a confidence value granted for the LCZ value of each geom. Ignored if filePath is not empty.
#' @param filePath is the path to a csv file containing geom IDs, LCZ classifications and
#' a confidence value granted for the LCZ value of each geom.
#' @param nPoints is the number of points (quantiles) of confidence for which
#' the average agreement between classifs will be computed
#' @param wf1 is the name of the workflow used to produce the first LCZ
#' @param wf2 is the name of the workflow used to produce the second LCZ
#' @param geomID1 is the name of the column that contains the geom ID associated to the first workflow
#' @param column1 is the name of the column storing the first LCZ classification values
#' @param confid1 is the name of the column storing the first LCZ classification confidence value
#' @param geomID2 is the name of the column that contains the geom ID associated to the second workflow
#' @param column2 is the name of the column storing the second LCZ classification values
#' @param confid2 is the name of the column storing the second LCZ classification confidence value
#' @param sep the separator used if filePAth is not empty
#' @param repr were the levels grouped or do we expect original LCZ values
#' @param niveaux levels of LCZ expected in both LCZ classifications
#' @param plot if True the graph is plotted
#' @import dplyr ggplot2
#' @return
#' @export
#'
#' @examples
confidSensib<-function(file, filePath="", nPoints=5,
                       wf1="bdtopo_2_2", wf2="osm",
                       geomID1="ID_RSU", column1="LCZ_PRIMARY", confid1="LCZ_UNIQUENESS_VALUE",
                       geomID2="ID_RSU.1",column2="LCZ_PRIMARY.1", confid2="LCZ_UNIQUENESS_VALUE.1",
                       sep=";", repr="brut",
                       niveaux=levLCZ, plot=TRUE){

  colonnes<-c(geomID1,column1,confid1,geomID2,column2,confid2)
  colonnes<-colonnes[sapply(colonnes,nchar)!=0] %>% c("accord","aire","location")

  # Import the data if they are in a csv file or in a R object
  if(filePath!=""){
    echInt<-read.csv(filePath,sep,header=T,stringsAsFactors = T)
    names(echInt)<-colonnes
  } else {echInt<-file[,colonnes]}


  echInt$confidMin<-pmin(echInt[,confid1],echInt[,confid2])



# What is the agreement between LCZ classifications when no confidence value is available on any of them ?
  echIntNoconf<-subset(echInt,is.na(echInt$confidMin))

  NAPercAgr<-matConfLCZGlob(file=echIntNoconf, wf1=wf1, wf2=wf2,
                 geomID1=geomID1, column1=column1, confid1=confid1,
                 geomID2=geomID2, column2=column2, confid2=confid2,
                 sep=";", repr="brut",niveaux="", plot=F)$pourcAcc

  #How does the max of the confidence value of the LCZ classifs influences the agreement
  # between LCZ classifications

  echIntConf<-subset(echInt,!is.na(echInt$confidMin))
  # print("séparation");(print(nrow(echIntNoconf)));(print(nrow(echIntConf)));(print(nrow(echInt)))

  #############################################################################################
  # All LCZ levels treated together
  #############################################################################################
   internFunction<-function(echIntConf,nPoints){
       confSeq<-quantile(echIntConf$confidMin,probs=seq(0,1,length.out=nPoints),na.rm=T)
       print("confSeq in internFunction ") ; print(confSeq)

       percAgrKeep<-NULL
       confKeep<-NULL
       nbKeep<-NULL
       percAgrDrop<-NULL
       confDrop<-NULL
       nbDrop<-NULL

        for (i in confSeq){
         print(i)
         echIntKeep<-subset(echIntConf,confidMin>=i)
         if(nrow(echIntKeep)>0){print(nrow(echIntKeep))
           percAgrKeep<-c(percAgrKeep,
                        matConfLCZGlob(file=echIntKeep, wf1=wf1, wf2=wf2,
                                       geomID1=geomID1, column1=column1, confid1=confid1,
                                       geomID2=geomID2, column2=column2, confid2=confid2,
                                       sep=";", repr="brut",niveaux="", plot=F)$pourcAcc)
           nbKeep<-c(nbKeep,nrow(echIntKeep))

         }else{
             percAgrKeep<-c(percAgrKeep,NA)
             nbKeep<-c(nbKeep,0)
             }

         echIntDrop<-subset(echIntConf,confidMin<i)
         if(nrow(echIntDrop)>0){print(nrow(echIntDrop))
           percAgrDrop<-c(percAgrDrop,
                          matConfLCZGlob(file=echIntDrop, wf1=wf1, wf2=wf2,
                                         geomID1=geomID1, column1=column1, confid1=confid1,
                                         geomID2=geomID2, column2=column2, confid2=confid2,
                                         sep=";", repr="brut",niveaux="", plot=F)$pourcAcc)
           nbDrop<-c(nbDrop,nrow(echIntDrop))
         }else{
           percAgrDrop<-c(percAgrDrop,NA)
           nbDrop<-c(nbDrop,0)}

     }
     #   summary(echInt)

    data<-data.frame(Confidence=c(confSeq,confSeq),
                     Agreement=c(percAgrKeep,percAgrDrop),
                     Kept=rep(c("confidence >= threshold","confidence < threshold"),each=nPoints),
                     nbGeoms=c(nbKeep,nbDrop))

    # graphics

    etiquette<-paste0("average agreement percentage for LCZ with no confidence value : ",
                      NAPercAgr," \n (these geoms are excluded from computing other points)")

    confThreshPlot<-ggplot(data=data, aes(x=Confidence, y=Agreement, color=Kept, shape=Kept))+
      labs(x="Confidence threshold", color = "Geom set", shape="Geom set")+
      geom_point() +
      geom_text(aes(x=Confidence,y=Agreement,label=nbGeoms), nudge_y=-2)+
      geom_hline(yintercept=NAPercAgr,linetype='dashed',color='grey')+
      geom_text(aes(x=0.50,y=NAPercAgr,label=etiquette,vjust=1.5),inherit.aes=F,color='grey',size=4)+
      ggtitle(label="Agreement according to the minimum confidence granted to LCZ level",
              subtitle="Number of geoms used to compute agreement written under each point")

    ctOut<-list(ctPlot=confThreshPlot,ctData=data)
    return(ctOut)
}

  allLCZ<-internFunction(echIntConf=echIntConf,nPoints=nPoints)
  plot(allLCZ$ctPlot)
#############################################################################################
# Per LCZ levels of the first classification
#############################################################################################
niveaux<-unique(echIntConf[,column1]) %>% as.vector
  print("niveaux")
  print(niveaux)
 print("echinConf avant boucle LCZ") ; print(head(echIntConf))
  byLCZ<-data.frame(Confidence=numeric(), Agreement=numeric(),
                    Kept=character(),nbGeoms=numeric(),LCZ=character())

 echIntConfSplit<-split(x=echIntConf,f=echIntConf[[column1]])

 internFunction2<-function(echIntConf,nPoints){internFunction(echIntConf,nPoints)$ctData}
 # sortieParLCZ<-aggregate(echIntConf,by=echIntConf[[column1]],internFunction2,nPoints=nPoints)
 sortieParLCZ<-lapply(echIntConfSplit,internFunction2,nPoints=nPoints)
 nivList<-names(sortieParLCZ)
 sortie<-data.frame(Confidence=numeric(0),Agreement=numeric(0),Kept=character(0),nbGeom=numeric(0),LCZ=character(0))
 for (i in names(sortieParLCZ)){
    sortie<-rbind(sortie,cbind(sortieParLCZ[[i]],rep(i,nrow(sortieParLCZ[[i]]))))
 }


 #sortie<-do.call(what="rbind",sortieParLCZ)
 return(sortie)

 # for (i in 1:length(niveaux)){
#   print("boucle BYLCZ")
#   print(paste("i",i,"niveaux[i] ",niveaux[i]))
#   echIntConfTemp<-subset(echIntConf,column1==niveaux[i])
#   print(paste("echIntConf pour la modalité ",niveaux[i]))
#   print(head(echIntConf))
#   confSeq<-quantile(echIntConfTemp$confidMin,probs=seq(0,1,length.out=nPoints),na.rm=T)
#   print(paste("confSeqLCZ ", confSeq))
#   if(sum(is.na(confSeq))!=nPoints){
#   data<-internFunction(echIntConf=echIntConfTemp,nPoints=nPoints)$ctData
#   print(paste("nb lignes data = ",nrow(data)))
#   LCZ<-rep(x=niveaux[i],times=nPoints*2)
#   print(paste("nb lignes LCZ = ",nrow(LCZ)))
#   data<-cbind(data,LCZ)
#   nomData<-paste0("dataLCZ",i)
#   assign(nomData,data)
#   byLCZ<-rbind(byLCZ,get(nomData))} else {warning("not enough data to compute thresholds")}
#  }
#
#   print("tableau byLCZ")
#   print(byLCZ)
#
#
#

}

# test<-read.csv("bdtopo_2_2_osm.csv",sep=";")
#
# confidSensib(file=test,filePath="", nPoints=6,
#                        wf1="bdtopo_2_2", wf2="osm",
#                        geomID1="ID_RSU", column1="LCZ_PRIMARY", confid1="LCZ_UNIQUENESS_VALUE",
#                        geomID2="ID_RSU.1",column2="LCZ_PRIMARY.1", confid2="LCZ_UNIQUENESS_VALUE.1",
#                        sep=";", repr="brut",
#                        niveaux=levLCZ, plot=TRUE)

