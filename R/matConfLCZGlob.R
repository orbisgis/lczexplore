#' Compares two LCZ classifications on several locations
#'
#' @param filePath : the full path to the inputDfto import
#' @param inputDf : if filePath is an empty string inputDf is the name of an R dataframe containing the data
#' @param wf1 : a string indicating the origin of the LCZ classification 1
#' @param wf2 : a string indicating the origin of the LCZ classification 1
#' @param sep : the seperator used in the csv file
#' @param repr : brut or grouped
#' @param niveaux : by default levels of the LCZ classification are built
#' reading the data, but one can input a character string vector of levels,
#' for instance to produce several graphs with the same levels or levels in a specific order
#' @param plot if TRUE, then confusion matrix plot is shown, else it is just returned in the matConfOut object
#' @param column1 is the name of the column containing the Local Climatic Zone of the first type of datasets in the \'file\' dump file  be analyzed
#' @param column2 see column1 but for the second type of datasets
#' @param geomID1 is the name of the column containing the ID of each spatial units from the first type of datasets in \'inputDf\', it allows to post-treat some of the units
#' @param geomID2 is the name of the column containing the ID of each spatial units from the second type of datasets in \'inputDf\'
#' @param confid1 is the name of the column measuring the confidence given to the LCZ level in column1
#' @param confid2 is the name of the column measuring the confidence given to the LCZ level in column2
#' @param ... other parameters that may be passed from a function calling matConfLCZGlob
#' @import tidyr units ggplot2 dplyr cowplot forcats units tidyr RColorBrewer
#' @importFrom stats quantile
#' @return matConfOut is a list containing : matconf, the confusion matrix in its longer form ; matConfPlot,
#' a ggplot2 plot of said matrix ; areas, the summed area per levels for both LCZ classification ;
#' and pourcAcc, the general agreement between classifications expressed in percent of areas
#' @export
#'
#' @examples
#' matConfLCZGlob(filePath= paste0(
#' system.file("extdata", package = "lczexplore"),"/bdtopo_2_2_osm.csv"),
#' file="bdtopo_2_2_osm.csv", wf1="bdt", wf2="osm",
#' geomID1="ID_RSU", column1="LCZ_PRIMARY", confid1="LCZ_UNIQUENESS_VALUE",
#' geomID2="ID_RSU.1", column2="LCZ_PRIMARY.1", confid2="LCZ_UNIQUENESS_VALUE.1", sep=";", repr="brut",
#' niveaux="", plot=TRUE)
#' testSource<-read.csv(paste0(system.file("extdata", package = "lczexplore"),
#' "/bdtopo_2_2_osm.csv"), sep=";",header=TRUE)
#' matConfLCZGlob(filePath="",
#' inputDf = testSource, wf1="bdt", wf2="osm",
#' geomID1="ID_RSU", column1="LCZ_PRIMARY", confid1="LCZ_UNIQUENESS_VALUE",
#' geomID2="ID_RSU.1", column2="LCZ_PRIMARY.1", confid2="LCZ_UNIQUENESS_VALUE.1", sep=";", repr="brut",
#' niveaux="", plot=TRUE)

matConfLCZGlob<-function(filePath="", inputDf, wf1, wf2, geomID1="", column1, confid1="",
                         geomID2="", column2, confid2="", sep=";", repr="brut",
                         niveaux="", plot=TRUE, ...){

  if(column1==column2){
    column2<-paste0(column1,".1")
  }


  colonnes<-c(geomID1,column1,confid1,geomID2,column2,confid2)
  colonnes<-colonnes[sapply(colonnes,nchar)!=0] %>% c("accord","area","location")

  if(filePath!=""){
    echInt<-read.csv(filePath,sep,header=T,stringsAsFactors = T)
    names(echInt)<-colonnes
  } else {echInt<-inputDf[,colonnes]}




  nbTowns<-length(unique(echInt$ville))

  # if niveaux not specified, it is created from the file (longer sys.time)

  if (length(niveaux)==1){
    niveaux<-unique(
      c(echInt[,column1],echInt[,column2])
    )
  }

  echInt[,column1]<-factor(echInt[,column1],levels=niveaux)
  echInt[,column2]<-factor(echInt[,column2],levels=niveaux)

  # print("echInt")
  # print(head(echInt))

  # Marginal areas for first LCZ

  areaLCZ1<-echInt %>% group_by_at(.vars=column1) %>%
    summarize(area=sum(area,na.rm=F))%>%
    drop_units %>%
    ungroup()
  areaLCZ1$area<-round(areaLCZ1$area/sum(areaLCZ1$area,na.rm=F)*100,digits = 2)
  areaLCZ1<-areaLCZ1 %>% as.data.frame()

  # marginal for second LCZ
  areaLCZ2<-echInt %>% group_by_at(.vars=column2) %>%
    summarize(area=sum(area,na.rm=F))%>%
    drop_units %>%
    ungroup()
  areaLCZ2$area<-round(areaLCZ2$area/sum(areaLCZ2$area,na.rm=F)*100,digits = 2)
  areaLCZ2<-areaLCZ2 %>% as.data.frame()

  # Problem : some of the input files do not exhibit all possible LCZ values :
  # pasting the area to the labels would return an error
  # Here is an ugly solution to overcome this (and see later to include the potentially missing combination of levels)

  areas<-data.frame(niveaux=niveaux, area1=0, area2=0)
  #print("areas")
  #print(areas)
  #print("head of areaLCZ1 column1")
  #print(head(areaLCZ1[,column1]))

  for (i in areaLCZ1[,column1]){
    #print(" i as a level of areaLCZ1 equals")
    #print(i)
    areas[areas$niveaux==i,'area1']<-areaLCZ1[areaLCZ1[,column1]==i,'area']
  }

  for (i in areaLCZ2[,column2]){
    # print(" i as a level of areaLCZ2 equals")
    # print(i)
    areas[areas$niveaux==i,'area2']<-areaLCZ2[areaLCZ2[,column2]==i,'area']
  }

  # Get the general agreement between both input files
  pourcAcc<-(((echInt %>%  filter(accord==T) %>% select(area) %>% sum) /
                (echInt %>% select(area) %>% sum))*100) %>% round(digits=2)

  # the way to "feed" group_by is through .dots, to be checked, as it seems to be deprecated :
  # fixed with group_by_at


  matConf<-echInt %>% group_by_at(.vars=c(column1,column2)) %>%
    summarize(area=sum(area))%>% drop_units %>% ungroup %>% ungroup

  # print("matConf")
  # print(head(matConf))

  matConfLarge<-pivot_wider(data=matConf,names_from=column2,values_from=area)
  readable<-matConfLarge[,-1]/rowSums(matConfLarge[,-1],na.rm=T)*100
  matConfLarge<-cbind(matConfLarge[,1],round(x=readable,digits=2))

  # print("matConfLarge")
  # print(head(matConfLarge))


  ###############################################################
  ## A degeomer
  ###############################################################

  # Longer format to feet the geom_tile aes in ggplot2

  matConfLong<-pivot_longer(matConfLarge,cols=-1,names_to = column2)
  # print("matConfLong avant reorder factor")
  names(matConfLong)<-c(column1,column2,"accord")

  # Reordering of factors (as they were sorted in the order of showing in the file)

  matConfLong<-matConfLong %>% mutate(across(where(is.character),as_factor))
  matConfLong<-matConfLong %>%
    mutate(!!column1:=ordered(subset(matConfLong,select=column1,drop=T),levels=niveaux))
  matConfLong<-matConfLong %>%
    mutate(!!column2:=ordered(subset(matConfLong,select=column2,drop=T),levels=niveaux))


  ##############################################################################################################
  # Some values of LCZ may not appear in all datasets, we want to force them to appear to make all heat map easy to compare
  #################################################################################################################

  complement<-cbind(crossing(niveaux,niveaux),
                    data.frame(
                      indice=apply(
                        crossing(niveaux,niveaux),1,paste,collapse="."),
                      area=0
                    )
  )

  names(complement)<-c("LCZ1","LCZ2","uselessIndex","tempArea")
  # print("complement")
  # complement %>% head %>% print

  completed<-merge(x=matConfLong,y=complement,by.x=c(column1,column2),by.y=c("LCZ1","LCZ2"),all=T)
  completed$accord[is.na(completed$accord)]<-completed$tempArea[is.na(completed$accord)]


  matConfLong<-completed[,c(column1,column2,"accord")]
  matConfLong<-matConfLong %>%
    mutate(!!column1:=addNA(subset(matConfLong,select=column1,drop=T),ifany = T),
           !!column2:=addNA(subset(matConfLong,select=column2,drop=T),ifany = T),) %>%
    mutate(!!column1:=factor(subset(matConfLong,select=column1,drop=T),levels=niveaux),
           !!column2:=factor(subset(matConfLong,select=column2,drop=T),levels=niveaux))


  matConfLong<-matConfLong %>% arrange(column1,column2)
  #Include all the lcz levels, even if they are not present in the datasets

  # print("matConfLongaprès reorder factor")
  # print(matConfLong)
  datatemp<-data.frame(a=factor(niveaux),percArea1=areas$area1,percArea2=areas$area2)
  ############
  # Plot
  coordRef<-length(niveaux)+1
  # if (repr=='brut'){titrou<-"LCZ"} else {titrou<-"Grouped LCZs"}

  if (wf1=="bdtopo_2_2"){adtitre1<-" BDTOPO V2.2"} else
    if(wf1=="osm"){adtitre1<-" OSM "} else
      if(wf1=="wudapt"){adtitre1<-" WUDAPT"} else {adtitre1<-wf1}


  if (wf2=="bdtopo_2_2"){adtitre2<-" BDTOPO V2.2"} else
    if(wf2=="osm"){adtitre2<-" OSM "} else
      if(wf2=="wudapt"){adtitre2<-" WUDAPT"} else {adtitre2<-wf2}

  titre4<-paste(" Repartition of", adtitre1, " LCZ into LCZs of", adtitre2)
  sousTitre<-paste0("number of analysed locations : ", nbTowns)

  if(plot==T){
    matConfPlot<-ggplot(data = matConfLong, aes(x=get(column1), y=get(column2), fill =accord)) +
      geom_tile(color = "white",lwd=1.2,linetype=1)+
      labs(x="Reference",y="Alternative")+
      scale_fill_gradient2(low = "lightgrey", mid="cyan", high = "blue",
                           midpoint = 50, limit = c(0,100), space = "Lab",
                           name="% area") +
      geom_text(data=matConfLong[matConfLong$accord!=0,],aes(label=round(accord,digits=0)),
                color="black") +coord_fixed()+
      theme(axis.text.x = element_text(angle =70, hjust = 1),
            panel.background = element_rect(fill="grey"))+
      geom_tile(datatemp,mapping=aes(x=a,y=coordRef,fill=percArea1,height=0.8,width=0.8))+
      geom_tile(datatemp,mapping=aes(x=coordRef,y=a,fill=percArea2,height=0.8,width=0.8))+
      ggtitle(titre4,subtitle=sousTitre)
    print(matConfPlot)} else {matConfPlot<-NULL}

  matConfOut<-list(matConf=matConfLong,matConfPlot=matConfPlot,areas=areas,pourcAcc=pourcAcc)
  return(matConfOut)
}

