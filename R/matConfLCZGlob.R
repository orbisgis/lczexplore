#' Compares two LCZ classifications on several locations
#'
#' @param filePath : the full path to the file to import
#' @param wf1 : a string indicating the origin of the LCZ classification 1
#' @param wf2 : a string indicating the origin of the LCZ classification 1
#' @param sep : the seperator used in the csv file
#' @param repr : brut or grouped
#' @param niveaux : by default levels of the LCZ classification are built
#' reading the data, but one can input a character string vector of levels,
#' for instance to produce several graphs with the same levels or levels in a specific order
#' @param plot if TRUE, then confusion matrix plot is shown, else it is just returned in the matConfOut object
#' @param ...

#' @import tidyr units ggplot2 dplyr cowplot forcats units tidyr RColorBrewer
#' @return matConfOut is a list containing the confusion matrix in its longer form, matConf,
#' a ggplot2 plot of said matrix called matConfPlot,
#' the summed area per levels for both LCZ classification called aires,
#' and the general agreement expressed in percent, pourcAcc
#' @export
#'
#' @examples
matConfLCZGlob<-function(filePath,wf1,wf2,column1,column2,sep=";",repr="brut",
                        niveaux="",plot=T,...){

  if(column1==column2){
    column2<-paste0(column1,".1")
  }


  echInt<-read.csv(filePath,sep,header=F,stringsAsFactors = T)
  names(echInt)<-c("ID",column1,column2,"accord","aire","ville")

  nbTowns<-length(unique(echInt$ville))

  # if niveaux not specified, it is created from the file (longer sys.time)

  if (length(niveaux)==1){
    niveaux<-unique(
      c(echInt[,column1],echInt[,column2])
    )
  }

  echInt[,column1]<-factor(echInt[,column1],levels=niveaux)
  echInt[,column2]<-factor(echInt[,column2],levels=niveaux)

  print("echInt")
  print(head(echInt))

  # Marginal areas

  areaLCZ1<-echInt %>% group_by_at(.vars=column1) %>%
    summarize(aire=sum(aire,na.rm=F))%>%
    drop_units %>%
    ungroup()
  areaLCZ1$aire<-round(areaLCZ1$aire/sum(areaLCZ1$aire,na.rm=F)*100,digits = 2)
  areaLCZ1<-areaLCZ1 %>% as.data.frame()

  # marginal for second LCZ
  areaLCZ2<-echInt %>% group_by_at(.vars=column2) %>%
    summarize(aire=sum(aire,na.rm=F))%>%
    drop_units %>%
    ungroup()
  areaLCZ2$aire<-round(areaLCZ2$aire/sum(areaLCZ2$aire,na.rm=F)*100,digits = 2)
  areaLCZ2<-areaLCZ2 %>% as.data.frame()

  # Problem : some of the input files do not exhibit all possible LCZ values :
  # pasting the area to the labels would return an error
  # Here is an ugly solution to overcome this (and see later to include the potentially missing combination of levels)

  aires<-data.frame(niveaux=niveaux, aire1=0, aire2=0)
  print(aires)

  print(head(areaLCZ1[,column1]))

  for (i in areaLCZ1[,column1]){
    aires[aires$niveaux==i,'aire1']<-areaLCZ1[areaLCZ1[,column1]==i,'aire']
  }

  for (i in areaLCZ2[,column2]){
    aires[aires$niveaux==i,'aire2']<-areaLCZ2[areaLCZ2[,column2]==i,'aire']
  }

  # Get the general agreement between both input files
  pourcAcc<-(((echInt %>%  filter(accord==T) %>% select(aire) %>% sum) /
                (echInt %>% select(aire) %>% sum))*100) %>% round(digits=2)

  # the way to "feed" group_by is through .dots, to be checked, as it seems to be deprecated :
  # fixed with group_by_at


  matConf<-echInt %>% group_by_at(.vars=c(column1,column2)) %>%
    summarize(aire=sum(aire))%>% drop_units %>% ungroup %>% ungroup

  print("matConf")
  print(head(matConf))

  matConfLarge<-pivot_wider(data=matConf,names_from=column2,values_from=aire)
  readable<-matConfLarge[,-1]/rowSums(matConfLarge[,-1],na.rm=T)*100
  matConfLarge<-cbind(matConfLarge[,1],round(x=readable,digits=2))

  print("matConfLarge")
  print(head(matConfLarge))


  ###############################################################
  ## A degeomer
  ###############################################################

  # Longer format to feet the geom_tile aes in ggplot2

  matConfLong<-pivot_longer(matConfLarge,cols=-1,names_to = column2)
  print("matConfLong avant reorder factor")
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
                      aire=0
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

  print("matConfLongaprès reorder factor")
  print(matConfLong)
  datatemp<-data.frame(a=factor(niveaux),pourcAire=aires$aire1)
  ############
  # Plot
  coordRef=length(niveaux)+1
  if (repr=='brut'){titrou<-"LCZ"} else {titrou<-"Grouped LCZs"}

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
      geom_tile(datatemp,mapping=aes(x=a,y=coordRef,fill=pourcAire,height=0.8,width=0.8))+
      geom_tile(datatemp,mapping=aes(x=coordRef,y=a,fill=pourcAire,height=0.8,width=0.8))+
      ggtitle(titre4,subtitle=sousTitre)
    print(matConfPlot)} else {matConfPlot=NULL}

  matConfOut<-list(matConf=matConfLong,matConfPlot=matConfPlot,aires=aires,pourcAcc=pourcAcc)
  return(matConfOut)


}


