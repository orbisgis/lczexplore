#' Produces a confusion matrix between two sets of lcz on the same area (geoms do not need to be the same)
#'
#' @details Most of the time this function will not be called directly by the user but by the compareLCZ function
#' @param sf1 is the sf dataset containing the first lcz classification
#' @param column1 is the column of the first data set containing the lcz to be compared
#' @param sf2 is the sf dataset containing the second lcz classification
#' @param column2 is the column of the second data set containing the lcz to be compared
#' @param repr brut allows to use the optimal color scheme to represent the original LCZ.
#' If set to grouped, the ... arguments must contain the name of the groups and
#' the names of the colors associated to these user defined groups
#' @param niveaux by default the levels of lcz incoded from 1 to 10 and 101 to 107.
#' When comparing grouped LCZ, the grouped levels have to be specified.
#' @param plot if TRUE the plot of the matrix
#' @param ...
#'
#' @return returns an object called matConfOut which contains
#' matConfLong, a confusion matrix in a longer form, which can be written in a file by the compareLCZ function
#' and is used by the geom_tile function of the ggplot2 package.
#' matConfPlot is a ggplot2 object showing the confusion matrix. If plot=T, it is also directly plotted
#' aires contains the sums of each LCZ area
#' pourcAcc is the general agreement between the two sets of LCZ, expressed as a percentage of the total area of the study zone
#' @import sf ggplot2 dplyr cowplot forcats units tidyr RColorBrewer

#' @export
#'
#' @examples
matConfLCZ<-function(sf1,column1,sf2,column2,repr="brut",niveaux=as.character(c(1:10,101:107)),plot=F,...){
  # coerce the crs of sf2 to the crs of sf1
  if(st_crs(sf1)!=st_crs(sf2)){sf2<-sf2 %>% st_transform(crs=st_crs(sf1))}

  if(column1==column2){
    column2<-paste0(column1,".1")
    sf2<-sf2 %>% mutate(!!column2:=subset(sf2,select=column1,drop=T))
    sf2<-sf2 %>% mutate(!!column1:=NULL)
  }

  sf1<-sf1 %>% mutate(!!column1:=factor(subset(sf1,select=column1,drop=T),levels=niveaux))%>% drop_na(column1)
  sf2<-sf2 %>% mutate(!!column2:=factor(subset(sf2,select=column2,drop=T),levels=niveaux))%>% drop_na(column2)

  # creation of the data set with intersected geoms and the values of both lcz class in these geoms
  echInt<-st_intersection(x=sf1[column1],y=sf2[column2])
  # checks if the two LCZ classifications agree
  echInt$accord<-subset(echInt,select=column1,drop=T)==subset(echInt,select=column2,drop=T)



  ######################################################
  ###
  ### Confusion matrix, weights being the area of the intersecting geoms
  ###
  ######################################################

  # compute the area of geoms (used later as wieght of agreement between classifcations)
  echInt<-echInt %>% mutate(aire=st_area(geometry)) %>% drop_units

  # the writing/appending will happen in compareLCZ function

  # marginal areas (grouped by levels of LCZ for each input dataset) rounded at the unit
    # marginal for first LCZ
   areaLCZ1<-echInt %>% st_drop_geometry %>% group_by_at(.vars=column1) %>%
    summarize(aire=sum(aire,na.rm=F))%>%
    drop_units %>%
    ungroup()
  areaLCZ1$aire<-round(areaLCZ1$aire/sum(areaLCZ1$aire,na.rm=F)*100,digits = 2)

    # marginal for second LCZ
  areaLCZ2<-echInt %>% st_drop_geometry %>% group_by_at(.vars=column2) %>%
    summarize(aire=sum(aire,na.rm=F))%>%
    drop_units %>%
    ungroup()
  areaLCZ2$aire<-round(areaLCZ2$aire/sum(areaLCZ2$aire,na.rm=F)*100,digits = 2)

  # Problem : some of the input files do not exhibit all possible LCZ values :
  # pasting the area to the labels would return an error
  # Here is an ugly solution to overcome this (and see later to include the potentially missing combination of levels)

  aires<-data.frame(niveaux=niveaux, aire1=0, aire2=0)


  for (i in subset(areaLCZ1,select=column1,drop=T)){
    if (!is.na(i)){
      aires[aires$niveaux==i,'aire1']<-areaLCZ1[subset(areaLCZ1,select=column1,drop=T)==i,'aire']
    }
  }

  for (i in subset(areaLCZ2,select=column2,drop=T)){
    if (!is.na(i)){
      aires[aires$niveaux==i,'aire2']<-areaLCZ2[subset(areaLCZ2,select=column2,drop=T)==i,'aire']
      #testprov<-areaLCZ1[subset(areaLCZ1,select=column1,drop=T)==i,'aire']
    }
  }

  # Get the general agreement between both input files
  pourcAcc<-(((echInt %>% st_drop_geometry() %>% filter(accord==T) %>% select(aire) %>% sum) /
                (echInt %>% st_drop_geometry()%>% select(aire) %>% sum))*100) %>% round(digits=2)

  # the way to "feed" group_by is through .dots, to be checked, as it seems to be deprecated :
  # fixed with group_by_at


  matConf<-echInt %>% st_drop_geometry %>% group_by_at(.vars=c(column1,column2)) %>%
    summarize(aire=sum(aire))%>% drop_units %>% ungroup %>% ungroup

  print("matConf")
  print(head(matConf))

  # Wider format to compute area based confusion

  # This "confusion matrix" contains the area of intersected geom who have the i LCZ
  # for the first dataset and the j LCZ for the second dataset
  matConfLarge<-pivot_wider(data=matConf,names_from=column2,values_from=aire)
  readable<-matConfLarge[,-1]/rowSums(matConfLarge[,-1],na.rm=T)*100
  matConfLarge<-cbind(matConfLarge[,1],round(x=readable,digits=2))

  print("matConfLarge")
  print(head(matConfLarge))


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
    geom_tile(datatemp,mapping=aes(x=a,y=coordRef,fill=pourcAire,height=0.8))+
    geom_tile(datatemp,mapping=aes(x=coordRef,y=a,fill=pourcAire,height=0.8))+
    ggtitle("Repartition of Reference classes into alternative classes")
    print(matConfPlot)} else {matConfPlot=NULL}

  matConfOut<-list(matConf=matConfLong,matConfPlot=matConfPlot,aires=aires,pourcAcc=pourcAcc)
  return(matConfOut)

}

