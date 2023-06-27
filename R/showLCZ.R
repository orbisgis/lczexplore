#' Produces a simple representation of the LCZ contained in an sf file.
#'
#' @param sf is the sf file which contains the LCZ column to be plotted.
#' @param wf is the workflow used to produced the LCZ. "bdt" and "osm" indicate
#' that LCZ were produced by GeoClimate using the BD_TOPO_V2 or the Open Street Map data as input, respectively.
#' If the LCZ were produced by WUDAPT, use "wudapt".
#' @param column is the column that contains the LCZ.
#' @param repr indicates if the sf dataset contains standard LCZ levels, 
#' or alternative values, like grouped LCZ.
#' If "standard" then an optimal set of cols is used to produce the plotted map. 
#' Else, colors can be specified with the cols argument.
#' 
#' The values must at least cover the values of the column in the dataset, or it will be ignored.
#' @param title allows the user to set the title of the plot
#' @param drop indicates if you want to show the levels present in no geometry.
#' @param useStandCol is set to TRUE implies that any levels detected as a standard LCZ level will receive the standard associated color
#' @param ... these dynamic dots allow you to pass arguments to specify levels expected in your dataset and colors associated to these levels
#' @import sf ggplot2 dplyr cowplot forcats grDevices
#' @return no object is returned, but plots of the LCZ levels are produced
#' @export
#' @examples # On original LCZ levels, use the \'standard\' value for the \'repr\' argument.
#' showLCZ(redonBDT,column="LCZ_PRIMARY", repr="standard")
#' # On grouped data, use the alter value for the repr argument.
#' redonBDTgrouped<-LCZgroup2(redonBDT,column="LCZ_PRIMARY",
#' urban=c("1","2","3","4","5","6","7","8","9"),
#' industry="10", vegetation=c("101","102","103","104"),
#' impervious="105",pervious="106",water="107")
#' showLCZ(redonBDTgrouped,column="grouped",repr="alter",
#' LCZlevels=c("urban","industry","vegetation","impervious","pervious","water"),
#' cols=c("red","black","green","grey","burlywood","blue"),wf="BD TOPO")
#' 
showLCZ<-function(sf, title="", wf="",column="LCZ_PRIMARY",
                  repr="standard", drop=FALSE, useStandCol=TRUE,...){

  datasetName<-deparse(substitute(sf))

  try(class(sf)[1]=="sf", stop("Input data must be sf object"))

 if(wf!=""){nomLegende<-paste0("LCZ from ",wf," workflow")} else{nomLegende<-"LCZ"}

  if (repr=='standard'){
  LCZlevels<-as.character(c(1:10,101:107))
  temp<-subset(sf,select=column,drop=T) %>% fct_recode("Compact high"="1",
                                                            "Compact mid"="2",
                                                            "Compact low"="3",
                                                            "Open high"="4",
                                                            "Open mid"="5",
                                                            "Open low"="6",
                                                            "Lightweight low"="7",
                                                            "Large low"="8",
                                                            "Sparsely built"="9",
                                                            "Heavy industry"="10",
                                                            "Dense trees"="101",
                                                            "Scattered trees"="102",
                                                            "Bush scrub"="103",
                                                            "Low plants"="104",
                                                            "Bare rock or paved"="105",
                                                            "Bare soil sand"="106",
                                                            "Water"="107")

  sf<-sf %>% mutate(!!column:=temp)

  colorMap<-c("#8b0101","#cc0200","#fc0001","#be4c03","#ff6602","#ff9856",
                "#fbed08","#bcbcba","#ffcca7","#57555a","#006700","#05aa05",
                "#648423","#bbdb7a","#010101","#fdf6ae","#6d67fd")
  typeLevels<-colorMap
  names(typeLevels)<-levels(subset(sf,select=column,drop=T))
  areas<-LCZareas(sf,column,LCZlevels=names(typeLevels))
  etiquettes<-paste(c("LCZ 1: Compact high-rise","LCZ 2: Compact mid-rise","LCZ 3: Compact low-rise",
                "LCZ 4: Open high-rise","LCZ 5: Open mid-rise","LCZ 6: Open low-rise",
                "LCZ 7: Lightweight low-rise","LCZ 8: Large low-rise",
                "LCZ 9: Sparsely built","LCZ 10: Heavy industry",
                "LCZ A: Dense trees", "LCZ B: Scattered trees",
                "LCZ C: Bush,scrub","LCZ D: Low plants",
                "LCZ E: Bare rock or paved","LCZ F: Bare soil or sand","LCZ G: Water"),": ", areas$area, "%")

  if (wf!=""){nomLegende<-paste0("LCZ from ",wf," workflow")} else{nomLegende<-"LCZ"}

###### Shows the geoms with the original values of LCZ as described by Stewardt & Oke, and produced for instance by the GeoClimate workflow

      if(title==""){
        if(wf!=""){wtitre<-paste("LCZ from", wf, "workflow, for ", datasetName,"dataset")} else{
        wtitre<-paste("LCZ from", datasetName,"dataset")
        }
      }else{
        wtitre<-title
      }

    if(drop==TRUE){
      presentLevels<-levels(droplevels(subset(sf,select=column,drop=T)))
      temp<-subset(sf,select=column,drop=T) %>% 
        factor(levels=presentLevels)
      print(levels(temp))
      sf<-sf %>% mutate(!!column:=temp)
      presentIndices<-sapply(presentLevels,grep,x=etiquettes) %>% unlist %>% print
      colorMap<-colorMap[presentIndices]
      etiquettes<-etiquettes[presentIndices]
      }
    
    pstandard<-ggplot(sf) + # data
      geom_sf(data=sf,aes(fill=get(column)))+
      scale_fill_manual(values=colorMap,labels=etiquettes)+
      guides(fill=guide_legend(nomLegende))+
      ggtitle(wtitre)
  }
  #

###### Shows other qualitative variables, like LCZ once they are regrouped in more general classes, 
  # for instance outputs of the LCZgroup2 function.

  if (repr=="alter"){
    print(datasetName)

    typeLevels<-levCol(sf=sf,column=column,...)$levelsColors
    print("output of levCol")
    print(typeLevels)
    
    # if(length(LCZlevels)==1 && LCZlevels[1]=="" && length(cols)==1){
    #   typeLevels<-levCol(sf,column, drop=drop)$levelsColors
    # } else if (length(LCZlevels)==1 & LCZlevels[1]==""){
    #   typeLevels<-levCol(sf,column,cols=cols, drop=drop)$levelsColors}
    # else if (length(cols)==1 & cols[1]==""){
    #   typeLevels<-levCol(sf,column,levels=LCZlevels, drop=drop)$levelsColors
    # }
    # else {typeLevels<-levCol(sf,column,levels=LCZlevels, cols=cols, drop=drop)$levelsColors }

    # IN CAS ESOME STANDARD LEVELS ARE DETECTED, ONE MAY WANT STANDARD COLORS TO BE APPLIED

    if(useStandCol==TRUE){typeLevels<-standLevCol(levels=names(typeLevels),colors=typeLevels,useStandCol = TRUE)}

    # if ( length(LCZlevels)<=1){
    # typeLevels<-levCol(sf,column)$levelsColors
    # }
    # if(length(cols)==length(typeLevels)){typeLevels[1:length(typeLevels)]<-cols}

    LCZlevels<-names(typeLevels)
    sf<-sf %>% mutate(!!column:=factor(subset(sf,select=column,drop=T),levels=LCZlevels))
    areas<-LCZareas(sf,column,LCZlevels=names(typeLevels))
    etiquettes<-paste(LCZlevels,": ",areas$area,"%")

    
   if(title=="") {
     if(wf!=""){wtitre<-paste("Grouped LCZ for ", wf, "workflow, applied to ", datasetName,"dataset")} else {
       wtitre<-paste("Grouped LCZ from", datasetName," dataset")
     }
   } else {
     wtitre<-title
   }

    palter<-
      ggplot(sf) + # les données
      geom_sf(aes(fill=get(column))) +        # Le type de géométrie : ici un sf, avec fill pour remplir les polygones
      scale_fill_manual(values=typeLevels,
                        labels=etiquettes,drop=FALSE)+
      guides(fill=guide_legend(nomLegende))+
      ggtitle(wtitre)
 }

  if (repr=="standard"){print(pstandard)}
    else {
      if (repr=="alter"){print(palter)}
      else {stop("the repr argument must be \"standard\" or \"alter\" ")}
    }
  
}