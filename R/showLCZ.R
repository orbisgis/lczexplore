#' Produces a simple representation of the LCZ contained in an sf file.
#'
#' @param sf is the sf file which contains the LCZ column to be plotted.
#' @param wf is the workflow used to produced the LCZ. "bdt" and "osm" indicate
#' that LCZ were produced by GeoClimate using the BD_TOPO_V2 or the Open Street Map data as input, respectively.
#' If the LCZ were produced by WUDAPT, use "wudapt".
#' @param column is the column that contains the LCZ.
#' @param repr indicates if the sf dataset contains brute LCZ levels or grouped LCZ.
#' If "brut" then an optimal set of cols is used to produce the plotted map. Else, colors can be specified with the cols argument.
#' @param niveaux is a vector of strings specifying the name of the expected levels when repr is set to 'grouped'.
#' If \'niveaux\' is set to an empty string, then the unique values taken in the specified \'column\' will be used.
#' @param cols is a vector of strings specifying the colors of each levels of \'niveaux.\'
#' If cols is an empty string, or if the number of specified color is less than the number of levels in \'niveaux\',
#' random colors will be chosen.
#' @param title allows the user to set the title of the plot
#' @import sf ggplot2 dplyr cowplot forcats grDevices
#' @return no object is returned, but plots of the LCZ levels are produced
#' @export
#' @examples showLCZ(redonBDT,column="LCZ_PRIMARY", repr="brut")
showLCZ<-function(sf, title="", wf="",column="LCZ_PRIMARY", repr="brut", niveaux="", cols=""){

  datasetName<-print(deparse(substitute(sf)))
  #dependancies should be dealt with @import
  # paquets<-c("sf","ggplot2","dplyr","cowplot","forcats")
  # lapply(paquets, require, character.only = TRUE)

  try(class(sf)[1]=="sf", stop("Input data must be sf object"))
  # sf<-sf %>% dplyr::mutate(!!column:=factor(subset(sf,select=column,drop=T)))

  if(repr=="brut"){niveaux=as.character(c(1:10,101:107))} else{
    if(repr=='grouped'){
      if(length(niveaux)<=1){
        print("Argument \'niveaux\' is not specified, levels are set to unique values from column")
        niveaux<-sf[[column]] %>% unique
      }else {
        temp<-sf[[column]] %>% unique
        if(length(niveaux)<length(temp)){
          warning("There are more levels in your grouped columns than specified in the \'niveaux\' argument.
                This argument will be ignored and unique values will determine levels")
          niveaux<-temp
          rm(temp)
        } else{
          print("Levels of LCZ will take the values specified in \'niveaux\'")
          }
      }
  }
  }

sf<-sf %>% mutate(!!column:=factor(subset(sf,select=column,drop=T),levels=niveaux))

  #Color style for brut lcz from geoclimate
if (wf!=""){nomLegende<-paste0("LCZ from ",wf," workflow")} else{nomLegende<-"LCZ"}

  if (repr=='brut'){


  colorMap<-c("#8b0101","#cc0200","#fc0001","#be4c03","#ff6602","#ff9856",
              "#fbed08","#bcbcba","#ffcca7","#57555a","#006700","#05aa05",
              "#648423","#bbdb7a","#010101","#fdf6ae","#6d67fd")
  names(colorMap)<-niveaux # color vector Creation
  etiquettes<-c("LCZ 1: Compact high-rise","LCZ 2: Compact mid-rise","LCZ 3: Compact low-rise",
                     "LCZ 4: Open high-rise","LCZ 5: Open mid-rise","LCZ 6: Open low-rise",
                     "LCZ 7: Lightweight low-rise","LCZ 8: Large low-rise",
                     "LCZ 9: Sparsely built","LCZ 10: Heavy industry",
                     "LCZ A: Dense trees", "LCZ B: Scattered trees",
                     "LCZ C: Bush,scrub","LCZ D: Low plants",
                     "LCZ E: Bare rock or paved","LCZ F: Bare soil or sand","LCZ G: Water"
  )     # names of LCZ : would be better to call it from a hidden data file enclosed in the package, will be done later
  if (wf!=""){nomLegende<-paste0("LCZ from ",wf," workflow")} else{nomLegende<-"LCZ"}

  }

###### Shows the geoms with the original values of LCZ as described by Stewardt & Oke, and produced for instance by the GeoClimate workflow
  if (repr=="brut"|repr=="both"){
    print(datasetName)
    #print(head(sf[column]))
      if(title==""){
        if(wf!=""){wtitre<-paste("LCZ from", wf, "workflow, for ", datasetName,"dataset")} else{
        wtitre<-paste("LCZ from", datasetName,"dataset")
        }
      }else{
        wtitre<-title
      }

    pBrut<-ggplot(sf) + # les données
      geom_sf(aes(fill=get(column))) +        # Le type de géométrie : ici un sf, avec fill pour remplir les polygones
      scale_fill_manual(values=colorMap,labels=etiquettes,drop=FALSE)+
      guides(fill=guide_legend(nomLegende))+
      ggtitle(wtitre)
  }
  #

###### Shows LCZ once they are regrouped in more general classes, for instance outputs of the LCZgroup2 function

  if (repr=="grouped"|repr=="both"){
     if(length(cols)>1&&length(cols)==length(niveaux)){
       couleurs<-cols ; names(couleurs)<-niveaux
       nomLegende<-"Grouped LCZ"
     }else{
       if(length(niveaux)>36){
         stop("The number of levels must be less than 37 for the map to be readable,
              you may want to group some of the levels using LCZgroup2 function ")} else {
                if(length(cols)<=1){
                  warning("No cols were specified, cols will be picked from the Polychrome 36 palette")
                  couleurs<-palette.colors(n=length(niveaux), palette="Polychrome 36")
                  names(couleurs)<-niveaux
                  nomLegende<-"Grouped LCZ"
                  } else{
                  if (length(cols)<length(niveaux)){
                    message(paste("you specified less colors in cols argument (here ",length(cols),
                    ") than levels of LCZ in the niveaux argument (here ",length(niveaux),").
                     \n, Maybe you didn't take into account empty levels ?
                     missing cols will be randomly picked from the Polychrom 36 palette."))
                  nMissCol<-length(niveaux)-length(cols)
                  couleurs<-c(cols,palette.colors(n=nMissCol,palette="Polychrome 36"))
                  names(couleurs)<-niveaux
                  warning(paste0(
                    "only ", length(cols), " colors were specified \n for ",
                    length(niveaux)," levels of grouped LCZ \n", nMissCol, " was/were chosen at random. \n ",
                    "For a better rendition, specify as many colors as levels of LCZ"))
                  nomLegende<-"Grouped LCZ"
                      }
                  }
              }
     }

   print(datasetName)
   #print(head(sf[column]))
   if(title==""){
     if(wf!=""){wtitre<-paste("Grouped LCZ for ", wf, "workflow, applied to ", datasetName,"dataset")} else{
       wtitre<-paste("Grouped LCZ from", datasetName," dataset")
     }
   }else{
     wtitre<-title
   }

    pgrouped<-
      ggplot(sf) + # les données
      geom_sf(aes(fill=get(column))) +        # Le type de géométrie : ici un sf, avec fill pour remplir les polygones
      scale_fill_manual(values=couleurs,
                        labels=niveaux,drop=FALSE)+
      guides(fill=guide_legend(nomLegende))+
      ggtitle(wtitre)

  }

# Représentation des LCZ issues de Wudapt


  # if (wf=="wudapt" & repr=="brut"){
  #   sf$EU_LCZ_map<-codeNivLCZ(sf$EU_LCZ_map,orig="wudapt")
  #
  #   print(head(sf$EU_LCZ_map))
  #   wtitre<-"WUDAPT"
  #   pBrut<-ggplot(sf) + # les données
  #     geom_sf(aes(fill=EU_LCZ_map)) +        # Le type de géométrie : ici un sf, avec fill pour remplir les polygones
  #     scale_fill_manual(values=colorMap,labels=etiquettes)
  #   }
  # if (wf=="wudapt" & repr=="grouped"){
  #   niveaux<-levels(subset(sf,select=column,drop=T))
  #   print(niveaux)
  #      wtitreOc<-"Grouped classes"
  #   pgrouped<-ggplot(sf)+
  #     geom_sf(aes(fill=grouped)) +        # Le type de géométrie : ici un sf, avec fill pour remplir les polygones
  #     scale_fill_manual(values=cols()[sample(x=1:657,size=length(niveaux))],
  #                       labels=niveaux)+
  #     ggtitle(paste("LCZ Wudapt ",wtitreOc))
  # }



  if(repr=="both"){plot_grid(pBrut,pgrouped)}
  else {
    if (repr=="brut"){print(pBrut)}
    else {
      if (repr=="grouped"){print(pgrouped)}
      else {stop("the repr argument must be \"brut\", \"grouped\", or a \"both\" ")}
    }
  }
}
