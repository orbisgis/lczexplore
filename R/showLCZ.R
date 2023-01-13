#' Produces a simple representation of the LCZ contained in an sf file.
#'
#' @param sf is the sf file which contains the LCZ column to be plotted.
#' @param wf is the workflow used to produced the LCZ. "bdt" and "osm" indicate
#' that LCZ were produced by GeoClimate using the BD_TOPO_V2 or the Open Street Map data as input, respectively.
#' If the LCZ were produced by WUDAPT, use "wudapt".
#' @param column is the column that contains the LCZ.
#' @param repr : if "brut" then an optimal set of cols is used to produce the plotted map.
#' If "grouped", cols will be randomly chosen according to the number of groups present in the column.
#' @import sf ggplot2 dplyr cowplot forcats
#' @return no object is returned, but plots of the LCZ levels are produced
#' @export
#' @examples showLCZ(redonBDT,column="LCZ_PRIMARY", repr="brut", niveaux="", cols="")
showLCZ<-function(sf, wf="",column="LCZ_PRIMARY", repr="brut", niveaux="", cols=""){

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


  if (repr=='brut'){

  #CodeCoulLCZ<-read.csv("/home/gousseff/Documents/2_CodesSources/GeoClimate/GeoclimateDefaultCase/Manips_R/CodesCouleursLCZ.csv")  # A remplacer par un fichier embarqué dans le paquet
  colorMap<-c("#8b0101","#cc0200","#fc0001","#be4c03","#ff6602","#ff9856",
              "#fbed08","#bcbcba","#ffcca7","#57555a","#006700","#05aa05",
              "#648423","#bbdb7a","#010101","#fdf6ae","#6d67fd")
  names(colorMap)<-niveaux # création d'un vecteur de couleurs
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

###### Représentations des LCZ Brutes sorties de GeoClimate
  if (repr=="brut"|repr=="both"){
    print(datasetName)
    #print(head(sf[column]))
    if(wf!=""){wtitre<-paste("LCZ from", wtitre, "workflow, for ", datasetName,"dataset")} else{
    wtitre<-paste("LCZ from", datasetName,"dataset")
     }


    pBrut<-ggplot(sf) + # les données
      geom_sf(aes(fill=get(column))) +        # Le type de géométrie : ici un sf, avec fill pour remplir les polygones
      scale_fill_manual(values=colorMap,labels=etiquettes,drop=FALSE)+
      guides(fill=guide_legend(nomLegende))+
      ggtitle(wtitre)
  }
  #

###### Représentations des LCZ Geoclimate reclassées en occupation du sol

  if (repr=="grouped"|repr=="both"){

   wtitreOc<-paste(wf,"grouped")

   if(length(cols)>1&&length(cols)==length(niveaux)){
     couleurs<-cols ; names(couleurs)<-niveaux
     nomLegende<-"Grouped LCZ"
   }else{
     if(length(niveaux)>36){
       stop("The number of levels must be less than 37 for the map to be readable,
            you may want to group some of the levels using LCZgroup2 function ")} else {
              if(length(cols)<=1){
                message("No cols were specified, cols will be picked from the Polychrome 36 palette")

                couleurs<-palette.colors(n=length(niveaux),palette="Polychrome 36")

                names(couleurs)<-niveaux
                nomLegende<-"Grouped LCZ \n for a better map \n specify a vector of colors \n in argument \'cols\' "
                } else{
                  if (length(cols)!=length(niveaux)){
                    print(paste("cols argument must contain the same number of cols (here ",length(cols),
                   ") as the number of levels of LCZ (here ",length(niveaux),").
                   \n, Maybe you didn't take into account empty levels ?
                   cols will be randomly picked from the Polychrom 36 palette."))
                    couleurs<-palette.colors(n=length(niveaux),palette="Polychrome 36")
                    names(couleurs)<-niveaux
                    nomLegende<-"Grouped LCZ \n for a better map \n specify as many colors in \'cols\' \n as there are levels in \'niveaux\'"
                    }
                }
            }
   }

    pgrouped<-
      ggplot(sf) + # les données
      geom_sf(aes(fill=get(column))) +        # Le type de géométrie : ici un sf, avec fill pour remplir les polygones
      scale_fill_manual(values=couleurs,
                        labels=niveaux,drop=FALSE)+
      guides(fill=guide_legend(nomLegende))+
      ggtitle(paste("LCZ",wtitreOc))

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
