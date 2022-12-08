#' Produces a simple representation of the LCZ contained in an sf file.
#'
#' @param sf is the sf file which contains the LCZ column to be plotted.
#' @param wf is the workflow used to produced the LCZ. "bdt" and "osm" indicate
#' that LCZ were produced by GeoClimate using the BD_TOPO_V2 or the Open Street Map data as input, respectively.
#' If the LCZ were produced by WUDAPT, use "wudapt".
#' @param column is the column that contains the LCZ.
#' @param repr : if "brut" then an optimal set of colors is used to produce the plotted map.
#' If "grouped", colors will be randomly chosen according to the number of groups present in the column.
#' @import sf ggplot2 dplyr cowplot forcats
#' @return no object is returned, but plots of the LCZ levels are produced
#' @export
#' @examples
showLCZ<-function(sf,wf,column,repr,niveaux="",colors=""){

  #dependancies should be dealt with @import
  # paquets<-c("sf","ggplot2","dplyr","cowplot","forcats")
  # lapply(paquets, require, character.only = TRUE)

  try(class(sf)[1]=="sf", stop("Input data must be sf object"))
  sf<-sf %>% dplyr::mutate(!!column:=factor(subset(sf,select=column,drop=T)))



  #require("sf","ggplot2","dplyr","cowplot","forcats")
  # sf = l'objet sf dans lequel on a importé la sortie de geoclimate
  # wf = le workflow qui a généré les données : bdt=Geoclimate BDT, osm=GeoclimateOSM, wpt=WUDAPT
  # repr = le type de représentation qu'on veut : brut=lcz brutes, grouped=réencodé
  # entre urbain, industrie, vegetation, sol impermeable, sol permeable et eau, both=les deux
  # LE CHOIX ACTUEL EST D'ENTRER DES OBJETS ISSUS DE L'IMPORT DIRECT DE GEOCLIMATE,
  # IL FAUDRA PEUT-ÊTRE REVOIR CETTE APPROCHE ET SE RAMENER À LA REPRESENTATION DE GEOM+lcz UNIQUEMENT

  #Color style for brut lcz from geoclimate
  # Voir comment on attache un jeu de données à un package et mettre le chemin relatif qui va bien

  if (repr=='brut'){
  #CodeCoulLCZ<-read.csv("/home/gousseff/Documents/2_CodesSources/GeoClimate/GeoclimateDefaultCase/Manips_R/CodesCouleursLCZ.csv")  # A remplacer par un fichier embarqué dans le paquet
  colorMap<-c("#8b0101","#cc0200","#fc0001","#be4c03","#ff6602","#ff9856",
              "#fbed08","#bcbcba","#ffcca7","#57555a","#006700","#05aa05",
              "#648423","#bbdb7a","#010101","#fdf6ae","#6d67fd")
  names(colorMap)<-as.character(c(1:10,101:107)) # création d'un vecteur de couleurs
  etiquettes<-c("LCZ 1: Compact high-rise","LCZ 2: Compact mid-rise","LCZ 3: Compact low-rise",
                     "LCZ 4: Open high-rise","LCZ 5: Open mid-rise","LCZ 6: Open low-rise",
                     "LCZ 7: Lightweight low-rise","LCZ 8: Large low-rise",
                     "LCZ 9: Sparsely built","LCZ 10: Heavy industry",
                     "LCZ A: Dense trees", "LCZ B: Scattered trees",
                     "LCZ C: Bush,scrub","LCZ D: Low plants",
                     "LCZ E: Bare rock or paved","LCZ F: Bare soil or sand","LCZ G: Water"
  )     # attribution de noms qui sont les LCZ type

  }

###### Représentations des LCZ Brutes sorties de GeoClimate
  if (repr=="brut"|repr=="both"){

    print(head(sf[column]))
    wtitre<-wf

    pBrut<-ggplot(sf) + # les données
      geom_sf(aes(fill=get(column))) +        # Le type de géométrie : ici un sf, avec fill pour remplir les polygones
      scale_fill_manual(values=colorMap,labels=etiquettes)+
      ggtitle(paste("LCZ Geoclimate from", wtitre))
    #print(pBrut)
  }
  #

###### Représentations des LCZ Geoclimate reclassées en occupation du sol

  if (repr=="grouped"|repr=="both"){


    if (length(niveaux)==1&&niveaux==""){
    niveaux<-levels(subset(sf,select=column,drop=T))
    print(niveaux)
    }

    wtitreOc<-paste(wf,"grouped")

    if(length(colors)==1 && colors==""){couleurs<-sample(x=1:657,size=length(niveaux))} else{
      if (length(colors)!=length(niveaux))
      {print(paste("colors argument must contain the same number of colors (here ",length(colors),
                   ") as the number of levels of LCZ (here ",length(niveaux),"). \n, Maybe you didn't take into account empty levels ? Colors will be randomly picked from the colors you submitted.
               If this number is unknown, leave colors=\"\" and random colors will be picked for you"))
        couleurs<-colors ; names(couleurs)<-niveaux} else{
                 couleurs<-colors ; names(couleurs)<-niveaux
               }
    }


    pgrouped<-
      ggplot(sf) + # les données
      geom_sf(aes(fill=get(column))) +        # Le type de géométrie : ici un sf, avec fill pour remplir les polygones
      scale_fill_manual(values=couleurs,
                        labels=niveaux)+
      ggtitle(paste("LCZ",wtitreOc))
   #print(pgrouped)
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
  #     scale_fill_manual(values=colors()[sample(x=1:657,size=length(niveaux))],
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
