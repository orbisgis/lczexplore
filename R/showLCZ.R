#' Produces a simple representation of the LCZ contained in an sf file.
#'
#' @param sf is the sf file which contains the LCZ column to be plotted.
#' @param wf is the workflow used to produced the LCZ. "bdt" and "osm" indicate
#' that LCZ were produced by GeoClimate using the BD_TOPO_V2 or the Open Street Map data as input, respectively.
#' If the LCZ were produced by WUDAPT, use "wudapt".
#' @param column is the column that contains the LCZ.
#' @param repr : if "brut" then an optimal set of colors is used to produce the plotted map.
#' If "grouped", colors will be randomly chosen according to the number of groups present in the column.
#'
#' @return no object is returned, but plots of the LCZ levels are produced
#' @export
#' @import sf ggplot2 dplyr cowplot forcats
#' @examples
showLCZ<-function(sf,wf,column,repr){

  #dependancies should be dealt with @import
  # paquets<-c("sf","ggplot2","dplyr","cowplot","forcats")
  # lapply(paquets, require, character.only = TRUE)

  try(class(sf)[1]=="sf", stop("Input data must be sf object"))
  sf<-sf %>% mutate(!!column:=factor(subset(sf,select=column,drop=T)))



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
  CodeCoulLCZ<-read.csv("/home/gousseff/Documents/2_CodesSources/GeoClimate/GeoclimateDefaultCase/Manips_R/CodesCouleursLCZ.csv")  # A remplacer par un fichier embarqué dans le paquet
  colorMap<-CodeCoulLCZ$Hexa.Color.code # création d'un vecteur de couleurs
  names(colorMap)<-CodeCoulLCZ$Type     # attribution de noms qui sont les LCZ type
  }


  # print(paste("traitement avec wf =", wf, "et repr= ",repr,
  #             "et sf est de classe ",
  #             ifelse((wf=="bdt"|wf=="osm"), class(sf$rsu_lcz)[1],class(sf)[1])))

###### Représentations des LCZ Brutes sorties de GeoClimate
  if ((wf=="bdt"|wf=="osm") & (repr=="brut"|repr=="both")){

    print(head(sf[column]))
    wtitre<-ifelse(wf=="bdt","BD_TOPO v2","Open Street Map")

    pBrut<-ggplot(sf) + # les données
        geom_sf(aes(fill=get(column))) +        # Le type de géométrie : ici un sf, avec fill pour remplir les polygones
        scale_fill_manual(values=colorMap,labels=CodeCoulLCZ$Type.definition)+
        ggtitle(paste("LCZ Geoclimate", wtitre))
    #print(pBrut)
  }
  #

###### Représentations des LCZ Geoclimate reclassées en occupation du sol

  if ((wf=="bdt"|wf=="osm") && (repr=="grouped"|repr=="both")){

    niveaux<-levels(subset(sf,select=column,drop=T))
    print(niveaux)
    wtitreOc<-paste(ifelse(wf=="bdt","BD_TOPO v2","Open Street Map"), "grouped")

    pgrouped<-
      ggplot(sf) + # les données
      geom_sf(aes(fill=get(column))) +        # Le type de géométrie : ici un sf, avec fill pour remplir les polygones
      scale_fill_manual(values=colors()[sample(x=1:657,size=length(niveaux))],
                        labels=niveaux)+
      ggtitle(paste("LCZ Geoclimate",wtitreOc))
   #print(pgrouped)
  }

# Représentation des LCZ issues de Wudapt


  if (wf=="wudapt" & repr=="brut"){
    sf$EU_LCZ_map<-codeNivLCZ(sf$EU_LCZ_map,orig="wudapt")

    print(head(sf$EU_LCZ_map))
    wtitre<-"WUDAPT"
    pBrut<-ggplot(sf) + # les données
      geom_sf(aes(fill=EU_LCZ_map)) +        # Le type de géométrie : ici un sf, avec fill pour remplir les polygones
      scale_fill_manual(values=colorMap,labels=CodeCoulLCZ$Type.definition)
    }
  if (wf=="wudapt" & repr=="grouped"){
    niveaux<-levels(subset(sf,select=column,drop=T))
    print(niveaux)
       wtitreOc<-"Grouped classes"
    pgrouped<-ggplot(sf)+
      geom_sf(aes(fill=grouped)) +        # Le type de géométrie : ici un sf, avec fill pour remplir les polygones
      scale_fill_manual(values=colors()[sample(x=1:657,size=length(niveaux))],
                        labels=niveaux)+
      ggtitle(paste("LCZ Wudapt ",wtitreOc))
  }



  if(repr=="both"){plot_grid(pBrut,pgrouped)}
  else {
    if (repr=="brut"){print(pBrut)}
    else {
      if (repr=="grouped"){print(pgrouped)}
      else {stop("the repr argument must be \"brut\", \"grouped\", or a \"both\" ")}
    }
  }
}
