#' Compares two LCZ classification on the same zone,
#' produces a map for each classification, a map of their agreement and a representation of a confusion matric between them
#'
#' @param sf1 is the sf object that contains the first LCZ classification
#' @param geomID1 is the name of the column storing the ID of the geoms in sf1
#' @param column1 is the column of sf1 that contains the LCZ classification for each geom of sf1.
#' By defautl it is set to an empty string and no ID is loaded.
#' @param confid1 is a column that contains an indicator of confidence
#' of the level of the LCZ in column 1, e.g. a uniqueness value, or a probability of belonging to the class...
#' By defautl it is set to an empty string and no confidence indicator is loaded.
#' @param wf1 is the workflow used to produce the first LCZ classification.
#' When GeoClimate was used with BD_TOPO V2 data as input,
#' use "bdtopo_2_2". When GeoClimate was used with Open Street Map data as input, use "osm".
#' When the LCZ come from the wudapt Europe tiff, use "wudapt".
#' @param sf2 is the sf object that contains the second LCZ classification
#' @param geomID2 is the name of the column storing the ID of the geoms in sf2
#' @param column2 is the column of sf2 that contains the LCZ classification for each geom of sf2
#' @param confid2 is a column that contains an indicator of confidence
#' of the level of the LCZ in column 2, e.g. a uniqueness value, or a probability of belonging to the class...
#' By defautl it is set to an empty string and no confidence indicator is loaded.
#' @param wf2 is the workflow used to produce the second LCZ classification.
#' When GeoClimate was used with BD_TOPO V2 data as input,
#' use "bdtopo_2_2". When GeoClimate was used with Open Street Map data as input, use "osm".
#' When the LCZ come from the wudapt Europe tiff, use "wudapt".
#' @param ref : If the coordinate reference system (CRS) of sf1 and sf2 differ, ref indicates which CRS to choose for both files (1 or 2)
#' @param repr "brut" means that original values of LCZ are used,
#'  "grouped" means the user has groupe some of the LCZ levels under a new label.
#'  In the latter case, the ... arguments must contain the groups and a color vector.
#' @param plot : when FALSE non of the graphics are plotted or saved
#' @param saveG : when an empty character string, "", the plots are not saved. Else, the saveG string is used to produce the name of the saved png file.
#' @param location : the name of the study area, as chosen as the name of the directory on the GeoClimate team cloud.
#' If the area you wish to analyse is not uploaded yet, please contact the GeoClimate Team.
#' @param exwrite : when TRUE, the values of the LCZ on the intersected geoms are written down in a csv file
#' @param ... allow to pass arguments if representation is grouped.
#' The expected arguments are the name of each grouped label,
#' the levels of LCZ they contain, and last a vector of the colors to use to plot them.
#' @import sf ggplot2 dplyr cowplot forcats units tidyr RColorBrewer utils
#' @return returns an object called matConfOut which contains
#' matConfLong, a confusion matrix in a longer form, which can be written in a file by the compareLCZ function
#' and is used by the geom_tile function of the ggplot2 package.
#' matConfPlot is a ggplot2 object showing the confusion matrix. If plot=T, it is also directly plotted
#' aires contains the sums of each LCZ area
#' pourcAcc is the general agreement between the two sets of LCZ, expressed as a percentage of the total area of the study zone
#' If saveG is not an empty string, graphics are saved under "saveG.png"
#' @export
#' @examples
compareLCZ<-function(sf1,geomID1="",column1,confid1="",wf1="bdtopo_2_2",
                     sf2,column2,geomID2="",confid2="",wf2="osm",ref=1,
                     repr="brut",saveG="",exwrite=TRUE,location="Redon", plot=TRUE, ...){

  # dependancies dealt with @import through roxygen
  #paquets<-c("sf","ggplot2","dplyr","cowplot","forcats","units","tidyr","RColorBrewer")
  #lapply(paquets, require, character.only = TRUE)

  # if (!(wf1=="osm"||wf1=="bdtopo_2_2"||wf1=="wudapt")|| !(wf2=="osm"||wf2=="bdtopo_2_2"||wf2=="wudapt"))
  #   stop("Workflow parameters wf1 and wf2 muste be one of bdtopo_2_2, osm or wudapt")


  # store the column names in a way that can be injected in functions A SUPPRIMER ?
  namesf1<-deparse(substitute(sf1))
  namesf2<-deparse(substitute(sf2))
  message(paste(" The column ",column1, " of the dataset", namesf1,
              "is the reference against which the ",column2,
              " column of the dataset ", namesf2, "will be compared."))

  # handling of different crs for the two datasets
  if(st_crs(sf1)!=st_crs(sf2)){
        # if (ref!=""){crsOpt=ref} else {
         message("Both sf datasets need to live in the same crs projection (srid / epsg),")

     if(ref==1){
       message(paste0("they will be coerced to the specified reference (",namesf1,")"))
       sf2<-sf2 %>% st_transform(crs=st_crs(sf1))}
         else if(ref==2){
           message(paste0("they will be coerced to the specified reference (",namesf2,")"))
           sf1<-sf1 %>% st_transform(crs=st_crs(sf2))}
              else {
                warning("the ref argument is unclear (should be either 1 or 2), so the files will be coerced to the crs of",namesf1," file")
                sf2<-sf2 %>% st_transform(crs=st_crs(sf1))
              }
    # }
  }


  # In order not to "carry" the whole file, keep only the column of interest (LCZ)

  # If LCZ name is the same in both input sf files we rename column2
  if(column1==column2){
    column2<-paste0(column1,".1")
    sf2<-sf2 %>% mutate(!!column2:=subset(sf2,select=column1,drop=T))
    sf2<-sf2 %>% mutate(!!column1:=NULL)
  }
  # If geomID name is the same in both input sf files we rename column2

  if(geomID1!="" && geomID1==geomID2){
    geomID2<-paste0(geomID1,".1")
    sf2<-sf2 %>% mutate(!!geomID2:=subset(sf2,select=geomID1,drop=T))
    sf2<-sf2 %>% mutate(!!geomID1:=NULL)
  }
  # If confid name is the same in both input sf files we rename column2

  if(confid1!="" && confid1==confid2){
    confid2<-paste0(confid1,".1")
    sf2<-sf2 %>% mutate(!!confid2:=subset(sf2,select=confid1,drop=T))
    sf2<-sf2 %>% mutate(!!confid1:=NULL)
  }




  # Check the input file class

  try(class(sf1)[1]=="sf", stop("Input data must be sf objects"))
  try(class(sf1)[1]=="sf", stop("Input data must be sf objects"))
  try(st_crs(sf1)==st_crs(sf2),stop("Input sf objects must have the same SRID"))


 nom1<-c(geomID1,column1,confid1)
 nom1<-nom1[sapply(nom1,nchar)!=0]
 nom2<-c(geomID2,column2,confid2)
 nom2<-nom2[sapply(nom2,nchar)!=0]


 sf1<-select(sf1,nom1) %>% drop_na(column1)
 sf2<-select(sf2,nom2) %>% drop_na(column2)
 # Prepare the levels of the expected LCZ

  if(repr=="brut"){
      #CodeCoulLCZ<-read.csv("/home/gousseff/Documents/2_CodesSources/GeoClimate/GeoclimateDefaultCase/Manips_R/CodesCouleursLCZ.csv")  # A remplacer par un fichier embarqué dans le paquet
      #colorMap<-CodeCoulLCZ$Hexa.Color.code
      #valeurs<-colorMap

      valeurs<-c("#8b0101","#cc0200","#fc0001","#be4c03","#ff6602","#ff9856",
                  "#fbed08","#bcbcba","#ffcca7","#57555a","#006700","#05aa05",
                  "#648423","#bbdb7a","#010101","#fdf6ae","#6d67fd")
      #valeurs=colorMap
      #etiquettes=CodeCoulLCZ$Type.definition
      etiquettes<-c("LCZ 1: Compact high-rise","LCZ 2: Compact mid-rise","LCZ 3: Compact low-rise",
                    "LCZ 4: Open high-rise","LCZ 5: Open mid-rise","LCZ 6: Open low-rise",
                    "LCZ 7: Lightweight low-rise","LCZ 8: Large low-rise",
                    "LCZ 9: Sparsely built","LCZ 10: Heavy industry",
                    "LCZ A: Dense trees", "LCZ B: Scattered trees",
                    "LCZ C: Bush,scrub","LCZ D: Low plants",
                    "LCZ E: Bare rock or paved","LCZ F: Bare soil or sand","LCZ G: Water"
                    )

      niveaux<-as.character(c(1:10,101:107))
      #names(niveaux)<-niveaux
      # Classification must be encoded as factors
      sf1<-sf1 %>% mutate(!!column1:=factor(subset(sf1,select=column1,drop=T),levels=niveaux))
      sf2<-sf2 %>% mutate(!!column2:=factor(subset(sf2,select=column2,drop=T),levels=niveaux))
      temp1<-subset(sf1,select=column1,drop=T) %>% fct_recode(
        "Compact high"="1",
        "Compact mid"="2",
        "Compact low"="3",
        "Open High"="4",
        "Open mid"="5",
        "Open low"="6",
        "Lightweight low"="7",
        "Large low"="8",
        "Sparsely Built"="9",
        "Heavy industry"="10",
        "Dense trees"="101",
        "Scattered trees"="102",
        "Bush scrub"="103",
        "Low plants"="104",
        "Bare rock paved"="105",
        "Bare soil sand"="106",
        "Water"="107")
      sf1<-sf1 %>% mutate(!!column1:=temp1)

      temp2<-subset(sf2,select=column2,drop=T) %>% fct_recode("Compact high"="1",
                                                              "Compact mid"="2",
                                                              "Compact low"="3",
                                                              "Open High"="4",
                                                              "Open mid"="5",
                                                              "Open low"="6",
                                                              "Lightweight low"="7",
                                                              "Large low"="8",
                                                              "Sparsely Built"="9",
                                                              "Heavy industry"="10",
                                                              "Dense trees"="101",
                                                              "Scattered trees"="102",
                                                              "Bush scrub"="103",
                                                              "Low plants"="104",
                                                              "Bare rock paved"="105",
                                                              "Bare soil sand"="106",
                                                              "Water"="107")
      sf2<-sf2 %>% mutate(!!column2:=temp2)
      niveaux<-c("Compact high",
                 "Compact mid",
                 "Compact low",
                 "Open High",
                 "Open mid",
                 "Open low",
                 "Lightweight low",
                 "Large low",
                 "Sparsely Built",
                 "Heavy industry",
                 "Dense trees",
                 "Scattered trees",
                 "Bush scrub",
                 "Low plants",
                 "Bare rock paved",
                 "Bare soil sand",
                 "Water")
  names(valeurs)<-niveaux
rm(temp1) ; rm(temp2)
  }



  if(repr=="grouped"){

    # Generate colors to plot grouped values, according to the number of levels of grouped
      # generate palette
      # get the name of colors, specified by user in the (produceAnalysis function)

    argums<-list(...)
    indSep<-names(argums)
    indCol<-grep(x=indSep,pattern="col")

      if (is.null(indCol)){
        niveaux<-names(argums) #no color passed in arguments, the name of the grouped lcz are the names of the arguments passed in ...
          qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
        col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
        set.seed(1)
        longueur<-length(niveaux)
        valeurs<-col_vector[sample(1:longueur,longueur,replace=F)]
        names(valeurs)<-niveaux
        etiquettes<-niveaux
        set.seed(NULL)}

              else{
          niveaux<-argums[indSep[-indCol]] %>% names
          valeurs<-argums[indSep[indCol]] %>% unlist() %>% as.vector()
          # print("niveaux");print(niveaux);print("valeurs");print(valeurs)
          names(valeurs)<-niveaux
          etiquettes<-niveaux
      }
    niveaux
    # Classification must be encoded as factors
    sf1<-sf1 %>% mutate(!!column1:=factor(subset(sf1,select=column1,drop=T),levels=niveaux))
    sf2<-sf2 %>% mutate(!!column2:=factor(subset(sf2,select=column2,drop=T),levels=niveaux))
  }




  ######################################################
   # # Intersect geometries of both files
  ######################################################
    #intersection of geometries
  echInt<-st_intersection(x=sf1[nom1],y=sf2[nom2])
    # checks if the two LCZ classifications agree
  echInt$accord<-subset(echInt,select=column1,drop=T)==subset(echInt,select=column2,drop=T)


######################################################
###
### Confusion matrix, weights being the area of the intersecting geoms
###
######################################################

  # Export of lcz and area for each geom for further analysis

        echInt<-echInt %>% mutate(aire=st_area(geometry)) %>% drop_units
          # print("echInt")
          # print(head(echInt))
        #echInt
        echIntExpo<-echInt %>% mutate(location=location,aire=as.numeric(aire)) %>%
          st_set_geometry(NULL) %>% as.data.frame()


        nom<-paste0(wf1,"_",wf2,".csv")
        print(paste0("Comparison data will be appended to the following file : ",nom))
        filePath<-paste0(getwd(),"/",nom)

        if (exwrite==TRUE){
        if (!file.exists(filePath)){
        write.table(x=echIntExpo, file =nom, append = TRUE, quote = TRUE, sep = ";",
                    eol = "\n", na = "NA", dec = ".",
                   qmethod = c("escape", "double"),
                    fileEncoding = "", col.names=TRUE,row.names=F)
        }else{
          write.table(x=echIntExpo, file =nom, append = TRUE, quote = TRUE, sep = ";",
                      eol = "\n", na = "NA", dec = ".",
                      qmethod = c("escape", "double"),
                      fileEncoding = "", col.names=FALSE,row.names=F)
        }
        }
###################################################
# Confusion Matrix
###################################################

matConfOut<-matConfLCZ(sf1=sf1,column1=column1,sf2=sf2,column2=column2,repr=repr,niveaux=niveaux,plot=FALSE)
matConfOut$data<-echIntExpo
matConfLong<-matConfOut$matConf
aires<-matConfOut$aires
pourcAcc<-matConfOut$pourcAcc

################################################
#  GRAPHICS
################################################
if (plot == TRUE){
  if (repr=='brut'){titrou<-"LCZ"} else {titrou<-"Grouped LCZs"}

  if (wf1=="bdtopo_2_2"){adtitre1<-" BDTOPO V2.2"} else
    if(wf1=="osm"){adtitre1<-" OSM "} else
      if(wf1=="wudapt"){adtitre1<-" WUDAPT"}else{adtitre1<-wf1}


  if (wf2=="bdtopo_2_2"){adtitre2<-" BDTOPO V2.2"} else
    if(wf2=="osm"){adtitre2<-" OSM "} else
      if(wf2=="wudapt"){adtitre2<-" WUDAPT"}else{adtitre2<-wf2}


  titre1<-paste(titrou,"from ", adtitre1)
  titre2<-paste(titrou,"from", adtitre2)
  titre3<-"Agreement between classifications"
  titre4<-paste(" Repartition of", adtitre1, " LCZ into LCZs of", adtitre2)

  lab1<-paste(titrou, adtitre1)
  lab2<-paste(titrou, adtitre2)
  # ypos<-if (repr=="brut"){ypos=5} else {ypos=2}
  etiquettes1<-paste(etiquettes, aires$aire1 ," %")
  etiquettes2<-etiquettes
  etiquettes2<-gsub(":.*",": ",etiquettes)
  etiquettes2<-paste(etiquettes2, aires$aire2 ," %")
  etiquettes1.2<-paste(etiquettes2,aires$aire1)
  datatemp<-data.frame(a=factor(niveaux),pourcAire=aires$aire1,pourcAire1=aires$aire1,pourcAire2=aires$aire2)

  boundary1<-sf1 %>% st_union %>% st_boundary()
  centro<-st_centroid(boundary1)
  boundary1<-boundary1 %>% st_cast("POINT")
  dist1<-st_distance(boundary1,centro) %>% max
  boundary<-st_buffer(x=centro,dist=dist1) %>% st_make_grid(n=1)

  nbgeom1<-nrow(sf1)
  nbgeom2<-nrow(sf2)
  nbgeomInter<-nrow(echInt)

  # Plot the first classification
     l1Plot<- ggplot(boundary)+ # les données
          geom_sf(data=boundary, fill=NA,lty='blank')+
     geom_sf(data=sf1,aes(fill=get(column1)),lwd=0)+
     scale_fill_manual(values=valeurs,labels=etiquettes1)+
     guides(fill=guide_legend(title=titrou))+
     ggtitle(titre1, subtitle=paste0("Number of RSU : ",nbgeom1))
     #


  # Plot the second classification
     l2Plot<-ggplot(boundary)+
      geom_sf(data=boundary, fill=NA,lty='blank')+
      geom_sf(data=sf2,aes(fill=get(column2)),lwd=0)+
      scale_fill_manual(values=valeurs,labels=etiquettes2)+
      guides(fill=guide_legend(title=titrou))+
      ggtitle(titre2,subtitle=paste0("Number of RSU : ",nbgeom2))

  # Plot areas where classifications agree
     accordPlot<-ggplot(boundary)+
       geom_sf(data=boundary, fill=NA,lty='blank')+
       geom_sf(data=echInt,aes(fill=accord),lwd=0)+
       scale_fill_manual(values=c("red","green"),
                         name=paste0(
                           "The two classifications agree for \n ",pourcAcc, " % of the area Agreement"))+
       ggtitle(label=titre3, subtitle=paste0("Number of intersected geoms : ", nbgeomInter))

  # Plot how the LCZ each level of the first classification is split into levels of the second classification
     coordRef=length(niveaux)+1

     matConfPlot<-ggplot(data = matConfLong, aes(x=get(column1), y=get(column2), fill =accord)) +
       geom_tile(color = "white",lwd=1.2,linetype=1)+
       labs(x=lab1,y=lab2)+
       scale_fill_gradient2(low = "lightgrey", mid="cyan", high = "blue",
                         midpoint = 50, limit = c(0,100), space = "Lab",
                         name="% area") +
       geom_text(data=matConfLong[matConfLong$accord!=0,],aes(label=round(accord,digits=0)),
                 color="black") +coord_fixed()+
       theme(axis.text.x = element_text(angle =70, hjust = 1),
             panel.background = element_rect(fill="grey"))+
       geom_tile(datatemp,mapping=aes(x=a,y=coordRef,fill=pourcAire1, height=0.8,width=0.8))+
       geom_tile(datatemp,mapping=aes(x=coordRef,y=a,fill=pourcAire2, height=0.8,width=0.8))+
       ggtitle(titre4,subtitle="Percentage inferior to 0.5 are rounded to 0")

       # annotate("segment",x=0.6, xend=0.6, y=ypos-2, yend=ypos+2,color="lightskyblue1",
       #          arrow = arrow(type = "closed", length = unit(0.02, "npc")))


       if (saveG!=""){
         plotName<-paste0(saveG,".png")
         png(filename = plotName,width=1200,height=900)
         print(plot_grid(l1Plot,l2Plot,accordPlot,matConfPlot, align='hv'))
         dev.off()
       } else {
         print(plot_grid(l1Plot,l2Plot,accordPlot,matConfPlot, align='hv'))
       }
  }else{message("no plots created")}

return(matConfOut)

}

