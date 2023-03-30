#' Compares two LCZ classification on the same zone,
#' produces a map for each classification, a map of their agreement and a representation of a confusion matrix between them
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
#' @param repr "standard" means that original values of LCZ are used,
#'  "grouped" means the user has groupe some of the LCZ levels under a new label.
#'  In the latter case, the ... arguments must contain the groups and a color vector.
#' @param plot : when FALSE non of the graphics are plotted or saved
#' @param saveG : when an empty character string, "", the plots are not saved. Else, the saveG string is used to produce the name of the saved png file.
#' @param location : the name of the study area, as chosen as the name of the directory on the GeoClimate team cloud.
#' If the area you wish to analyse is not uploaded yet, please contact the GeoClimate Team.
#' @param exwrite : when TRUE, the values of the LCZ on the intersected geoms are written down in a csv file
#' @param outDir : when exwrite equals TRUE, outDir is the path to the folder where one wants to write
#' the csv file containing the values of the LCZ on the intersected geoms
#' @param tryGroup : when TRUE, if the specified level names don't match the data, but the specified levels do,
#' a call to the LCZgroup2 function will be tried, and if it works, the resulting grouping column will be named
#' "grouped" and the comparison will be done using it.
#' @param ... allow to pass arguments if representation is grouped.
#' The expected arguments are the name of each grouped label,
#' the levels of LCZ they contain, and last a vector of the colors to use to plot them.
#' @import sf ggplot2 dplyr cowplot forcats units tidyr RColorBrewer utils grDevices
#' @return returns an object called matConfOut which contains
#' matConfLong, a confusion matrix in a longer form, which can be written in a file by the compareLCZ function
#' and is used by the geom_tile function of the ggplot2 package.
#' matConfPlot is a ggplot2 object showing the confusion matrix. If plot=T, it is also directly plotted
#' areas contains the sums of each LCZ area
#' percAgg is the general agreement between the two sets of LCZ, expressed as a percentage of the total area of the study zone
#' If saveG is not an empty string, graphics are saved under "saveG.png"
#' @export
#' @examples
#' compareLCZ(sf1=redonBDT, column1="LCZ_PRIMARY", geomID1 = "ID_RSU",
#' confid1="LCZ_UNIQUENESS_VALUE", wf1="bdtopo_2_2",
#' sf2=redonOSM, column2="LCZ_PRIMARY", geomID2 = "ID_RSU",
#' confid2="LCZ_UNIQUENESS_VALUE", wf2="osm",
#' repr="standard", saveG="", exwrite=TRUE, location="Redon", plot=TRUE)
compareLCZ<-function(sf1,geomID1="",column1,confid1="",wf1="bdtopo_2_2",
                     sf2,column2,geomID2="",confid2="",wf2="osm",ref=1,
                     repr="standard",saveG="",exwrite=TRUE,outDir=getwd(),location="Redon", plot=TRUE, tryGroup=FALSE, ...){



  # store the column names in a way that can be injected in functions A SUPPRIMER ?

  column2Init<-column2 # in case of column1==column2, this will be used to call levCol
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

  if(repr=="standard"){

    uniqueData1<-sf1[column1] |> sf::st_drop_geometry()  |> unique() # Attention unique outputs a list of length 1
    uniqueData1<-levels(uniqueData1[,1]) |> as.character() |> as.vector()

    uniqueData2<-sf2[column2] |> sf::st_drop_geometry()  |> unique() # Attention unique outputs a list of length 1
    uniqueData2<-levels(uniqueData2[,1]) |> as.character() |> as.vector()

    LCZlevels<-as.character(c(1:10,101:107))
    if (prod(uniqueData1%in%LCZlevels)==0){
      line1<-"The column chosen for the first data set dosen't seem to be a standard LCZ encoding. \n"
      line2<-"Did you import the data with importLCZgen ? \n"
      line3<-" If the LCZ types are not standard, you can try to set repr to grouped and specify the levels. \n"
      errorMessage<-paste(line1,line2,line3)
      stop(errorMessage) }
    if (prod(uniqueData2%in%LCZlevels)==0){
      line1<-"The column chosen for the second data set dosen't seem to be a standard LCZ encoding. \n"
      line2<-"Did you import the data with importLCZgen ? \n"
      line3<-" If the LCZ types are not standard, you can try to set repr to grouped and specify the levels. \n"
      errorMessage<-paste(line1,line2,line3)
      stop(errorMessage) }

       typeLevels<-c("#8b0101","#cc0200","#fc0001","#be4c03","#ff6602","#ff9856",
                   "#fbed08","#bcbcba","#ffcca7","#57555a","#006700","#05aa05",
                   "#648423","#bbdb7a","#010101","#fdf6ae","#6d67fd")

      etiquettes<-c("LCZ 1: Compact high-rise","LCZ 2: Compact mid-rise","LCZ 3: Compact low-rise",
                    "LCZ 4: Open high-rise","LCZ 5: Open mid-rise","LCZ 6: Open low-rise",
                    "LCZ 7: Lightweight low-rise","LCZ 8: Large low-rise",
                    "LCZ 9: Sparsely built","LCZ 10: Heavy industry",
                    "LCZ A: Dense trees", "LCZ B: Scattered trees",
                    "LCZ C: Bush,scrub","LCZ D: Low plants",
                    "LCZ E: Bare rock or paved","LCZ F: Bare soil or sand","LCZ G: Water"
                    )


      #names(typeLevels)<-typeLevels
      # Classification must be encoded as factors
      sf1<-sf1 %>% mutate(!!column1:=factor(subset(sf1,select=column1,drop=T),levels=LCZlevels))
      sf2<-sf2 %>% mutate(!!column2:=factor(subset(sf2,select=column2,drop=T),levels=LCZlevels))
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
      LCZlevels<-c("Compact high",
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
  names(typeLevels)<-LCZlevels
  rm(temp1) ; rm(temp2)
  }



  if(repr=="grouped")
  { ############### This is a temporary feature. Grouping LCZ, showing and comparing grouped LCZ will be re-written in a  cleaner way in a later version
    args<-list(...)

    # Call levCol to deal with levels and colors
    levCol1<-levCol(sf1,column1,...)
    levCol2<-levCol(sf2,column2,...)
    levColCase1<-levCol1$case
    levColCase2<-levCol2$case
    temporaire3<-c(levCol1$levelsColors,levCol2$levelsColors)
    typeLevels<-temporaire3[unique(names(temporaire3))]
    LCZlevels<-names(typeLevels)

    # if there are several parameters to specify grouping levels
    # and their names don't cover the values in column, and if tryGroup is T
    # then we try to call LCZgroup2 And procede to grouping accordingly

    if (tryGroup==TRUE && (length(grep("14: ",levColCase1))!=0 ||length(grep("15: ",levColCase1))!=0 )){
      message("Level names in your 1st dataset didn't match original data.
      As tryGroup=TRUE, the function LCZgroup2 will try to create a \"grouped\" column with level names and levels specified in (...).
      If this doesn't work, compareLCZ function may fail.")
      sfNew1<-LCZgroup2(sf1,column = column1,...)
      #sf1[column1]<-sfNew1["grouped"]
      sf1<-sfNew1 %>% mutate(!!column1:=subset(sfNew1,select="grouped",drop=TRUE))
      # print(summary(sf1))
      levCol1<-levCol(sf1,column1,...)

      rm(sfNew1)
    }

    if (tryGroup==TRUE && (length(grep("14: ",levColCase2))!=0 ||length(grep("15: ",levColCase2))!=0 )){
      message("As tryGroup=TRUE, the function LCZgroup2 will try to create a \"grouped\" column with level names and levels specified in (...).
      If this doesn't work, compareLCZ function may fail.")
      sfNew2<-LCZgroup2(sf2,column = column2,...)
      #sf2[column2]<-sfNew2["grouped"]
      sf2<-sfNew2 %>% mutate(!!column2:=subset(sfNew2,select="grouped",drop=TRUE))
      # print(summary(sf2))
      levCol2<-levCol(sf2,column2,...)
      rm(sfNew2)
    }

    # print(summary(sf1))
    # print(summary(sf2))
    temporaire3<-c(levCol1$levelsColors,levCol2$levelsColors)
    typeLevels<-temporaire3[unique(names(temporaire3))]
    LCZlevels<-names(typeLevels)
    etiquettes<-LCZlevels

    nom1<-c(geomID1,column1,confid1)
    nom1<-nom1[sapply(nom1,nchar)!=0]
    nom2<-c(geomID2,column2,confid2)
    nom2<-nom2[sapply(nom2,nchar)!=0]


    sf1<-select(sf1,nom1) %>% drop_na(column1)
    sf2<-select(sf2,nom2) %>% drop_na(column2)

    # this illustrates how silly it was to chose to store levels and colors in the same vector as names and values.
    # Classification must be encoded as factors

     sf1<-sf1 %>% mutate(!!column1:=factor(subset(sf1,select=column1,drop=T),levels=LCZlevels))
     sf2<-sf2 %>% mutate(!!column2:=factor(subset(sf2,select=column2,drop=T),levels=LCZlevels))
  }




  ######################################################
   # # Intersect geometries of both files
  ######################################################
    #intersection of geometries
  echInt<-st_intersection(x=sf1[,nom1],y=sf2[,nom2])
    # checks if the two LCZ classifications agree
  echInt$agree<-subset(echInt,select=column1,drop=T)==subset(echInt,select=column2,drop=T)


######################################################
###
### Confusion matrix, weights being the area of the intersecting geoms
###
######################################################

  # Export of lcz and area for each geom for further analysis

        echInt<-echInt %>% mutate(area=st_area(geometry)) %>% drop_units
        echIntExpo<-echInt %>% mutate(location=location,area=as.numeric(area)) %>%
          st_set_geometry(NULL) %>% as.data.frame()


        nom<-paste0(wf1,"_",wf2,".csv")
        print(paste0("Comparison data will be appended to the following file : ",nom))
        filePath<-paste0(outDir,"/",nom)

        if (exwrite==TRUE){
          print(paste0("The data will be exported in the ",
                       nom,
                       " file, in your working directory:",
                       getwd())
                )
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

matConfOut<-matConfLCZ(sf1=sf1, column1=column1, sf2=sf2, column2=column2,
                       repr=repr, typeLevels=LCZlevels, plot=FALSE)
matConfOut$data<-echIntExpo
matConfLong<-matConfOut$matConf
areas<-matConfOut$areas
percAgg<-matConfOut$percAgg

################################################
#  GRAPHICS
################################################
if (plot == TRUE){
  if (repr=='standard'){titrou<-"LCZ"} else {titrou<-"Grouped LCZs"}

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


  # ypos<-if (repr=="standard"){ypos=5} else {ypos=2}
  etiquettes1<-paste(etiquettes, areas$area1 ," %")
  names(etiquettes1)<-LCZlevels
  etiquettes2<-etiquettes
  etiquettes2<-gsub(":.*",": ",etiquettes)
  etiquettes2<-paste(etiquettes2, areas$area2 ," %")

  etiquettes1.2<-paste(etiquettes2,areas$area1)
  datatemp<-data.frame(a=factor(LCZlevels),percArea=areas$area1,percArea1=areas$area1,percArea2=areas$area2)

  # center all plots
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
     geom_sf(data=sf1,aes(fill=get(column1)), colour=NA)+
     scale_fill_manual(values=typeLevels,labels=etiquettes1, drop=FALSE)+
     guides(fill=guide_legend(title=titrou))+
     ggtitle(titre1, subtitle=paste0("Number of RSU : ",nbgeom1))
     #


  # Plot the second classification
     l2Plot<-ggplot(boundary)+
      geom_sf(data=boundary, fill=NA,lty='blank')+
      geom_sf(data=sf2,aes(fill=get(column2)), colour=NA)+
      scale_fill_manual(values=typeLevels,labels=etiquettes2,drop=FALSE)+
      guides(fill=guide_legend(title=titrou))+
      ggtitle(titre2,subtitle=paste0("Number of RSU : ", nbgeom2))

  # Plot areas where classifications agree
     agreePlot<-ggplot(boundary)+
       geom_sf(data=boundary, fill=NA,lty='blank')+
       geom_sf(data=echInt,aes(fill=agree),lwd=0,colour=NA)+
       scale_fill_manual(values=c("red","green"),
                         name=paste0(
                           "The two classifications agree for \n ",percAgg, " % of the area Agreement"))+
       ggtitle(label=titre3, subtitle=paste0("Number of intersected geoms : ", nbgeomInter))

  # Plot how the LCZ each level of the first classification is split into levels of the second classification
     coordRef<-length(typeLevels)+1

     matConfPlot<-ggplot(data = matConfLong, aes(x=get(column1), y=get(column2), fill =agree)) +
       geom_tile(color = "white",lwd=1.2,linetype=1)+
       labs(x=titre1,y=titre2)+
       scale_fill_gradient2(low = "lightgrey", mid="cyan", high = "blue",
                         midpoint = 50, limit = c(0,100), space = "Lab",
                         name="% area") +
       geom_text(data=matConfLong[matConfLong$agree!=0,],aes(label=round(agree,digits=0)),
                 color="black") +coord_fixed()+
       theme(axis.text.x = element_text(angle =70, hjust = 1),
             panel.background = element_rect(fill="grey"))+
       geom_tile(datatemp,mapping=aes(x=a,y=coordRef,fill=percArea1, height=0.8,width=0.8))+
       geom_tile(datatemp,mapping=aes(x=coordRef,y=a,fill=percArea2, height=0.8,width=0.8))+
       ggtitle(titre4,subtitle="Percentage inferior to 0.5 are rounded to 0")



       if (saveG!=""){
         plotName<-paste0(saveG,".png")
         png(filename = plotName,width=1200,height=900)
         print(plot_grid(l1Plot,l2Plot,agreePlot,matConfPlot, align='hv'))
         dev.off()
       } else {
         print(plot_grid(l1Plot,l2Plot,agreePlot,matConfPlot, align='hv'))
       }
}else{message("Plot set to FALSE, no plots created")}

matConfOut<-matConfOut
}

