compareLCZ<-function(sf1,column1,wf1="bdtopo_2_2",sf2,column2,wf2="osm",
                     repr="brut",saveG="",ref=1,location="Redon",...){

  # dependancies

  paquets<-c("sf","ggplot2","dplyr","cowplot","forcats","units","tidyr","RColorBrewer")
  lapply(paquets, require, character.only = TRUE)

  if (!(wf1=="osm"||wf1=="bdtopo_2_2"||wf1=="wudapt")|| !(wf2=="osm"||wf2=="bdtopo_2_2"||wf2=="wudapt"))
    stop("Workflow parameters wf1 and wf2 muste be one of bdtopo_2_2, osm or wudapt")


  # store the column names in a way that can be injected in functions A SUPPRIMER ?
  namesf1<-deparse(substitute(sf1))
  namesf2<-deparse(substitute(sf2))


  # handling of different crs for the two datasets
  if(st_crs(sf1)!=st_crs(sf2)){sf2<-sf2 %>% st_transform(crs=st_crs(sf1))
    # if (ref!=""){crsOpt=ref} else {
    #     crsOpt<-readline("Both sf datasets need to live in the same crs projection (srid/epsg) :
    #           type 1 to keep the crs of the first sf dataset, type 2 to keep the crs of the second,
    #           if unsure press 3 to interrupt")
    # if(crsOpt==1){sf2<-sf2 %>% st_transform(crs=st_crs(sf1))}
    #     else if(crsOpt==2){sf1<-sf1 %>% st_transform(crs=st_crs(sf2))}
    #          else {print("Instructions about which crs to use are unclear")}
    # }
  }


  # In order not to "carry" the whole file, keep only the column of interest (LCZ)





  # If LCZ name is the same in both input sf files we rename column2
  if(column1==column2){
    column2<-paste0(column1,".1")
    sf2<-sf2 %>% mutate(!!column2:=subset(sf2,select=column1,drop=T))
    sf2<-sf2 %>% mutate(!!column1:=NULL)
  }

 print(paste(" The column ",column1, " of the dataset", namesf1,
             ", is the reference against which the ",column2,
             " column of the dataset ", namesf2, "will be compared."))


  # Check the input file class

  try(class(sf1)[1]=="sf", stop("Input data must be sf objects"))
  try(class(sf1)[1]=="sf", stop("Input data must be sf objects"))
  try(st_crs(sf1)==st_crs(sf2),stop("Input sf objects must have the same SRID"))


 sf1<-select(sf1,column1) %>% drop_na(column1)
 sf2<-select(sf2,column2) %>% drop_na(column2)
 # Prepare the levels of the expected LCZ

  if(repr=="brut"){
      CodeCoulLCZ<-read.csv("/home/gousseff/Documents/2_CodesSources/GeoClimate/GeoclimateDefaultCase/Manips_R/CodesCouleursLCZ.csv")  # A remplacer par un fichier embarqué dans le paquet
      colorMap<-CodeCoulLCZ$Hexa.Color.code
      valeurs=colorMap
      etiquettes=CodeCoulLCZ$Type.definition
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
    # print(sf1[column1])
  # print(sf2[column2])
  }



  if(repr=="grouped"){
    # niveaux<-c("urban"="urban","industry"="industry","vegetation"="vegetation",
    #               "impervious"="impervious","perious"="pervious","water"="water")
    #
    #
    # valeurs<-c("urban"="red","industry"="Grey","vegetation"="green",
    #               "impervious"="black","pervious"="burlywood",
    #               "water"="blue")


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
          print("niveaux");print(niveaux);print("valeurs");print(valeurs)
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
  echInt<-st_intersection(x=sf1[column1],y=sf2[column2])
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
        echInt
        echIntExpo<-echInt %>% mutate(location=location,aire=as.numeric(aire)) %>%
          st_set_geometry(NULL) %>% as.data.frame()

        # nom<-paste0(deparse(substitute(location)),deparse(substitute(sf2)),"_",deparse(substitute(wf1)),deparse(substitute(sf2)),"_",deparse(substitute(wf2)),".geojson")

        nom<-paste0(wf1,"_",wf2,".csv")

        print(nom)

        write.table(x=echIntExpo, file =nom, append = TRUE, quote = TRUE, sep = ";",
                    eol = "\n", na = "NA", dec = ".", row.names = TRUE,
                    col.names = FALSE, qmethod = c("escape", "double"),
                    fileEncoding = "")

###################################################
# Confusion Matrix
###################################################

matConfOut<-matConfLCZ(sf1=sf1,column1=column1,sf2=sf2,column2=column2,repr=repr,niveaux=niveaux)

matConfLong=matConfOut$matConf
aires=matConfOut$aires
pourcAcc=matConfOut$pourcAcc

################################################
#  GRAPHICS
################################################

if (repr=='brut'){titrou<-"LCZ"} else {titrou<-"Grouped LCZs"}

if (wf1=="bdtopo_2_2"){adtitre1<-" BDTOPO V2.2"} else
  if(wf1=="osm"){adtitre1<-" OSM "} else
    if(wf1=="wudapt"){adtitre1<-" WUDAPT"}

if (wf2=="bdtopo_2_2"){adtitre2<-" BDTOPO V2.2"} else
  if(wf2=="osm"){adtitre2<-" OSM "} else
    if(wf2=="wudapt"){adtitre2<-" WUDAPT"}


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
datatemp<-data.frame(a=factor(niveaux),pourcAire=aires$aire1)

boundary1<-sf1 %>% st_union %>% st_boundary()
centro<-st_centroid(boundary1)
boundary1<-boundary1 %>% st_cast("POINT")
dist1<-st_distance(boundary1,centro) %>% max
boundary<-st_buffer(x=centro,dist=dist1) %>% st_make_grid(n=1)

nbgeom1<-nrow(sf1)
nbgeom2<-nrow(sf2)


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
     scale_fill_manual(values=c("red","green"),name="Agreement")+
     ggtitle(label=titre3, subtitle=paste0("The two classifications agree for ",
                                           pourcAcc, " % of the area "))

# Plot how the LCZ each level of the first classification is split into levels of the second classification
   matConfPlot<-ggplot(data = matConfLong, aes(x=get(column1), y=get(column2), fill =accord)) +
     geom_tile(color = "white",lwd=1.2,linetype=1)+
     labs(x=lab1,y=lab2)+
     scale_fill_gradient2(low = "lightgrey", mid="cyan", high = "blue",
                       midpoint = 50, limit = c(0,100), space = "Lab",
                       name="% area") +
     geom_text(data=matConfLong[matConfLong$accord!=0,],aes(label=round(accord,digits=2)),
               color="black") +coord_fixed()+
     theme(axis.text.x = element_text(angle =70, hjust = 1),
           panel.background = element_rect(fill="grey"))+
      geom_tile(datatemp,mapping=aes(x=a,y=18,fill=pourcAire,height=0.8))+
     geom_tile(datatemp,mapping=aes(x=18,y=a,fill=pourcAire,height=0.8))+
     ggtitle(titre4)

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



}

