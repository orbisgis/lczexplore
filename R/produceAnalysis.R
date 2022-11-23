#' Calls functions of the package to produce an analysis of two set of LCZ produced by GeoClimate.
#' Typically one will compare the LCZ produced by GeoClimate using the OSM data as input
#' to the LCZ produced by GeoClimate using the french BD_TOPO as input
#'
#' @param location defines the study area. The study area must have been coputed
#' and loaded on the cloud of the team. If the fetch function returns an error,
#' please contact the GeoClimate Team to make the data available
#'
#' @param outDir is the directory in which the output will be saved,
#' namely the graphics and both LCZ for intersected geometries
#' @param wf1 is the name of the workflow GeoClimate used to produce the first dataset It can be "osm" or "bdtopo_2_2"
#' @param wf2 is the name of the workflow Geoclimate used to produce the second dataset It can be "osm" or "bdtopo_2_2"
#' If needed, future versions may allow other values
#' @param refYear1 allows to indicate on which year the analysis was produced.
#' It is important for "osm" workflow as the data are updated frequently.
#' @param refYear2 see refYear1.
#' @param repr : "brut" means that original values of LCZ are used, "grouped" means the user wishes to group some of the LCZ levels under a new label.
#' @param saveG : when an empty character string, "", the plots are not saved. Else, the saveG string is used to produce the name of the saved png file.
#' @param ... allow to pass arguments if representation is grouped.
#' The expected arguments are the name of each grouped label,
#' the levels of LCZ they contain, and last a vector of the colors to use to plot them.
#' @import magrittr
#' @return
#' @export
#'
#' @examples
produceAnalysis<-function(location="Redon",
                          outDir="/home/gousseff/Documents/3_data/GeoClimateSource",
                          wf1="bdtopo_2_2",
                          wf2="osm",refYear1="2022",refYear2="2022",repr="brut",saveG=location,...)
{
setwd(outDir)
  args<-list(...)
  indSep<-names(args)
  indCol<-grep(x=indSep,pattern="colors")
  args2<-args[indSep[-indCol]]
  str(args2)
  valeurs<-args[indSep[indCol]] %>% unlist %>% as.vector()
  str(valeurs)
 if(length(args2)!=length(valeurs)) stop("You must specify as many colors as there are groups")


# Download and import the first/reference dataset
if (wf1=="bdtopo_2_2"){
  fetchLCZ(location=location,outDir=outDir,wf=wf1)
  inDir<-paste0(outDir,"/",wf1,"/",location,"/")
  print("inDir");print(inDir)
  df1<-importLCZgc(dirPath=inDir)
  print("df1");print(df1)
  }

if (wf1=="osm"){
  fetchLCZ(location=location,outDir=outDir,wf=wf1,refYear=refYear1)
  inDir<-paste0(outDir,"/",wf1,"/",refYear1,"/",location,"/")
  df1<-importLCZgc(dirPath=inDir)
}

if (wf1=="wudapt"){
  fetchLCZ(location=location,outDir=outDir,wf="bd_topo_v2",
           refYear = refYear1)
  inDirCont<-paste0(outDir,"/",wf1,"/",location,"/")
  dfBDTcontour<-importLCZgc(dirPath=inDirCont,output="contour")
  inDir<-paste0(outDir,"/",wf1,"/",refYear1,"/",location)
  df1<-importLCZwudapt("/home/gousseff/Documents/3_data/WUDAPTSources/WudaptEurope/",
                       bBox=dfBDTcontour)
}

# Import and download the second/alternative dataset
  if (wf2=="bdtopo_2_2"){
    fetchLCZ(location=location,outDir=outDir,wf=wf2)
    inDir<-paste0(outDir,"/",wf2,"/",location,"/")
    df2<-importLCZgc(dirPath=inDir)
  }

  if (wf2=="osm"){
    fetchLCZ(location=location,outDir=outDir,
             wf=wf2,refYear = refYear2)
    inDir<-paste0(outDir,"/",wf2,"/",refYear2,"/",location,"/")
    df2<-importLCZgc(dirPath=inDir)
  }

  if (wf2=="wudapt"){
    fetchLCZ(location=location,outDir=outDir,wf="bd_topo_v2")
    inDirCont<-paste0(outDir,"/",wf2,"/",location,"/")
    dfBDTcontour<-importLCZgc(dirPath=inDir,output="contour")
    inDir<-paste0(outDir,"/",wf2,"/",refYear1,"/",location,"/")
    df2<-importLCZwudapt("/home/gousseff/Documents/3_data/WUDAPTSources/WudaptEurope/",
                         bBox=dfBDTcontour)
  }

if(repr=='brut'){
      #name of output Graph
          nameG<-paste0(location,"_",wf1,"_",wf2,"_",repr)

      # Compare LCZ
      if((wf1=="osm"& wf2=="bdtopo2_2")|(wf2=="osm" & wf1=="bdtopo_2_2")){

        compareLCZ(sf1=df1,
                   column1="LCZ_PRIMARY",
                   sf2=df2,
                   column2="LCZ_PRIMARY",
                   ref=1,saveG=nameG,repr=repr,wf1=wf1,wf2=wf2,location=location,...)
      }

      if(wf1=="wudapt"&(wf2=="osm"|wf2=="bdtopo_v2")){

        compareLCZ(sf1=df1,
                   column1="EU_LCZ_map",
                   sf2=df2,
                   column2='LCZ_PRIMARY',
                   saveG=nameG,repr=repr,wf1=wf1,wf2=wf2,location=location,...)
      }

      if(wf2=="wudapt"&(wf1=="osm"|wf1=="bdtopo_v2")){

          compareLCZ(sf1=df1,
                     column1='LCZ_PRIMARY',
                     sf2=df2,
                     column2="EU_LCZ_map",
                     saveG=nameG,repr=repr,wf1=wf1,wf2=wf2,location=location,...)
      }
      }
  if(repr=='grouped'){

    print("entrée dans la boucle grouped")

    nameG<-paste0(location,"_",wf1,"_",wf2,"_",repr)

    if((wf1=="osm"& wf2=="bdtopo_2_2")|(wf2=="osm" & wf1=="bdtopo_2_2")){

      df1<-LCZgroup2(df1,column="LCZ_PRIMARY",...)
      df2<-LCZgroup2(df2,column="LCZ_PRIMARY",...)
      compareLCZ(sf1=df1,
                 column1='grouped',
                 sf2=df2,
                 column2='grouped',
                 ref=1,saveG=nameG,repr=repr,wf1=wf1,wf2=wf2,location=location,...)
      }

    if(wf1=="wudapt"&(wf2=="osm"|wf2=="bdtopo_2_2")){
      df1<-LCZgroup2(df1,column="EU_LCZ_map",...)
      df2<-LCZgroup2(df2,column="LCZ_PRIMARY",...)
      compareLCZ(sf1=df1,
                 column1='grouped',
                 sf2=df2,
                 column2='grouped',
                 saveG=nameG,repr=repr,wf1=wf1,wf2=wf2,location=location,...)
    }
    if(wf2=="wudapt"&(wf1=="osm"|wf1=="bdtopo_2_2")){
      df1<-LCZgroup2(df1,column="LCZ_PRIMARY",...)
      df2<-LCZgroup2(df2,column="EU_LCZ_map",...)
      compareLCZ(sf1=df1,
                 column1='grouped',
                 sf2=df2,
                 column2='grouped',
                 saveG=nameG,repr=repr,wf1=wf1,wf2=wf2,location=location,...)
    }

  }
}

