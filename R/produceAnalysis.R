#' Calls functions of the package to produce an analysis of two set of LCZ
#' produced exclusively by GeoClimate
#' @details Typically one will compare the LCZ produced by GeoClimate using the OpenStreetMap
#' data as input to 
#' the LCZ produced by GeoClimate using the french BDTopo v2.2 as input.
#' Same analysis are reproducible using all the generic functions of the package
#'
#' @param location defines the study area. The study area must have been coputed
#' and loaded on the cloud of the team. If the fetch function returns an error,
#' please contact the GeoClimate Team to make the data available
#' @param outDir is the directory in which the output will be saved,
#' namely the graphics and both LCZ for intersected geometries
#' @param wf1 is the name of the workflow GeoClimate used to produce the first dataset It can be "osm" or "bdtopo_2_2"
#' @param wf2 is the name of the workflow Geoclimate used to produce the second dataset It can be "osm" or "bdtopo_2_2"
#' If needed, future versions may allow other values
#' @param refYear1 allows to indicate on which year the analysis was produced.
#' It is important for "osm" workflow as the data are updated frequently.
#' @param refYear2 see refYear1.
#' @param repr : "standard" means that original values of LCZ are used, "alter" means the user wishes to group some of the LCZ levels under a new label.
#' @param saveG : when an empty character string, "", the plots are not saved. Else, the saveG string is used to produce the name of the saved png file.
#' @param ... allow to pass arguments if representation is alter.
#' The expected arguments are the name of each grouped label,
#' the levels of LCZ they contain, and last a vector of the colors to use to plot them.
#' @importFrom magrittr "%>%"
#' @return : the function doesn't return an object, as all the results
#' are exported either in csv (both LCZ by geoms by town) either in png
#' (maps of LCZ classifications and agreement, and confusion matrix).
#' @export
#'
#' @examples
#' #produceAnalysis(location="Redon", outDir=paste0(system.file(package="lczexplore"),"/tinytest"),
#' #wf1="bdtopo_2_2", wf2="osm", refYear1="2022", refYear2="2022", repr="standard", saveG="")
#' #wf1="bdtopo_2_2", wf2="osm", refYear1="2022", refYear2="2022", repr="standard", saveG="")
produceAnalysis<-function(location="Redon",
                          outDir=getwd(),
                          wf1="bdtopo_2_2",
                          wf2="osm",refYear1="2022",refYear2="2022",repr="standard",saveG=location,...)
{
 
  args<-list(...)
  indSep<-names(args)
  indCol<-grep(x=indSep,pattern="colors")
  args2<-args[indSep[-indCol]]
  str(args2)
  typeLevels<-args[indSep[indCol]] %>% unlist %>% as.vector()
  str(typeLevels)
 if(length(args2)!=length(typeLevels)) stop("You must specify as many colors as there are groups")

  output<-list()

# Download and import the first/reference dataset
if (wf1=="bdtopo_2_2"){
  fetchLCZ(location=location,outDir=outDir,wf=wf1)
  inDir<-paste0(outDir,"/",wf1,"/",location,"/")
  print("inDir");print(inDir)
  df1<-importLCZvect(dirPath=inDir,file="rsu_lcz.geojson",column="LCZ_PRIMARY",
                    geomID="ID_RSU", confid="LCZ_UNIQUENESS_VALUE",output="sfFile")
  print("df1");print(df1)
  }

if (wf1=="osm"){
  fetchLCZ(location=location,outDir=outDir,wf=wf1,refYear=refYear1)
  inDir<-paste0(outDir,"/",wf1,"/",refYear1,"/",location,"/")
  df1<-importLCZvect(dirPath=inDir,file="rsu_lcz.geojson",column="LCZ_PRIMARY",
                    geomID="ID_RSU", confid="LCZ_UNIQUENESS_VALUE",output="sfFile")
}

if (wf1=="wudapt"){
  fetchLCZ(location=location,outDir=outDir,wf="bd_topo_v2",
           refYear = refYear1)
  inDirCont<-paste0(outDir,"/",wf1,"/",location,"/")
  dfBDTcontour<-importLCZgc(dirPath=inDirCont,output="contour")
  inDir<-paste0(outDir,"/",wf1,"/",refYear1,"/",location)
  df1<-importLCZraster(getwd(),
                       bBox=dfBDTcontour)
}

# Import and download the second/alternative dataset
  if (wf2=="bdtopo_2_2"){
    fetchLCZ(location=location,outDir=outDir,wf=wf2)
    inDir<-paste0(outDir,"/",wf2,"/",location,"/")
    df2<-importLCZvect(dirPath=inDir,file="rsu_lcz.geojson",column="LCZ_PRIMARY",
                      geomID="ID_RSU", confid="LCZ_UNIQUENESS_VALUE",output="sfFile")
  }

  if (wf2=="osm"){
    fetchLCZ(location=location,outDir=outDir,
             wf=wf2,refYear = refYear2)
    inDir<-paste0(outDir,"/",wf2,"/",refYear2,"/",location,"/")
    df2<-importLCZvect(dirPath=inDir,file="rsu_lcz.geojson",column="LCZ_PRIMARY",
                      geomID="ID_RSU", confid="LCZ_UNIQUENESS_VALUE",output="sfFile")
  }

  if (wf2=="wudapt"){
    fetchLCZ(location=location,outDir=outDir,wf="bd_topo_v2")
    inDirCont<-paste0(outDir,"/",wf2,"/",location,"/")
    dfBDTcontour<-importLCZgc(dirPath=inDir,output="contour")
    inDir<-paste0(outDir,"/",wf2,"/",refYear1,"/",location,"/")
    df2<-importLCZraster(getwd(),
                         bBox=dfBDTcontour)
  }

if(repr=="standard"){
  print("standard loop entered")
      #name of output Graph
          nameG<-paste0(location,"_",wf1,"_",wf2,"_",repr)

      # Compare LCZ
      
      if((wf1=="osm" | wf1=="bdtopo_2_2") & (wf2=="bdtopo_2_2" | wf2=="osm")){
        print("compareLCZ called")
        
          output$compare<-compareLCZ(sf1=df1, geomID1="ID_RSU", confid1="LCZ_UNIQUENESS_VALUE",
                     column1="LCZ_PRIMARY", wf1=wf1,
                     sf2=df2,
                     column2="LCZ_PRIMARY", geomID2="ID_RSU", confid2="LCZ_UNIQUENESS_VALUE",wf2=wf2,
                     ref=1,
                     repr="standard", saveG=nameG, exwrite=TRUE,outDir=outDir,location=location)
               

      }

      if(wf1=="wudapt"&& (wf2=="osm"|wf2=="bdtopo_v2")){

        output$compare<-compareLCZ(sf1=df1,
                   column1="EU_LCZ_map",
                   sf2=df2,
                   column2='LCZ_PRIMARY',
                   saveG=nameG,repr=repr,wf1=wf1,wf2=wf2,exwrite=TRUE,outDir=outDir,location=location,...)
      }

      if(wf2=="wudapt"&& (wf1=="osm"|wf1=="bdtopo_v2")){

        output$compare<-compareLCZ(sf1=df1,
                     column1='LCZ_PRIMARY',
                     sf2=df2,
                     column2="EU_LCZ_map",
                     saveG=nameG,repr=repr,wf1=wf1,wf2=wf2,exwrite=TRUE,outDir=outDir,location=location,...)
      }
}
  if(repr=='alter'){

    print("alter loop")

    nameG<-paste0(location,"_",wf1,"_",wf2,"_",repr)

    if((wf1=="osm"& wf2=="bdtopo_2_2")|(wf2=="osm" & wf1=="bdtopo_2_2")){

      df1<-groupLCZ(df1,column="LCZ_PRIMARY",...)
      df2<-groupLCZ(df2,column="LCZ_PRIMARY",...)
      output$compare<-compareLCZ(sf1=df1,
                 column1='grouped',
                 sf2=df2,
                 column2='grouped',
                 ref=1,saveG=nameG,repr=repr,wf1=wf1,wf2=wf2,exwrite=TRUE,outDir=outDir,location=location,...)
      }

    if(wf1=="wudapt"&(wf2=="osm"|wf2=="bdtopo_2_2")){
      df1<-groupLCZ(df1,column="EU_LCZ_map",...)
      df2<-groupLCZ(df2,column="LCZ_PRIMARY",...)
      output$compare<-compareLCZ(sf1=df1,
                 column1='grouped',
                 sf2=df2,
                 column2='grouped',
                 saveG=nameG,repr=repr,wf1=wf1,wf2=wf2,location=location,exwrite=TRUE,outDir=outDir,...)
    }
    if(wf2=="wudapt"&(wf1=="osm"|wf1=="bdtopo_2_2")){
      df1<-groupLCZ(df1,column="LCZ_PRIMARY",...)
      df2<-groupLCZ(df2,column="EU_LCZ_map",...)
      output$compare<-compareLCZ(sf1=df1,
                 column1='grouped',
                 sf2=df2,
                 column2='grouped',
                 saveG=nameG,repr=repr,wf1=wf1,wf2=wf2,location=location,exwrite=TRUE,outDir=outDir,...)
    }

  }
 #  setwd(wd)
  output$df1<-df1
  output$df2<-df2
  return(output)
}
