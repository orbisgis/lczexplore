#' adds missing Reference Spatial Units (geometries) from the difference with a reference zone file, 
#' and possibly add a value to the lcz column according to a reference workflow
#' reference spatial units nor represented by their own geometries
#' adding a column for location and workflow names 
#' @param sfList the list of LCZ sf objects 
#' @param workflowNames sets the names of workflows and define the name of the files which will be loaded and intersected
#' @param location the name of the location at which all LCZ are created
#' @importFrom ggplot2 geom_sf guides ggtitle aes
#' @import sf dplyr cowplot forcats units tidyr RColorBrewer utils grDevices rlang
#' @return returns graphics of comparison and an object called matConfOut which contains :
#' matConfLong, a confusion matrix in a longer form, 
#' matConfPlot is a ggplot2 object showing the confusion matrix.
#' percAgg is the general agreement between the two sets of LCZ, expressed as a percentage of the total area of the study zone
#' pseudoK is a heuristic estimate of a Cohen's kappa coefficient of agreement between classifications
#' If saveG is not an empty string, graphics are saved under "saveG.png"
#' @export
#' @examples
addMissingRSUs<-function(sfList, missingGeomsWf="iau", zoneSf, refWf = "bdt", refLCZ = "107", residualLCZvalue="105",
                           location, column = "lcz_primary"){
  refCRS<-st_crs(sfList[[missingGeomsWf]])
  zoneSf<-st_transform(zoneSf,
                       crs=refCRS)
  missingDiff<-st_difference(
    st_union(zoneSf),
    st_union(sfList[[missingGeomsWf]])) %>% st_as_sf
    st_set_geometry(missingDiff, missingDiff$x)
  names(missingDiff)[names(missingDiff)=="x"]<-"geometry"
  st_geometry(missingDiff)<-"geometry"
  


  if(length(missingDiff)==0){stop("st_difference returns no missing features, maybe the function was already applied to your dataset ?")}
  
  if (is.null(refWf)){
    missingDiff[[column]]<-residualLCZvalue
    missingDiff[["location"]]<-unique(sfList[[missingGeomsWf]][["location"]])
    missingDiff[["wf"]]<-missingGeomsWf
    missingDiff<-missingDiff[,c(column, "location", "wf", "geometry")]
    sfList[[missingGeomsWf]]<-rbind(sfList[[missingGeomsWf]], missingDiff)
    }
  else {
    refSf<-sfList[[refWf]]
    LCZref<-refSf[ refSf[[column]] == refLCZ , ] %>% st_transform(crs=refCRS)
  
    LCZnew<-st_intersection(missingDiff, LCZref)%>% st_as_sf
    st_set_geometry(LCZnew, LCZnew$x)
    names(LCZnew)[names(LCZnew)=="x"]<-"geometry"
    st_geometry(LCZnew)<-"geometry"
  
    residualDiff<-st_difference(
      st_union(missingDiff),
      st_union(LCZnew)) %>% st_as_sf
    st_set_geometry(residualDiff, residualDiff$x)
    names(residualDiff)[names(residualDiff)=="x"]<-"geometry"
    st_geometry(residualDiff)<-"geometry"
    
    residualDiff[[column]]<-residualLCZvalue
    residualDiff[["location"]]<-unique(LCZref[["location"]])
    residualDiff[["wf"]]<-missingGeomsWf
    
    residualDiff<-residualDiff[,c(column, "location", "wf", "geometry")]
  
    sfList[[missingGeomsWf]]<-rbind(sfList[[missingGeomsWf]], LCZnew)
    sfList[[missingGeomsWf]]<-rbind(sfList[[missingGeomsWf]],residualDiff)
  }
 
 return(sfList)

}