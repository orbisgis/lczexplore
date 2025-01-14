#' adds LCZ 105 geometries to the difference between a reference zone file and a file where roads are neither merged into
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
addRoadWaterLCZ<-function(sfList, zoneSf, waterWf = "bdt", missingGeomsWf="iau", location){
  zoneSf<-st_transform(zoneSf,
                       crs=st_crs(sfList$iau))
  missingDiff<-st_difference(
    st_union(zoneSf$geometry),
    st_union(sfList[[missingGeomsWf]]$geometry))

  if(length(missingDiff)==0){stop("st_difference returns no missing features, maybe the function was already applied to your dataset ?")}


  missingDiff<-st_as_sf(missingDiff)
  st_set_geometry(missingDiff, missingDiff$x)

  # set water according to the workflow chosen as reference for water
 
  sfList[[waterWf]]



  missingDiff$lcz_primary<-105
  missingDiff$location<-location
  missingDiff$wf<-missingGeomsWf

  names(missingDiff)[names(missingDiff)=="x"]<-"geometry"
  st_geometry(missingDiff)<-"geometry"
  missingDiff<-missingDiff[,c("lcz_primary", "location", "wf", "geometry")]
  missingDiff<-st_as_sf(missingDiff)
  sfList[[missingGeomsWf]]<-rbind(sfList[[missingGeomsWf]], missingDiff)
  return(sfList)
}