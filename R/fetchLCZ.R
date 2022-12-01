#' Fetches the data from the Geomanum cloud, for locations already uploaded there by the Geoclimate team
#'
#' @param location is the name of the place for which you want to get the GeoClimate data
#' @param outDir is the path to the directory in which fetchLCZ will unpack the data
#' @param wf workflow can be bdtopo_2_2, when GeoClimate used the BDTOPO data base, or osm when it used the open street map data.
#' @param refYear this parameters is integrated in the name given to the data, in order to allow to re-run the function according to the reference year. At the moment only 2022 data have been tried
#' @param ... allows to pass arguments from the produce analysis function to the LCZgroup2 and compareLCZ functions
#' @import utils

#' @return returns no R object. The data are uploaded from the GeoClimate cloud and locally unpacked in the outDir directory
#' @export
#'
#' @examples
fetchLCZ<-function(location,outDir,wf="bdtopo_2_2",refYear="2022",...){
# wf : workflow, can be "osm" or "bdtopo_2_2"
      wd<-getwd()



      if(wf=="bdtopo_2_2"){
        folder<-paste0(paste0(outDir,"/",wf,"/",location))
        url<-paste0("https://cloud.geomanum.org/index.php/s/geoclimate/download?path=%2F",
                    wf,"&files=",location,".zip")
      } else if(wf=="osm"){
        folder<-paste0(outDir,"/",wf,"/",refYear,"/",location)
        url<-paste0("https://cloud.geomanum.org/index.php/s/geoclimate/download?path=%2F",
                    wf,"%2F",refYear,"&files=", location,".zip")
      }

    destFile<-paste0(folder,"/",location,".zip")

    cat("folder= ",folder)
    cat("url= ",url)
    #print(commande2)

    if(file.exists(folder)){
      print('The folder already exists')
    } else {
      dir.create(folder,recursive=T)
    }

    if(file.exists(destFile)){
      cat('The file already exists')
    } else {
      cat('the file needs to be downloaded')
      download.file(url=url,method="wget",destfile=destFile)
    }

unzip(destFile,exdir=folder)

  }



