#' Fetches LCZ classification data, by default from the Geomanum cloud, for locations already uploaded there by the Geoclimate team
#'
#' @param location is the name of the place for which you want to get the GeoClimate data
#' @param outDir is the path to the directory in which fetchLCZ will unpack the data
#' @param wf workflow can be bdtopo_2_2, when GeoClimate used the BDTOPO data base, or osm when it used the open street map data.
#' @param refYear this parameters is integrated in the name given to the data, in order to allow to re-run the function according to the reference year. At the moment only 2022 data have been tried
#' @param baseURL is the adress of the server where the function fetches the data. The data must be organised in a bdtopo_2_2 and an osm/year directories.
#' This function is intended to be used by Paendora project members, so the default is the Geomanum Foundation cloud.
#' @param ... allows to pass arguments from the produce analysis function to the groupLCZ and compareLCZ functions
#' @import utils methods


#' @return returns no R object. The data are uploaded from the GeoClimate cloud and locally unpacked in the outDir directory
#' @export
#'
#' @examples 
#' # not run as the access is not open
#' # fetchLCZ(location="Allaire",
#' # outDir=system.file("extdata", package = "lczexplore"),
#' # wf="bdtopo_2_2",refYear="2022")
fetchLCZ<-function(location,outDir,wf="bdtopo_2_2",refYear="2022",
                   baseURL="https://cloud.geomanum.org/index.php/s/geoclimate/download?path=%2F",...){

      #wd<-getwd()



      if(wf=="bdtopo_2_2"){
        folder<-paste0(paste0(outDir,"/",wf,"/",location))
        url<-paste0(baseURL,
                    wf,"&files=",location,".zip")
      } else if(wf=="osm"){
        folder<-paste0(outDir,"/",wf,"/",refYear,"/",location)
        url<-paste0(baseURL,
                    wf,"%2F",refYear,"&files=", location,".zip")
      }

    destFile<-paste0(folder,"/",location,".zip")
    destLCZfile<-paste0(folder,"/","rsu_lcz.geojson")
    cat("folder= ",folder,"\n")
    cat("url= ",url,"\n")

    if(file.exists(folder)){
      message('The folder already exists. \n')
    } else {
      message('The proper folder doesn\'t already exist and wille be created \n')
      dir.create(folder,recursive=T)
      folderToken<-1
    }

  if (file.exists(destLCZfile)){message(' An rsu_lcz.geojson already exists in this directory. \n If you think it is not the proper file
              try deleting it and re-running the fetchLCZ function.')} else
  { if(file.exists(destFile)) {
      message('The zip file already exists and will be unzipped now.')
        unzip(destFile,exdir=folder)}
     else {
      message('The zip file doesn\'t exist in the directory \n it will be downloaded and unzipped.')
        try_fetch<-try(download.file(url=url,method="auto",destfile=destFile))
        if(is(try_fetch,"try-error")){
         warning("The file couldn't be downloaded, maybe the location wasn't proceeded by the GeoClimate team ?")
          if (folderToken==1){unlink(folder,recursive=T)}
        } else{
        download.file(url=url,destfile=destFile)
        unzip(destFile,exdir=folder)}
      }
  }
}




