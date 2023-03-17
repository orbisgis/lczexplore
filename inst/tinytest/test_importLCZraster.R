#This tests teh function importLCZgen
# library(tinytest)
#
# library(sf)

# Test functionnal import

redonBbox<-importLCZgen(dirPath=paste0(
  system.file("extdata", package = "lczexplore"),"/bdtopo_2_2/Redon"),file="rsu_lcz.geojson",column="LCZ_PRIMARY",
  geomID="ID_RSU",confid="LCZ_UNIQUENESS_VALUE",output="bBox")


expect_warning(redonWudapt<-importLCZraster(
  system.file("extdata", package = "lczexplore"),fileName="redonWudapt.tif",bBox=redonBbox),
              'attribute variables are assumed to be spatially constant throughout all geometries' )

expect_silent(showLCZ(sf=redonWudapt,column="EU_LCZ_map",repr="standard"))

# Test out of Europe Bbox (supposed to fail)
library(sf)
#bBoxCoord<-c(-117.312698,32.805168,-117.227554,32.864593)
lowCorner<-st_point(c(-117.312698,32.805168))
upCorner<-st_point(c(-117.227554,32.864593))
outBbox<-st_sfc(lowCorner,upCorner,crs=4326)
#importLCZraster("/home/gousseff/Documents/2_CodesSources/Wudapt/WudaptEurope/",bBox=outBbox)


expect_error(current = test<-importLCZraster("/home/gousseff/Documents/2_CodesSources/Wudapt/WudaptEurope/",bBox=outBbox),
             "The bounding box doesn\'t intersect with the Wudapt tiff :"
               )
expect_error(current = test<-importLCZraster("/home/gousseff/Documents/2_CodesSources/Wudapt/WudaptEurope/",bBox=outBbox),
             "maybe it\'s out of the Europe zone.")

