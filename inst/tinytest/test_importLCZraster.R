#This tests teh function importLCZvect
# library(tinytest)
#
# library(sf)

# Test functionnal import

redonBbox<-importLCZvect(dirPath=paste0(
  system.file("extdata", package = "lczexplore"),"/multipleWfs/Redon"),file="osm_lcz.fgb",column="LCZ_PRIMARY",
  , output="bBox")

expect_warning(redonWudapt<-importLCZraster(

  system.file("extdata", package = "lczexplore"),
  fileName="redonWudapt.tif", bBox=redonBbox,
  LCZband=1, LCZcolumn="LCZ_PRIMARY"),
              'attribute variables are assumed to be spatially constant throughout all geometries' )

# library(terra)
# test<-rast(paste0(system.file("extdata", package = "lczexplore"),"/redonWudapt.tif"))

expect_silent(showLCZ(redonWudapt, column = "LCZ_PRIMARY", repr = "standard"))

# Test out of Europe Bbox (supposed to fail)
# library(sf)
#bBoxCoord<-c(-117.312698,32.805168,-117.227554,32.864593)
lowCorner<-sf::st_point(c(-117.312698,32.805168))
upCorner<-sf::st_point(c(-117.227554,32.864593))
outBbox<-sf::st_sfc(lowCorner,upCorner,crs=4326)
#importLCZraster("/home/gousseff/Documents/2_CodesSources/Wudapt/WudaptEurope/",bBox=outBbox)

# test bounding box not intersecting with rastet
# expect_error(current = test<-importLCZraster(dirPath = system.file("extdata", package = "lczexplore"),
#                                              fileName="redonWudapt.tif",bBox=outBbox),
#              "The bounding box doesn\'t intersect ")


# test chosing wich band of the raster is imported
# redonWudapt2<-importLCZraster(  
#   system.file("extdata", package = "lczexplore"),
#   fileName="redonWudapt.tif",bBox=redonBbox,
# LCZband=1, LCZcolumn="LCZ")
#  showLCZ(redonWudapt2, column = "LCZ")


# test default import

redonBbox<-importLCZvect(dirPath=paste0(system.file("extdata", package = "lczexplore"),
  "/multipleWfs/Redon"), file="bdt_lcz.fgb", column="LCZ_PRIMARY", output="bBox")

redonWudapt<-importLCZraster(system.file("extdata", package = "lczexplore"),
 fileName="redonWudapt.tif",bBox=redonBbox, LCZband=1, LCZcolumn='EU_LCZ_map')

expect_warning(redonWudapt<-importLCZraster(system.file("extdata", package = "lczexplore"),
                                            fileName="redonWudapt.tif",bBox=redonBbox, LCZband=1, LCZcolumn='EU_LCZ_map'),
               "attribute variables are assumed to be spatially constant"
)