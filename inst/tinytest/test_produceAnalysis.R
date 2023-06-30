#This tests the function produceAnalysis
#library(tinytest)
#library(lczexplore)
#library(sf)

#system.file(package="lczexplore")

expect_warning(produceAnalysis(location="Redon",
                          outDir=paste0(
                            system.file(package="lczexplore"),"/tinytest"),
                          wf1="bdtopo_2_2",
                          wf2="osm",refYear1="2022",refYear2="2022",repr="standard",saveG=location),
               "attribute variables are assumed to be spatially constant")

expect_message(produceAnalysis(location="Redon",
                outDir=paste0(
                  system.file(package="lczexplore"),"/tinytest"),
                wf1="bdtopo_2_2",
                wf2="osm",refYear1="2022",refYear2="2022",repr="standard",saveG=location),
 " but an rsu_lcz.geojson already exists in this directory"
)

file.remove(paste0(
  system.file(package="lczexplore"),"/tinytest/bdtopo_2_2/Redon/rsu_lcz.geojson"))

# expect_message(
#   produceAnalysis(location="Redon",
#                 outDir=paste0(
#                   system.file(package="lczexplore"),"/tinytest"),
#                 wf1="bdtopo_2_2",
#                 wf2="osm",refYear1="2022",refYear2="2022",repr="standard",saveG=location),
#   "and will be unzipped now"
# )
# 
# file.remove(paste0(
#   system.file(package="lczexplore"),"/tinytest/bdtopo_2_2/Redon/rsu_lcz.geojson"))
# 
# file.remove(paste0(
#   system.file(package="lczexplore"),"/tinytest/bdtopo_2_2/Redon/Redon.zip"))

expect_message(
  produceAnalysis(location="Redon",
                  outDir=paste0(
                    system.file(package="lczexplore"),"/tinytest"),
                  wf1="bdtopo_2_2",
                  wf2="osm",refYear1="2022",refYear2="2022",repr="standard",saveG=location),
  "The zip file doesn't exist in the directory."
)


# file.remove(paste0(
#   system.file(package="lczexplore"),"/tinytest/bdtopo_2_2/Redon/Redon.zip"))

# produceAnalysis(location="Redon",
#                 outDir=paste0(
#                   system.file(package="lczexplore"),"/tinytest"),
#                 wf1="bdtopo_2_2",
#                 wf2="osm",refYear1="2022",refYear2="2022",repr="standard",saveG=location)
file.remove("Redon_bdtopo_2_2_osm_standard.png")
file.remove("bdtopo_2_2_osm.csv")
