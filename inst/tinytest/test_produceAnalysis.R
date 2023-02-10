#This tests the function produceAnalysis
# library(tinytest)
# library(lczexplore)
# library(sf)

system.file(package="lczexplore")

expect_warning(produceAnalysis(location="Redon",
                          outDir=paste0(
                            system.file(package="lczexplore"),"/tinytest"),
                          wf1="bdtopo_2_2",
                          wf2="osm",refYear1="2022",refYear2="2022",repr="brut",saveG=location),
               "attribute variables are assumed to be spatially constant")

expect_message(produceAnalysis(location="Redon",
                outDir=paste0(
                  system.file(package="lczexplore"),"/tinytest"),
                wf1="bdtopo_2_2",
                wf2="osm",refYear1="2022",refYear2="2022",repr="brut",saveG=location),
 "the unzipped rsu_lcz.geojson file already exists"
)

file.remove(paste0(
  system.file(package="lczexplore"),"/tinytest/bdtopo_2_2/Redon/rsu_lcz.geojson"))

expect_message(
  produceAnalysis(location="Redon",
                outDir=paste0(
                  system.file(package="lczexplore"),"/tinytest"),
                wf1="bdtopo_2_2",
                wf2="osm",refYear1="2022",refYear2="2022",repr="brut",saveG=location),
  "and will be unzipped now"
)

file.remove(paste0(
  system.file(package="lczexplore"),"/tinytest/bdtopo_2_2/Redon/rsu_lcz.geojson"))

file.remove(paste0(
  system.file(package="lczexplore"),"/tinytest/bdtopo_2_2/Redon/Redon.zip"))

expect_message(
  produceAnalysis(location="Redon",
                  outDir=paste0(
                    system.file(package="lczexplore"),"/tinytest"),
                  wf1="bdtopo_2_2",
                  wf2="osm",refYear1="2022",refYear2="2022",repr="brut",saveG=location),
  "it will be downloaded and unzipped."
)


file.remove(paste0(
  system.file(package="lczexplore"),"/tinytest/bdtopo_2_2/Redon/Redon.zip"))

produceAnalysis(location="Redon",
                outDir=paste0(
                  system.file(package="lczexplore"),"/tinytest"),
                wf1="bdtopo_2_2",
                wf2="osm",refYear1="2022",refYear2="2022",repr="brut",saveG=location)

