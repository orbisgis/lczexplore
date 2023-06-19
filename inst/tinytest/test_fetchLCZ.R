# library(tinytest)
# system.file("extdata/", package = "lczexplore") %>% list.files()
# system.file("extdata/bdtopo_2_2", package = "lczexplore") %>% list.files()
# file.exists(system.file("extdata/bdtopo_2_2/Redon", package = "lczexplore"))
#
# system.file("extdata/osm/2022", package = "lczexplore") %>% list.files()
#
#
# fetchLCZ(location="Redon",
#          outDir=system.file("extdata", package = "lczexplore"),
#          wf="bdtopo_2_2")

expect_message(fetchLCZ(location="Redon",
                        outDir=system.file("extdata", package = "lczexplore"),
                        wf="bdtopo_2_2"), "The folder already exists.")
expect_message(fetchLCZ(location="Redon",
                        outDir=system.file("extdata", package = "lczexplore"),
                        wf="bdtopo_2_2"), "The zip file doesn't exist in the directory")
expect_message(fetchLCZ(location="Redon",
                        outDir=system.file("extdata", package = "lczexplore"),
                        wf="bdtopo_2_2"),
               "but an rsu_lcz.geojson already exists in this directory.")
expect_warning(fetchLCZ(location="Clohars-Carnoët",
          outDir=system.file("extdata", package = "lczexplore"),
          wf="bdtopo_2_2"),"The file couldn't be downloaded")