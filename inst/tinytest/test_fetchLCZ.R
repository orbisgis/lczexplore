# library(tinytest)

# NO TESTS AS THIS IS DEPRECATED


# system.file("extdata/", package = "lczexplore") %>% list.files()
# system.file("extdata/bdtopo_2_2", package = "lczexplore") %>% list.files()
# file.exists(system.file("extdata"multipleWfs/Redon"", package = "lczexplore"))
#
# system.file("extdata/osm/2022", package = "lczexplore") %>% list.files()
#
#
# fetchLCZ(location="Redon",
#          outDir=system.file("extdata", package = "lczexplore"),
#          wf="bdtopo_2_2")

# expect_message(fetchLCZ(location="Redon",
#                         outDir=system.file("extdata", package = "lczexplore"),
#                         wf="bdtopo_2_2"), "The folder already exists.")
# expect_message(fetchLCZ(location="Redon",
#                         outDir=system.file("extdata", package = "lczexplore"),
#                         wf="bdtopo_2_2"), "rsu_lcz.geojson already exists in this directory")
# expect_message(fetchLCZ(location="Redon",
#                         outDir=system.file("extdata", package = "lczexplore"),
#                         wf="bdtopo_2_2"),
#                "The folder already exists")
# expect_warning(fetchLCZ(location="Clohars-Carnoët",
#           outDir=system.file("extdata", package = "lczexplore"),
#           wf="bdtopo_2_2"),"The file couldn't be downloaded")