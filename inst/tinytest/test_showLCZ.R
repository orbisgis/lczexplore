# This tests the function showLCZ
# library(tinytest)
#
# library(sf)
#showLCZ<-function(sf, title="", wf="",column="LCZ_PRIMARY", repr="standard", typeLevels="", colors="")

expect_silent(
  showLCZ(redonOSM, title = "Zones climatiques locales à Redon",
          repr = "standard", drop = FALSE, colors = "", LCZlevels = "", naAsUnclassified = TRUE)
)
expect_silent(showLCZ(redonBDT, drop = TRUE))

testCol <- palette.colors(n = 17, palette = "Polychrome 36")

showLCZ(redonBDT, title = "Zones climatiques locales à Redon", repr = "standard", labelType = "short", noPerc = TRUE)

#  showLCZ(sf=redonOSM, wf="OSM", column="LCZ_PRIMARY", title="test", repr="alter", colors=testCol, useStandCol=FALSE)
# # 
# # 
#  showLCZ(redonBDT, title="Zones climatiques locales à Redon",repr="alter",
#          useStandCol=TRUE,
#          colors = testCol )
#levCol(sf=redonBDT, column="LCZ_PRIMARY",colors = testCol)

redonBDTgrouped <- groupLCZ(redonBDT, column = "LCZ_PRIMARY", urban = c("1", "2", "3", "4", "5", "6", "7", "8", "9"),
                            industry = "10",
                            vegetation = c("101", "102", "103", "104"),
                            impervious = "105", pervious = "106", water = "107",
                            colors = c("red", "black", "green", "grey", "burlywood", "blue"))

expect_message(
  showLCZ(redonBDTgrouped, column = "grouped", repr = "alter",
          wf = "BD TOPO", useStandCol = FALSE, addBorders = FALSE, noPercAlter = TRUE,
          levels = c("urban", "industry", "vegetation", "impervious", "pervious", "water"),
          colors = c("red", "black", "green", "grey", "burlywood", "blue")),
  "9:"
)

# levCol(redonBDTgrouped,column="grouped",
#        levels=c("urban","industry","vegetation","impervious","pervious","water"),
#        colors=c("red","black","green","grey","burlywood","blue"))

expect_message(
  showLCZ(redonBDTgrouped, column = "grouped", repr = "alter",
          LCZlevels = c("urban", "industry", "vegetation", "impervious", "pervious", "water"),
          colors = c("red", "black", "green", "grey"),
          title = "LCZ regroupées à Redon"), "case 11:"
)


expect_message(
  showLCZ(redonBDTgrouped, column = "grouped", repr = "alter", levels = c(
    "urban" = "red", "industry" = "black", "vegetation" = "green", "impervious" = "grey",
    "pervious" = "burlywood", "water" = "blue", "Unclassified" = "ghostwhite")),
  "4:"
)


expect_message(
  showLCZ(redonBDTgrouped, column = "grouped", repr = "alter", levels = c(
    "urban" = "red", "industry" = "black", "vegetation" = "green", "impervious" = "grey",
    "pervious" = "burlywood")),
  "6:"
)

expect_silent(
  showLCZ(redonBDTgrouped, column = "grouped", repr = "alter")
)

#levCol(redonBDTgrouped,column="grouped",levels=NULL,colors=NULL)

expect_message(
  showLCZ(redonBDTgrouped, column = "grouped", repr = "alter",
          LCZlevels = c("urban", "industry", "vegetation", "impervious", "pervious", "water", "Unclassified")),
  "7: No color vector but a level vector whose names cover the levels in the data"
)


expect_message(
  showLCZ(sf = redonBDTgrouped, column = "LCZ_PRIMARY", repr = "alter",
          urban = c("1", "2", "3", "4", "5", "6", "7", "8", "9"),
          industry = "10",
          vegetation = c("101", "102", "103", "104"),
          impervious = "105", pervious = "106", water = "107", Unclassified = "Unclassified",
          colors = c("red", "black", "green", "grey", "burlywood", "blue", "ghostwhite"), tryGroup = TRUE),
  "the function groupLCZ will try to create ")
