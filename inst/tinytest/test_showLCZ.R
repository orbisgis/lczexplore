# This tests the function showLCZ
# library(tinytest)
#
# library(sf)
#showLCZ<-function(sf, title="", wf="",column="LCZ_PRIMARY", repr="standard", typeLevels="", cols="")

expect_silent(
  showLCZ(redonBDT, title="Zones climatiques locales à Redon",repr="standard")
)
expect_silent(showLCZ(redonBDT))

testCol <- palette.colors(n=17, palette="Polychrome 36")
showLCZ(redonBDT, title="Zones climatiques locales à Redon",repr="alter",
        useStandCol=FALSE,
        cols = testCol )
levCol(sf=redonBDT, column="LCZ_PRIMARY",cols = testCol)

redonBDTgrouped<-LCZgroup2(redonBDT,column="LCZ_PRIMARY", urban=c("1","2","3","4","5","6","7","8","9"),
                           industry="10",
                           vegetation=c("101","102","103","104"),
                           impervious="105",pervious="106",water="107",
                           cols=c("red","black","green","grey","burlywood","blue"))

expect_message(
  showLCZ(redonBDTgrouped,column="grouped",repr="alter",
          wf="BD TOPO",useStandCol=FALSE,
        levels=c("urban","industry","vegetation","impervious","pervious","water"),
         cols=c("red","black","green","grey","burlywood","blue")),
  "9:"
)


expect_message(
  showLCZ(redonBDTgrouped,column="grouped",repr="alter",
                       LCZlevels=c("urban","industry","vegetation","impervious","pervious","water"),
        cols=c("red","black","green","grey","blue"),
          title="LCZ regroupées à Redon"),
  "For a better rendition specify as many colors as levels."
)


expect_message(
showLCZ(redonBDTgrouped,column="grouped",repr="alter", levels=c(
  "urban" = "red", "industry" = "black","vegetation" = "green","impervious" = "grey",
  "pervious" = "burlywood","water"="blue")),
"4:"
)

expect_message(
  showLCZ(redonBDTgrouped,column="grouped",repr="alter", levels=c(
    "urban" = "red", "industry" = "black","vegetation" = "green","impervious" = "grey",
    "pervious" = "burlywood")),
  "6:"
)

expect_message(
showLCZ(redonBDTgrouped,column="grouped",repr="alter"),
  "No level vector and no color vector"
)

#levCol(redonBDTgrouped,column="grouped",levels=NULL,cols=NULL)

expect_message(
showLCZ(redonBDTgrouped,column="grouped",repr="alter",
        LCZlevels=c("urban","industry","vegetation","impervious","pervious","water")),
  "7: No color vector but a level vector whose names cover the levels in the data"
)