# This tests the function showLCZ
# library(tinytest)
#
# library(sf)
#showLCZ<-function(sf, title="", wf="",column="LCZ_PRIMARY", repr="standard", typeLevels="", cols="")

expect_silent(showLCZ(redonBDT, title="Zones climatiques locales à Redon",repr="standard"))
expect_silent(showLCZ(redonBDT))

testCol <- palette.colors(n=17, palette="Polychrome 36")
showLCZ(redonBDT, title="Zones climatiques locales à Redon",repr="grouped",
        cols = testCol )

redonBDTgrouped<-LCZgroup2(redonBDT,column="LCZ_PRIMARY",urban=c("1","2","3","4","5","6","7","8","9"),
                           industry="10",
                           vegetation=c("101","102","103","104"),
                           impervious="105",pervious="106",water="107",colors=c("red","black","green","grey","burlywood","blue"))

expect_message(showLCZ(redonBDTgrouped,column="grouped",repr="grouped",
        LCZlevels=c("urban","industry","vegetation","impervious","pervious","water"),
         cols=c("red","black","green","grey","burlywood","blue"),
        wf="BD TOPO"),"9:"
)

expect_message(
  showLCZ(redonBDTgrouped,column="grouped",repr="grouped",
                       LCZlevels=c("urban","industry","vegetation","impervious","pervious","water"),
        cols=c("red","black","green","grey","blue"),title="LCZ regroupées à Redon"),
  "For a better rendition specify as many colors as levels."
)

expect_message(
showLCZ(redonBDTgrouped,column="grouped",repr="grouped"),
  "No level vector and no color vector"
)

#levCol(redonBDTgrouped,column="grouped",levels=NULL,cols=NULL)

expect_message(
showLCZ(redonBDTgrouped,column="grouped",repr="grouped",
        LCZlevels=c("urban","industry","vegetation","impervious","pervious","water")),
  "7: No color vector but a level vector whose names cover the levels in the data"
)



