# This tests the function showLCZ
# library(tinytest)
#
# library(sf)
#showLCZ<-function(sf, title="", wf="",column="LCZ_PRIMARY", repr="standard", typeLevels="", cols="")

expect_silent(showLCZ(redonBDT, title="Zones climatiques locales à Redon"))
expect_silent(showLCZ(redonBDT))

redonBDTgrouped<-LCZgroup2(redonBDT,column="LCZ_PRIMARY",urban=c("1","2","3","4","5","6","7","8","9"),
                           industry="10",
                           vegetation=c("101","102","103","104"),
                           impervious="105",pervious="106",water="107",colors=c("red","black","green","grey","burlywood","blue"))

expect_silent(
showLCZ(redonBDTgrouped,column="grouped",repr="grouped",typeLevels=c("urban","industry","vegetation","impervious","pervious","water"),
                                                   cols=c("red","black","green","grey","burlywood","blue"),
        wf="BD TOPO"))

expect_warning(showLCZ(redonBDTgrouped,column="grouped",repr="grouped",typeLevels=c("urban","industry","vegetation","impervious","pervious","water"),
        cols=c("red","black","green","grey","blue"),title="LCZ regroupées à Redon"),
  "For a better rendition, specify as many colors as levels of LCZ"
)

expect_warning(
showLCZ(redonBDTgrouped,column="grouped",repr="grouped"),
  "No colors were specified"
)

expect_warning(
showLCZ(redonBDTgrouped,column="grouped",repr="grouped",
        typeLevels=c("urban","industry","vegetation","impervious","pervious","water")),
  "No colors were specified"
)
