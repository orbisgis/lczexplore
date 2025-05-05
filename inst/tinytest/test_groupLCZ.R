# This tests the function groupLCZ
# library(tinytest)
#
# library(sf)

redonBDTgrouped<-groupLCZ(redonBDT,column="LCZ_PRIMARY",urban=c("1","2","3","4","5","6","7","8","9"),
                          industry="10",
                          vegetation=c("101","102","103","104"),
                          impervious="105",pervious="106",water="107"
                           )

redonBDTgrouped<-groupLCZ(redonBDT,column="LCZ_PRIMARY",urban=c("1","2","3","4","5","6","7","8","9"),
                           industry="10",
                           vegetation=c("101","102","103","104"),
                           impervious="105",pervious="106",water="107",
                           colors=c("red","black","green","grey","burlywood","blue")
)

expect_warning(
  redonBDTgrouped<-groupLCZ(redonBDT,column="LCZ_PRIMARY",urban=c("1","2","3","4","5","6","7","8","9"),
                                          industry="10",
                                          vegetation=c("101","102","103","104"),
                                          impervious="105",pervious="106",water="107",
                                         colors=c("red","black","green","grey","burlywood","blue")), "Unknown levels"
)


expect_warning(redonOSMgrouped<-groupLCZ(redonOSM,column="LCZ_PRIMARY",urban=c("1","2","3","4","5","6","7","8","9"),
                           industry="10",
                           vegetation=c("101","102","103","104"),
                           impervious="105",pervious="106",water="107",
                           colors=c("red","black","green","grey","burlywood","blue")),
   "Unknown levels"
)



#test the outCol feature
expect_warning(redonBDTgrouped2<-groupLCZ(redonBDT,column="LCZ_PRIMARY", outCol="groupedLCZ", urban=c("1","2","3","4","5","6","7","8","9"),
                           industry="10",
                           vegetation=c("101","102","103","104"),
                           impervious="105",pervious="106",water="107",
                           colors=c("red","black","green","grey","burlywood","blue"))
  , "Unknown levels")

expect_warning(redonOSMgrouped2<-groupLCZ(redonOSM,column="LCZ_PRIMARY",outCol="otherName",
                           urban=c("1","2","3","4","5","6","7","8","9"),
                           industry="10",
                           vegetation=c("101","102","103","104"),
                           impervious="105",pervious="106",water="107",
                           colors=c("red","black","green","grey","burlywood","blue")),
   "Unknown levels"
)

expect_message(
  showLCZ(redonBDTgrouped2,column="groupedLCZ",repr="alter",
        LCZlevels=c("urban","industry","vegetation","impervious","pervious","water"),
        colors=c("red","black","green","grey","burlywood","blue")),
               "case 9: "

)

# compareLCZ(sf1=redonBDTgrouped2, column1="groupedLCZ", wf1="BDT",
#            sf2=redonOSMgrouped2,column2="otherName",wf2="osm", exwrite=FALSE, repr="alter", plot=T,
#            urban="urban",industry="industry",vegetation="vegetation",impervious="impervious",pervious="pervious",water="water",
#            colors=c("red","black","green","grey","burlywood","blue") )

#test the non-coverage of levels

expect_message(
redonBadLevels<-groupLCZ(redonOSM,column="LCZ_PRIMARY",outCol="otherName",
                          urban=c("1","2","3","4","5","6","7","8","chaussure"),
                          industry="10",
                          vegetation=c("101","102","103","104"),
                          impervious="105",pervious="106",water="107"),
"One of the specified levels to group doesn't exist in the data"
)