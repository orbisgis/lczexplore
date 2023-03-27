# This tests the function LCZgroup2
# library(tinytest)
#
# library(sf)

# redonBDTgrouped<-LCZgroup2(redonBDT,column="LCZ_PRIMARY",urban=c("1","2","3","4","5","6","7","8","9"),
#                           industry="10",
#                           vegetation=c("101","102","103","104"),
#                           impervious="105",pervious="106",water="107",
#                            cols=c("red","black","green","grey","burlywood","blue"))
expect_silent(redonBDTgrouped<-LCZgroup2(redonBDT,column="LCZ_PRIMARY",urban=c("1","2","3","4","5","6","7","8","9"),
                                          industry="10",
                                          vegetation=c("101","102","103","104"),
                                          impervious="105",pervious="106",water="107",
                                         cols=c("red","black","green","grey","burlywood","blue")))


expect_silent(redonOSMgrouped<-LCZgroup2(redonOSM,column="LCZ_PRIMARY",urban=c("1","2","3","4","5","6","7","8","9"),
                           industry="10",
                           vegetation=c("101","102","103","104"),
                           impervious="105",pervious="106",water="107",
                           cols=c("red","black","green","grey","burlywood","blue"))
)


#test the outCol feature
expect_silent(redonBDTgrouped2<-LCZgroup2(redonBDT,column="LCZ_PRIMARY", outCol="groupedLCZ", urban=c("1","2","3","4","5","6","7","8","9"),
                           industry="10",
                           vegetation=c("101","102","103","104"),
                           impervious="105",pervious="106",water="107",
                           cols=c("red","black","green","grey","burlywood","blue")))

expect_silent(redonOSMgrouped2<-LCZgroup2(redonOSM,column="LCZ_PRIMARY",outCol="otherName",
                           urban=c("1","2","3","4","5","6","7","8","9"),
                           industry="10",
                           vegetation=c("101","102","103","104"),
                           impervious="105",pervious="106",water="107",
                           cols=c("red","black","green","grey","burlywood","blue"))
)

expect_silent(showLCZ(redonBDTgrouped2,column="groupedLCZ",repr='grouped',
        LCZlevels=c("urban","industry","vegetation","impervious","pervious","water"),
        cols=c("red","black","green","grey","burlywood","blue"))
)

# compareLCZ(sf1=redonBDTgrouped2, column1="groupedLCZ", wf1="BDT",
#            sf2=redonOSMgrouped2,column2="otherName",wf2="osm", exwrite=FALSE, repr="grouped", plot=T,
#            urban="urban",industry="industry",vegetation="vegetation",impervious="impervious",pervious="pervious",water="water",
#            cols=c("red","black","green","grey","burlywood","blue") )

#test the non-coverage of levels

redonOSMgrouped<-LCZgroup2(redonOSM,column="LCZ_PRIMARY",
                           urban=c("1","2","3","4","5","6","7","8","9"),
                           industry="10",
                           vegetation=c("101","102","103","104"),
                           impervious="105",pervious="106",water="107",
                           cols=c("red","black","green","grey","burlywood","blue"))
redonGoodLevels<-LCZgroup2(redonOSM,column="LCZ_PRIMARY",outCol="otherName",
                           urban=c("1","2","3","4","5","6","7","8","9"),
                           industry="10",
                           vegetation=c("101","102","103","104"),
                           impervious="105",pervious="106",water="107",
                           cols=c("red","black","green","grey","burlywood","blue"))

redonBadLevels<-LCZgroup2(redonOSM,column="LCZ_PRIMARY",outCol="otherName",
          urban=c("1","2","3","4","5","6","7","8","chaussure"),
          industry="10",
          vegetation=c("101","102","103","104"),
          impervious="105",pervious="106",water="107",
          cols=c("red","black","green","grey","burlywood","blue"))
showLCZ(redonBadLevels,column = "otherName",cols=c("red","black","green","grey","burlywood","blue"))


showLCZ(sf=RedonBadLevels,column="otherName", repr="grouped")
