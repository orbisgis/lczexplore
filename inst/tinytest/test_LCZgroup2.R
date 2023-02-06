# This tests the function LCZgroup2
# library(tinytest)
#
# library(sf)

redonBDTgrouped<-LCZgroup2(redonBDT,column="LCZ_PRIMARY",urban=c("1","2","3","4","5","6","7","8","9"),
                          industry="10",
                          vegetation=c("101","102","103","104"),
                          impervious="105",pervious="106",water="107",cols=c("red","black","green","grey","burlywood","blue"))
expect_silent(redonBDTgrouped<-LCZgroup2(redonBDT,column="LCZ_PRIMARY",urban=c("1","2","3","4","5","6","7","8","9"),
                                          industry="10",
                                          vegetation=c("101","102","103","104"),
                                          impervious="105",pervious="106",water="107",cols=c("red","black","green","grey","burlywood","blue")))


redonOSMgrouped<-LCZgroup2(redonOSM,column="LCZ_PRIMARY",urban=c("1","2","3","4","5","6","7","8","9"),
                           industry="10",
                           vegetation=c("101","102","103","104"),
                           impervious="105",pervious="106",water="107",cols=c("red","black","green","grey","burlywood","blue"))




