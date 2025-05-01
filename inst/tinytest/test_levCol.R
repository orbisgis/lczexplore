# This tests the function levCol.R
# library(tinytest)
# library(lczexplore)
# library(dplyr)
# library(sf)

dealtWithCases<-c("1","2","3","3.1","4","5","6","7","8","13")

redonBDTgrouped<-groupLCZ(redonBDT,column="LCZ_PRIMARY",urban=c("1","2","3","4","5","6","7","8","9"),
                           industry="10",
                           vegetation=c("101","102","103","104"),
                           impervious="105",pervious="106",water="107",colors=c("red","black","green","grey","burlywood","blue"))

# several vector covering the levels in the data, with colors of the proper size

# case 0: The number of levels must be less than 37 for the map to be readable,
#               you may want to group some of the levels using groupLCZ function
expect_error(
  levCol(sf=redonBDT, column="LCZ_PRIMARY",1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18,
       19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38,colors="black"),
  "more than 36 arguments")

# case 1: No level vector and no color vector, less than 36 levels,
# levels will be deduced from the data
# and colors will be chosen from a standard palette. Or there are a level vector and a color vector, but they are empty.

test<-levCol(redonBDTgrouped,column="grouped",levels="",colors="")
expect_equal(grep("1:",test$case),1)


test<-levCol(sf=redonBDT, column="LCZ_PRIMARY")
expect_equal(grep("1:",test$case),1)

# case 2: No levels but a color vector which size covers the number of levels in the data

test<-levCol(sf=redonBDTgrouped,column="grouped",
             colors=c("red","black","green","grey","burlywood","blue"))
expect_equal(grep("3.1 :",test$case),1)

# case 3: No levels but a color vector which size does not cover the number of levels in the data,
# colors will be picked from a standard palette..

test<-levCol(sf=redonBDTgrouped,column="grouped",
             colors=c("red","black","green","grey","burlywood"))
expect_equal(grep("3.1 :",test$case),1)

test<-levCol(sf=redonBDTgrouped,column="grouped",
                   colors=c("red","black","green","grey","burlywood","blue","purple"))






# case 4:  A single vector was provided, whose names cover the levels in the data
# and whose values are colors.
test<-levCol(redonBDTgrouped, column="grouped", levels=c("urban"="red","industry"="black",
                                                         "vegetation"="green",
                                                         "impervious"="grey",
                                                         "pervious"="burlywood","water"="blue"))
expect_equal(grep("4:",test$case),1)

# case 5: A single vector was provided, whose values are colors
#         but whose names don't cover the levels in the data.
#         Specified colors will be associated to levels deduced from the data,
#          in order of appearence.
test<-levCol(redonBDTgrouped, column="grouped", drop=TRUE, levels=c("urban"="red","industry"="black",
                                                         "vegetation"="green",
                                                         "impervious"="grey",
                                                         "not in data"="white",
                                                         "pervious"="burlywood","water"="blue"))
expect_equal(grep("5:",test$case),1)

# case 6: A single vector was provided, whose values are colors
#         but whose names don't cover the levels in the data.
#         Colors will be associated to unique values of the data
#         and missing colors will be added from a standard palette.
test<-levCol(redonBDTgrouped, column="grouped", levels=c("urban"="red","industry"="black",
                                                         "vegetation"="green",
                                                         "pervious"="burlywood","water"="blue"))
expect_equal(grep("6:",test$case),1)

# case 7: No color vector but a level vector whose names cover the levels in the data
test<-levCol(redonBDTgrouped, column="grouped",
             levels=c("urban","industry","vegetation","impervious","pervious","water"))
expect_equal(grep("7:",test$case),1)

# case 8: No color vector but a level vector whose names don't cover the levels in the data
#          Levels will be deduced from data and colors will be chosen from a standard palette.
test<-levCol(redonBDTgrouped, column="grouped",
             levels=c("urban","industry","John Scofield","impervious","pervious","water"))
expect_equal(grep("8:",test$case),1)

test<-levCol(redonBDTgrouped, column="grouped",
             levels=c("industry","John Scofield","impervious","pervious","water"))
expect_equal(grep("8:",test$case),1)

test<-levCol(redonBDTgrouped, column="grouped", drop=TRUE,
             levels=c("industry","John Scofield","impervious","pervious","water"))
expect_equal(grep("8:",test$case),1)


# case 9: Levels specified in one vector, whose values cover the levels in the data,
#           colors in another vector, these vectors having the same length
test<-levCol(redonBDTgrouped, column="grouped",
             levels=c("urban","industry","vegetation","impervious","pervious","water"),
             colors=c("red","black","green","grey","burlywood","blue"))

expect_equal(grep("9:",test$case),1)

# case 10: Levels specified in one vector, whose values cover the levels in the data,
#           colors in another vector, these vectors having the same length
#           BUT some of the color names are not recognized as a color
#           and will be replaced from a standard palette

test<-levCol(redonBDTgrouped, column="grouped",
             levels=c("urban","industry","vegetation","impervious","pervious","water"),
             colors=c("red","black","green","grey","chaussures","blue"))

expect_equal(grep("10:",test$case),1)

# case 11: One vector seems to be a vector of levels,
#           which covers the values of the data,
#           the other a vector of colors, whose length is shorter than the specified levels.
#           Missing colors will be picked from a standard palette.

test<-levCol(redonBDTgrouped, column="grouped",
             levels=c("urban","industry","vegetation","impervious","pervious","water"),
             colors=c("red",  "black",   "green",     "grey",       "blue"))


expect_equal(grep("11:",test$case),1)

# case 12: One vector seems to be a vector of levels,
#           which covers the values of the data,
#           the other a vector of colors, whose length is longer than the specified levels.
#           The supplemental colors will be dropped.

test<-levCol(redonBDTgrouped, column="grouped",
             levels=c("urban","industry","vegetation","impervious","pervious","water"),
             colors=c("red","black","green","grey","burlywood","blue","purple"))

expect_equal(grep("12:",test$case),1)

# case 13: No color vector is specified, there seems to be two ambiguous level vectors,
#         levels will be deduced from the data and colors chosen from a standard palette.
test<-levCol(redonBDTgrouped, column="grouped",
             level1=c("urban","industry","vegetation"),
             level2=c("impervious","pervious","water"))
expect_equal(grep("13:",test$case),1)

# case 14.0:The level vector doesn't cover the levels in the data
#        and the number of specified colors is zero or less than the number of levels present,
#        levels will be deduced from the data and colors will be chosen from a standard palette.

test<-levCol(redonBDTgrouped, column="grouped",
       levels=c("urban","industry","vegetation","pervious","water"),
       colors=c("red","black","green","grey","blue"))
expect_equal(grep("9:",test$case),1)
expect_equal(grep("6:",test$case),1)

# case 14: The specified levels don't cover the levels in the data
#        and the number of specified colors is zero or less than the number of levels present,
#        levels will be deduced from the data and colors will be chosen from a standard palette.

test<-levCol(sf=redonBDTgrouped, column="grouped",
       industry="10",
       vegetation=c("101","102","103","104"),
       impervious="105",pervious="106",water="107",
       colors=c("red","black","green","grey","burlywood"))


# case 15: The specified levels don't cover the levels in the data
# but the number of the specified colors is greater or equal
# to the number of levels present in the data,
# they are matched in the order of appearence.

test<-levCol(sf=redonBDTgrouped,column="grouped",urban=c("1","2","3","4","5","6","7","8","9"), 
       vegetation=c("101","102","103","104"),
       impervious="105",pervious="106",water="107",
       colors=c("red","black","green","grey","burlywood","blue","orange"))
expect_equal(grep("15:",test$case),1)

# case 15.1 : The specified levels cover the levels in the data
# # but the number of the specified colors is greater than the number of levels present in the data,
# # they are matched in the order of appearence.

test<-levCol(sf=redonBDTgrouped,column="grouped",urban=c("1","2","3","4","5","6","7","8","9"),
             industry="10",
             vegetation=c("101","102","103","104"),
             impervious="105",pervious="106",water="107",
             colors=c("red","black","green","grey","burlywood","blue","orange"))
expect_equal(grep("12:",test$case),1)

# case 16: The specified levels cover the levels in the data
#        and no colors were specified, colors will be chosen from a standard palette.
test<-levCol(sf=redonBDTgrouped,column="grouped",urban=c("1","2","3","4","5","6","7","8","9"),
             industry="10",
             vegetation=c("101","102","103","104"),
             impervious="105",pervious="106",water="107",
             )
expect_equal(grep("16:",test$case),1)

# case 17: Several arguments are specified, whose names cover
#           the levels in the data and are associated with a vector of colors of the same size.
test<-levCol(sf=redonBDTgrouped,column="grouped",urban=c("1","2","3","4","5","6","7","8","9"),
       industry="10",
       vegetation=c("101","102","103","104"),
       impervious="105",pervious="106",water="107",
       colors=c("red","black","green","grey","burlywood","blue"))


expect_equal(grep("17:",test$case),1)

# case 18 : Several arguments are specified, whose names cover
#           the levels in the data but the associated vector of colors
#           contains names which are not colors.
#           These will be replaced by colors from a standard palette.

test<-levCol(sf=redonBDTgrouped,column="grouped",urban=c("1","2","3","4","5","6","7","8","9"),
       industry="10",
       vegetation=c("101","102","103","104"),
       impervious="105",pervious="106",water="107",
       colors=c("red","black","green","grey","DJ Shadow","blue"))



expect_equal(grep("18:",test$case),1)

redonBDTgrouped<-groupLCZ(redonBDT,column="LCZ_PRIMARY",urban=c("1","2","3","4","5","6","7","8","9"),
                           industry="10",
                           vegetation=c("101","102","103","104"),
                           impervious="105",pervious="106",water="107",colors=c("red","black","green","grey","burlywood","blue"))

# levCol(sf=redonBDTgrouped,
#        column="grouped",
#        LCZlevels=c("urban","industry","vegetation","impervious","pervious","water"),colors="")


rm(test)
rm(redonBDTgrouped)

# levCol(sf=redonBDTgrouped,column="LCZ_PRIMARY",urban=c("1","2","3","4","5","6","7","8","9"),
#        industry="10",
#        vegetation=c("101","102","103","104"),
#        impervious="105",pervious="106",water="107",
#        colors=c("red","black","green","grey","chaussure","blue"))
