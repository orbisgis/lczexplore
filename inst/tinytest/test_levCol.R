# several vector covering the levels in the data, with colors of the proper size
levCol(sf=redonBDTgrouped,column="grouped",urban=c("1","2","3","4","5","6","7","8","9"),
       industry="10",
       vegetation=c("101","102","103","104"),
       impervious="105",pervious="106",water="107",
       cols=c("red","black","green","grey","burlywood","blue"))

# GERER L'APPEL A LCZGROUP2

# one vector covering the levels in the data with colors of the proper size
levCol(redonBDTgrouped,column="grouped",
       anything=c("urban","industry","vegetation","impervious","pervious","water"),
       cols=c("red","black","green","grey","burlywood","blue"))

# levels as the names and colors as the values
levCol(redonBDTgrouped, column="grouped", levels=c("urban"="red","industry"="black",
                                                   "vegetation"="green",
                                                   "impervious"="grey","pervious"="burlywood","water"="blue"))