redonBDTgrouped<-LCZgroup2(redonBDT,column="LCZ_PRIMARY",urban=c("1","2","3","4","5","6","7","8","9"),
                           industry="10",
                           vegetation=c("101","102","103","104"),
                           impervious="105",pervious="106",water="107",colors=c("red","black","green","grey","burlywood","blue"))
redonBDTgrouped$grouped %>% summary
showLCZ(redonBDT)
showLCZ(redonBDTgrouped,column="grouped",repr="grouped",niveaux=c("urban","industry","vegetation","impervious","pervious","water"),
                                                   cols=c("red","black","green","grey","burlywood","blue"))
showLCZ(redonBDTgrouped,column="grouped",repr="grouped",niveaux=c("urban","industry","vegetation","impervious","pervious","water"),
        cols=c("red","black","green","grey","blue"))

showLCZ(redonBDTgrouped,column="grouped",repr="grouped")
showLCZ(redonBDTgrouped,column="grouped",repr="grouped",
        niveaux=c("urban","industry","vegetation","impervious","pervious","water"))

testf<-function(jeu){
  print("le jeu de données qu'on teste s'appelle ")
  print(deparse(substitute(jeu)))
}
testf(redonBDT)
