colorMap<-c("#8b0101","#cc0200","#fc0001","#be4c03","#ff6602","#ff9856",
            "#fbed08","#bcbcba","#ffcca7","#57555a","#006700","#05aa05",
            "#648423","#bbdb7a","#010101","#fdf6ae","#6d67fd", "ghostwhite")
names(colorMap)<-as.character(c(1:10,101:107, "Unclassified"))
etiquettes<-c("LCZ 1: Compact high-rise","LCZ 2: Compact mid-rise","LCZ 3: Compact low-rise",
              "LCZ 4: Open high-rise","LCZ 5: Open mid-rise","LCZ 6: Open low-rise",
              "LCZ 7: Lightweight low-rise","LCZ 8: Large low-rise",
              "LCZ 9: Sparsely built","LCZ 10: Heavy industry",
              "LCZ A: Dense trees", "LCZ B: Scattered trees",
              "LCZ C: Bush,scrub","LCZ D: Low plants",
              "LCZ E: Bare rock or paved","LCZ F: Bare soil or sand","LCZ G: Water", "Unclassified")