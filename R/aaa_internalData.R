.lczenv<-new.env()

.lczenv$typeLevelsDefault <-c(as.character(c(1:10,101:107)), "Unclassified")

.lczenv$typeLevelsConvert <- c("1" = "1", "2" = "2", "3" = "3", "4" = "4", "5" = "5", "6" = "6", "7" = "7", "8" = "8",
                               "9" = "9", "10" = "10",
                               "101" = "101", "102" = "102", "103" = "103", "104" = "104", "105" = "105", "106" = "106", 
                               "107" = "107",
                               "101" = "11", "102" = "12", "103" = "13", "104" = "14", "105" = "15", "106" = "16", 
                               "107" = "17",
                               "101" = "A", "102" = "B", "103" = "C", "104" = "D", "105" = "E", "106" = "F", 
                               "107" = "G", "Unclassified" = "Unclassified")

.lczenv$colorMapDefault <- c("#8b0101", "#cc0200", "#fc0001", "#be4c03", "#ff6602", "#ff9856",
              "#fbed08", "#bcbcba", "#ffcca7", "#57555a", "#006700", "#05aa05",
              "#648423", "#bbdb7a", "#010101", "#fdf6ae", "#6d67fd", "ghostwhite")
names(.lczenv$colorMapDefault) <- c(as.character(c(1:10,101:107)), "Unclassified")
.lczenv$etiquettesDefault <- c("LCZ 1: Compact high-rise", "LCZ 2: Compact mid-rise", "LCZ 3: Compact low-rise",
                "LCZ 4: Open high-rise", "LCZ 5: Open mid-rise", "LCZ 6: Open low-rise",
                "LCZ 7: Lightweight low-rise", "LCZ 8: Large low-rise",
                "LCZ 9: Sparsely built", "LCZ 10: Heavy industry",
                "LCZ A: Dense trees", "LCZ B: Scattered trees",
                "LCZ C: Bush,scrub", "LCZ D: Low plants",
                "LCZ E: Bare rock or paved", "LCZ F: Bare soil or sand", "LCZ G: Water", "Unclassified")

.lczenv$shortEtiquettesDefault <- c("Compact high-rise", "Compact mid-rise", "Compact low-rise",
                               "Open high-rise", "Open mid-rise", "Open low-rise",
                               "Lightweight low-rise", "Large low-rise",
                               "Sparsely built", "Heavy industry",
                               "Dense trees", "Scattered trees",
                               "Bush,scrub", "Low plants",
                               "Bare rock or paved", "Bare soil or sand", "Water", "Unclassified")


.lczenv$veryShortEtiquettesDefault <- c(paste0("00", 1:9), "010", LETTERS[1:7], "Unclassified")
