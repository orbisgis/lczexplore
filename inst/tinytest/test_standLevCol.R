# This tests the function standLevCol.R
# library(tinytest)
# library(lczexplore)
#
# library(sf)


## Test when no colors are specified
# Test first 10 LCZ
levels <- as.character(1:10)
expect_equal(standLevCol(levels = levels, colors = "", useStandCol = TRUE),
             c("1" = "#8b0101", "2" = "#cc0200", "3" = "#fc0001", "4" = "#be4c03", "5" = "#ff6602", "6" = "#ff9856",
               "7" = "#fbed08", "8" = "#bcbcba", "9" = "#ffcca7", "10" = "#57555a"))

# Test levels with different conventions
levels <- as.character(11:17)
expect_equal(standLevCol(levels = levels, colors = "", useStandCol = TRUE),
             c("11" = "#006700", "12" = "#05aa05", "13" = "#648423", "14" = "#bbdb7a", "15" = "#010101",
               "16" = "#fdf6ae", "17" = "#6d67fd"))

levels <- LETTERS[1:7]
test <- standLevCol(levels = levels, colors = "", useStandCol = TRUE)
expect_equal(standLevCol(levels = levels, colors = "", useStandCol = TRUE),
             c("A" = "#006700", "B" = "#05aa05", "C" = "#648423", "D" = "#bbdb7a", "E" = "#010101",
               "F" = "#fdf6ae", "G" = "#6d67fd"))

levels <- as.character(101:107)
expect_equal(standLevCol(levels = levels, colors = "", useStandCol = TRUE),
             c("101" = "#006700", "102" = "#05aa05", "103" = "#648423", "104" = "#bbdb7a", "105" = "#010101",
               "106" = "#fdf6ae", "107" = "#6d67fd"))

levels <- c("1", "2", "A", "intrus")
expect_equal(standLevCol(levels = levels, colors = "", useStandCol = TRUE),
             c("1" = "#8b0101", "2" = "#cc0200", "A" = "#006700", "intrus" = "#FE00FA"))

### Check when standard colors are specified
levels <- c("11", "102", "C")
colors <- c("#006700", "#05aa05", "#648423")
expect_message(standLevCol(levels = levels, colors = colors, useStandCol = TRUE),
               "the specified colors may have been over-ridden")
expect_equal(standLevCol(levels = levels, colors = colors, useStandCol = TRUE),
             c("11" = "#006700", "102" = "#05aa05", "C" = "#648423"))

### Check when non standard colors are specified
#useStandCol=TRUE
levels <- c("11", "102", "C")
colors <- c("#006700", "#05aa05", "black")
expect_message(standLevCol(levels = levels, colors = colors, useStandCol = TRUE),
               "the specified colors may have been over-ridden")
expect_equal(standLevCol(levels = levels, colors = colors, useStandCol = TRUE),
             c("11" = "#006700", "102" = "#05aa05", "C" = "#648423"))

#useStandCol=FALSE
levels <- c("11", "102", "C")
colors <- c("#006700", "#05aa05", "black")
# expect_message(standLevCol(levels=levels, colors=colors,useStandCol=FALSE),
#                "the specified colors may have been over-ridden")
expect_equal(standLevCol(levels = levels, colors = colors, useStandCol = FALSE),
             c("11" = "#006700", "102" = "#05aa05", "C" = "black"))
