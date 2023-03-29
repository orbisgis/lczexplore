#' Manages the levels and the colors of the LCZ columns
#'
#' @param sf is the input sf file
#' @param column is the column that contains the data
#' @param drop is set to TRUE if one wants to drop unused levels, in case column is a factor with unused levels
#' @param ... other parameters specified, expected a vector of levels and a vector of colors
#' whose name must begin with cols. Other cases are handled to enhance usability.
#' @import dplyr sf
#' @importFrom grDevices palette.colors
#'
#' @return output is a list containing levelColors, a named vector, which names are the levels
#' present in the data and which values are the associated colors,
#' and case, a string spcifying what case was encountered when producing the levels and colors.
#' @export
#'
#' @examples
#' redonBDTgrouped<-LCZgroup2(redonBDT,column="LCZ_PRIMARY",
#' urban=c("1","2","3","4","5","6","7","8","9"),
#' industry="10",
#' vegetation=c("101","102","103","104"),
#' impervious="105",pervious="106",water="107",
#' cols=c("red","black","green","grey","burlywood","blue"))
levCol<-function(sf,column,drop=FALSE,...){
  args<-list(...)

  if (length(args)>=37){ stop("This function can not deal with more than 36 arguments.
  You can use the function LCZgroup2 to group some levels.")}

  #

  uniqueData<-sf[column] |> sf::st_drop_geometry()  |> unique() # Attention unique outputs a list of length 1
  if(drop==TRUE){uniqueData<-droplevels(uniqueData)}
  uniqueData<-levels(uniqueData[,1]) |> as.character() |> as.vector()

  if(length(uniqueData)>36){ stop(
    "This package is not suited for classification with more than 36 levels or types.
  You can use the function LCZgroup2 to group some levels.") }

  # print("uniqueData") ; print(uniqueData)


  argNames<-names(args)
  indCol<-grep(x=argNames, pattern="cols")
  if (length(indCol) != 0) {
    if (length(indCol)>1 ) {
      stop(
        "only one argument can start with cols, and it must contain the colors,
      please rename your arguments and retry.")} else {
      argCol <- args[indCol][[1]]
      # print("argCol");print(argCol)
      argLev<-args[-indCol]
      if (prod(argCol=="")==1){
          args<-args[-indCol]
          argCol<-NULL
        }}} else
        {
          argCol<-NULL
          argLev<-args
  }


  # print("prod(unlist(args)==chainevide"); print(prod(unlist(args)==""))
  # print("length(args)"); print(length(args))
  if(length(argLev)>36){ stop("This package is not suited for classification with more than 36 levels or types.
  You can use the function LCZgroup2 to group some levels.") }



# argLev has to be treatd differently if input is one vector with the levels or
# if each level has an argument.

  # Define cases
  # Case : no arguments at all in (...), or cols and other arguments are NULL
  if (length(args) == 0 ||
    (is.null(args)) ||
    (prod(unlist(args)=="")==1)){
      if (length(uniqueData) > 36)
      {
        case<-"Too many levels"
        stop("0: The number of levels must be less than 37 for the map to be readable,
              you may want to group some of the levels using LCZgroup2 function ")
      } else {
        case<-"1: No level vector and no color vector, less than 36 levels,
        levels will be deduced from the data
        and colors will be chosen from a standard palette."
        # print("length(uniqueData)");print(length(uniqueData))
        typeLevels<-palette.colors(n=length(uniqueData), palette="Polychrome 36")
        names(typeLevels)<-uniqueData
      }
  }

 # Case (...) contains only one argument (color OR levels)
  if(length(args) == 1) {
    # print("length(argLev)");print(length(argLev))
    # print("argLev");print(argLev)
    # typeLevels<-names(argLev)
    # print("typeLevels"); print(typeLevels)

    if (length(indCol) == 1 && argCol!="" && !is.null(argCol))
    {
      if (length(argCol) == length(uniqueData)){
           if ( prod(areColors(argCol)==1)){
             case<-"2: No level vector, but a color vector
             which size covers the number of levels in the data."
             typeLevels<-argCol
             names(typeLevels)<-uniqueData
      } else {
        case<-"2.1 : No level vector, but a color vector which size covers the number of levels in the data.
        Some of the specified colors are not recognized as colors and will be replaced by colors from a standard palette."
        colFalse<-!areColors(argCol)
        typeLevels<-argCol
        typeLevels[colFalse]<-palette.colors(
          n=sum(as.numeric(colFalse)), palette="Polychrome 36")
        names(typeLevels)<-uniqueData
        }
      }
     else
      {
      case<-"3: No levels but a color vector which size does not cover the number of levels in the data,
      colors will be picked from a standard palette. "
      typeLevels<-palette.colors(n=length(uniqueData), palette="Polychrome 36")
      names(typeLevels)<-uniqueData
      }
    }
  if (is.null(argCol))
    {
     if (prod(areColors(argLev[[1]]))==1)
     {
       # print("uniqueData");print(uniqueData);print("names(argLev) indcol is null");print(names(argLev[[1]]))

          if(prod(uniqueData%in%names(argLev[[1]]))==1 & length(uniqueData)==length(argLev[[1]]))
      {
        case<-"4: A single vector was provided, whose names cover the levels in the data
        and whose values are colors."
        typeLevels<-argLev[[1]]
        names(typeLevels)<-names(argLev[[1]])
      } else
      {

          if(length(uniqueData)<=length(argLev[[1]]))
          {
            case<-case<-"5: A single vector was provided, whose values are colors
        but who includes names of levels not present in the data.
        Specified colors will be associated to levels deduced from the data,
         in order of appearence. "
            typeLevels<-argLev[[1]][seq_along(uniqueData)]
            names(typeLevels)<-uniqueData
          } else
          {
            case<-"6: A single vector was provided, whose values are colors
        but whose names don't cover the levels in the data.
        Colors will be associated to unique values of the data
        and missing colors will be added from a standard palette. "
            typeLevels<-c(argLev[[1]],
                palette.colors(n=length(uniqueData)-length(argLev[[1]]), palette="Polychrome 36"))
            names(typeLevels)<-uniqueData
          }
      }
     }
         else
     {
       if (prod(uniqueData%in%argLev[[1]])==1)
       {
         case<-"7: No color vector but a level vector whose names cover the levels in the data"
         typeLevels<-palette.colors(n=length(argLev[[1]]), palette="Polychrome 36")
         names(typeLevels)<-argLev[[1]]
       } else
       {
         case<-"8: No color vector but a level vector whose names don't cover the levels in the data
         Levels will be deduced from data and colors will be chosen from a standard palette."
         typeLevels<-palette.colors(n=length(uniqueData),palette="Polychrome 36")
         names(typeLevels)<-uniqueData
       }


     }

    }
  }


# Case (...) contains 2 argument, hopefully a vector of levels and a vector of colors
  if ( length(args)==2 &&
    !(prod(unlist(args)=="")==1)) {

      # print("length(argLev)");print(length(argLev))
      # print("argLev");print(argLev)
      typeLevels<-argLev
      # print("typeLevels"); print(typeLevels)

      if(length(indCol==1))
      { if ( length(argLev)<length(uniqueData) || argCol=="" ){
        case<-"14.0: The vector level doesn't cover the levels in the data
       and the number of specified colors is zero or less than the number of levels present,
       levels will be deduced from the data and colors will be chosen from a standard palette."
        typeLevels<-palette.colors(n=length(uniqueData), palette="Polychrome 36")
        names(typeLevels)<-uniqueData
        }
        if(length(argCol)==length(argLev[[1]]))
        {
          if(prod(areColors(argCol))==1)
          {
            if (prod(uniqueData%in%argLev[[1]])==1)
            {
              case<-"9: Levels specified in one vector, whose values cover the levels in the data,
          colors in another vector, these vectors having the same length"
              typeLevels<-argCol
              names(typeLevels)<-argLev[[1]]
            }

          } else {
            case<-"10: Levels specified in one vector, whose values cover the levels in the data,
          colors in another vector, these vectors having the same length
          BUT some of the color names are not recognized as a color
          and will be replaced from a standard palette"
           colFalse<-!areColors(argCol)
           typeLevels<-argCol
           typeLevels[colFalse]<-palette.colors(
             n=sum(as.numeric(colFalse)), palette="Polychrome 36")
            names(typeLevels)<-argLev[[1]]
          }
        } else
        {
          if (prod(uniqueData%in%argLev[[1]])==1){

           if(length(uniqueData)>length(argCol)){
             case<-"11: One vector seems to be a vector of levels,
          which covers the values of the data, the other a vector of colors,
           who is empty or whose length is shorter than the specified levels.
          Missing colors will be picked from a standard palette.
          For a better rendition specify as many colors as levels."


             LCZlevels<-argLev[[1]]
             typeLevels<-palette.colors(n=length(LCZlevels), palette="Polychrome 36")
             indOK<-match(uniqueData,LCZlevels)[!is.na(match(uniqueData,LCZlevels))]
             typeLevels[indOK]<-argCol[indOK]
             names(typeLevels)<-LCZlevels
             } else if (length(uniqueData)<=length(argCol)){
             case<-"12: One vector seems to be a vector of levels,
          which covers the values of the data,
          the other a vector of colors, whose length is longer than the specified levels.
          The supplemental colors will be dropped."
             typeLevels<-argCol[1:length(uniqueData)]
             names(typeLevels)<-argLev[[1]]
           }
          }
      }
    }
      else {
        case<-"13: No color vector is specified, there seems to be two ambiguous level vectors,
        levels will be deduced from the data and colors chosen from a standard palette."
        typeLevels<-palette.colors(n=length(uniqueData), palette="Polychrome 36")
        names(typeLevels)<-uniqueData

      }
    }

# Case (...) contain more than 2 arguments,
  # hopefully an argument for each level and zero or one vector of colors

  if ( length(args) > 2 )
  {
    # print("length(args)");print(length(args))
    # print("length(argLev)");print(length(argLev))
    # print("argLev");print(argLev)
    LCZlevels<-names(argLev)
    # print("typeLevels"); print(typeLevels)
    ### GERER L'APPEL À LCZgroup2 DANS CE CAS
     if(prod(uniqueData%in%LCZlevels)==0 && length(argCol)<=length(uniqueData)){
       case<-"14: The specified levels don't cover the levels in the data
       and the number of specified colors is zero or less than the number of levels present,
       levels will be deduced from the data and colors will be chosen from a standard palette."
       typeLevels<-c(palette.colors(n=length(uniqueData), palette="Polychrome 36"))
       indOK<-match(uniqueData,LCZlevels)[!is.na(match(uniqueData,LCZlevels))]
       typeLevels[indOK]<-argCol[indOK]
       names(typeLevels)<-uniqueData
      } else if (prod(uniqueData%in%LCZlevels)==0 && length(argCol)>=length(uniqueData)){
         case<-"15: The specified levels don't cover the levels in the data
         but the number of specified colors is greater or equal
         to the number of levels present in the data,
         they are matched in the order of appearence."
         colTemp<-argCol
         names(colTemp)[1:length(LCZlevels)]<-LCZlevels
         allLev<-unique(c(LCZlevels,uniqueData))
         colN<-palette.colors(n=length(allLev),palette = "Polychrome 36")
         names(colN)<-allLev
         indOK<-match(names(colN),LCZlevels)[!is.na(match(names(colN),LCZlevels))]
         colN[indOK]<-colTemp[indOK]
         typeLevels<-colN
       }
     else if (prod(uniqueData%in%LCZlevels)==1 && length(argCol)>length(uniqueData)){
       case<-"15.1: The specified levels cover the levels in the data
         but the number of specified colors is greater or equal
         to the number of levels present in the data,
         they are matched in the order of appearence and
         levels non specified will be matched with color from a standard palette.
         Maybe recheck your levels and colors. "
       typeLevels<-argCol[seq_along(LCZlevels)]
       names(typeLevels)<-LCZlevels
     }
     else if (prod(uniqueData%in%LCZlevels)==1 && length(argCol)==0 ){
       case<-"16: The specified levels cover the levels in the data
       and no colors were specified, colors will be chosen from a standard palette."
       typeLevels<-palette.colors(n=length(LCZlevels), palette="Polychrome 36")
       names(typeLevels)<-LCZlevels
     }
       else if (prod(uniqueData%in%LCZlevels)==1 && length(argCol)==length(argLev)){
          if(prod(areColors(argCol))==1){
            case<-"17: Several arguments are specified, whose names cover
          the levels in the data and are associated with a vector of colors of the same size.
          Evrything seems OK and these levels and colors will be used."
            typeLevels<-argCol
            names(typeLevels)<-LCZlevels
          } else {
            # print("length(args)");print(length(args))
            # print("length(argCol)"); print(length(argCol))
            case<-"18: Several arguments are specified, whose names cover
          the levels in the data but the associated vector of colors
          contains names which are not colors.
          These will be replaced by colors from a standard palette."
            colFalse<-!areColors(argCol)
            typeLevels<-argCol
            typeLevels[colFalse]<-palette.colors(
              n=sum(as.numeric(colFalse)), palette="Polychrome 36")
            names(typeLevels)<-LCZlevels
          }
        }

  }


output<-list(levelsColors=typeLevels,case=case)
  message(case)
return(output)
}

#library(lczexplore)
#library(tinytest)
 # redonBDTgrouped<-LCZgroup2(redonBDT,column="LCZ_PRIMARY",outCol = "grouped",urban=c("1","2","3","4","5","6","7","8","9"),
 #                           industry="10",
 #                           vegetation=c("101","102","103","104"),
 #                           impervious="105",pervious="106",water="107",
 #                            cols=c("red","black","green","grey","burlywood","blue"))

#summary(redonBDTgrouped$grouped)




