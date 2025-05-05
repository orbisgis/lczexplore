#' Allows to group (Local Climate Zone) geographical classification levels into broader categories 
#' to explore classification agreements
#'
#' @param sf is the input files. It must be an sf file and contain an LCZ column (levels 1 to 10 and 101 to 107). It must contain the geom column.
#' @param column is the name of the column containing the LCZ to be grouped
#' @param outCol is the name of the colum in which the grouped LCZ will be put
#' @param ... the names of the groups followed by the levels the regrouped for instance urban=c("1","2","3","4","5","6","7","8","9")
#' @import forcats dplyr rlang grDevices

#' @return a file containing the original geom and lcz, and a new grouped column
#' @export
#'
#' @examples
#' redonBDTgrouped<-groupLCZ(redonBDT,column="LCZ_PRIMARY",
#' urban=c("1","2","3","4","5","6","7","8","9"),
#' industry="10", vegetation=c("101","102","103","104"),
#' impervious="105",pervious="106",water="107",
#' colors=c("red","black","green","grey","burlywood","blue"))
#' 
#' showLCZ(redonBDT,column="LCZ_PRIMARY", repr="standard")
#' showLCZ(redonBDTgrouped,column="grouped",repr="alter",
#' LCZlevels=c("urban","industry","vegetation","impervious","pervious","water"),
#' colors=c("red","black","green","grey","burlywood","blue"),wf="BD TOPO")
groupLCZ<-function(sf, column, outCol='grouped', ...)
{
  #require(forcats)
  #require(dplyr)

  # ensure all the LCZ levels are present in the imported column
  uniqueData<-sf[[column]] %>% unique() %>% as.character # Attention unique outputs a list of length 1

  # typeLevels<-c(1:10,101:107)
  sf[[column]]<-factor(sf[[column]], levels=uniqueData)
  temp<-sf[[column]]

    # get the grouping levels as passed by ..., but without keeping arguments about colours
  args<-list(...)
  indSep<-names(args)
  indCol<-grep(x=indSep,pattern="col")
 

     if(length(indCol)==0) {
       args<-append(list(temp),args)
       # temp<-do.call(fct_collapse,args)
    temp<-
       tryCatch(expr=do.call(fct_collapse,args),
             warning=function(w){
               message("One of the specified levels to group doesn't exist in the data, if it is a mispelled level of the data,
               this level will be kept as ungrouped",w)
             return(
               do.call(fct_collapse,args)
             )
             })

  } else {
    args2<-args[indSep[-indCol]]
    args2<-append(list(temp),args2)

    temp<-
       tryCatch(expr=do.call(fct_collapse,args2),
              warning=function(w){
              message("One of the specified levels to group doesn't exist in the data, if it is a mispelled level of the data,
              this level will be kept as ungrouped",w)
              return(
                do.call(fct_collapse,args2)
)
                      })
  }

  sf[[outCol]]<-temp
  return(sf)
}