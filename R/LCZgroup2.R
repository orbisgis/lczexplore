#' Allows to group local climate zones to improve the analysis.
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
#' redonBDTgrouped<-LCZgroup2(redonBDT,column="LCZ_PRIMARY",
#' urban=c("1","2","3","4","5","6","7","8","9"),
#' industry="10", vegetation=c("101","102","103","104"),
#' impervious="105",pervious="106",water="107",
#' cols=c("red","black","green","grey","burlywood","blue"))
LCZgroup2<-function(sf,column,outCol='grouped',...)
{
  #require(forcats)
  #require(dplyr)

  # ensure all the LCZ levels are present in the imported column
  uniqueData<-sf[column] |> sf::st_drop_geometry()  |> unique() # Attention unique outputs a list of length 1
  uniqueData<-levels(uniqueData[,1]) |> as.character() |> as.vector()

  # typeLevels<-c(1:10,101:107)
  sf<- sf %>%  mutate(!!column:=factor(st_drop_geometry(subset(sf,select=column,drop=T)) ,levels=uniqueData))
  temp<-subset(sf,select=column,drop=T)

    # get the grouping levels as passed by ..., but without keeping arguments about colours
  args<-list(...)
  indSep<-names(args)
  indCol<-grep(x=indSep,pattern="col")
  #options(warn=2)
     if(is.null(indCol)) {
       temp<-do.call(fct_collapse,args)
    # temp<-tryCatch(
    # {do.call(fct_collapse,args)},
    #   warning=function(w){
    #     message(" The specified levels don't seem to cover the levels in the specified column.
    #       The unspecified levels will be kept as their own group ")
    #     return(temp)
    #   },
    # error=function(e){stop(" an unknown error occured while grouping")}
    # )

  } else {
    args2<-args[indSep[-indCol]]
    args2<-append(list(temp),args2)
    temp<-do.call(fct_collapse,args2)
    # temp<-tryCatch(
    #    {do.call(fct_collapse,args2)},
    #    warning=function(w){
    #      message(" The specified levels don't seem to cover the levels in the specified column.
    #       The unspecified levels will be kept as their own group ",w$message)
    #      return(temp)
    #    },
    #    error=function(e){
    #      stop(" an unknown error occured while grouping")}
    #    )
  }
  #options(warn=0)
  #levels(temp)<-etiquettes
  sf<- sf %>%  mutate(!!outCol:=temp)
  return(sf)
  sf
}


