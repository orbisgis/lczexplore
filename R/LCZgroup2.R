#' Allows to group local climate zones to improve the analysis.
#'
#' @param sf is the input files. It must be an sf file and contain an LCZ column (levels 1 to 10 and 101 to 107).
#' It must contain the geom column.
#' @param column is the name of the column containing the LCZ to be grouped
#' @param ...
#'
#' @return a file containing the original geom and lcz, and a new grouped column
#' @import forcats dplyr
#' @export
#'
#' @examples
LCZgroup2<-function(sf,column,...)
{
  #require(forcats)
  #require(dplyr)

  # ensure all the LCZ levels are present in the imported column
  niveaux<-c(1:10,101:107)
  sf<- sf %>%  mutate(!!column:=factor(subset(sf,select=column,drop=T),levels=niveaux))
  temp<-subset(sf,select=column,drop=T)

  # get the grouping levels as passed by ..., but without keeping levels about colours
  args<-list(...)
  indSep<-names(args)
  indCol<-grep(x=indSep,pattern="col")
  if(is.null(indCol)){temp<-do.call(fct_collapse,args)} else{
    args2<-args[indSep[-indCol]]
    args2<-append(list(temp),args2)
    temp<-do.call(fct_collapse,args2)
    }
  #levels(temp)<-etiquettes
  sf<- sf %>%  mutate(grouped=temp)
  return(sf)
  sf
}
