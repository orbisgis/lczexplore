LCZgroup2<-function(sf,column,...)
{
  require(forcats)
  require(dplyr)

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
