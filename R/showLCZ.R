#' Produces a simple representation of the LCZ contained in an sf file.
#'
#' @param sf is the sf file which contains the LCZ column to be plotted.
#' @param wf is the workflow used to produced the LCZ. "bdt" and "osm" indicate
#' that LCZ were produced by GeoClimate using the BD_TOPO_V2 or the Open Street Map data as input, respectively.
#' If the LCZ were produced by WUDAPT, use "wudapt".
#' @param column is the column that contains the LCZ.
#' @param repr indicates if the sf dataset contains standarde LCZ levels or grouped LCZ.
#' If "standard" then an optimal set of colors is used to produce the plotted map. Else, colors can be specified with the colors argument.
#' @param title allows the user to set the title of the plot
#' @param drop indicates if you want to show the levels present in no geometry.
#' @param useStandCol is set to TRUE implies that any levels detected as a standard LCZ level will receive the standard associated color
#' @param tryGroup is set to TRUE when one wants to group and plot on the fly 
#' @param ... these dynamic dots allow you to pass arguments to specify levels expected 
#' in your dataset and colors associated to these levels when not in the standard representation. You can pas your levels through a vector and you colors through another vector called colors. 
#' For more details about this, read the "lcz_explore_alter" vignette. 
#' @import sf ggplot2 dplyr cowplot forcats grDevices ggspatial
#' @return return the plot of the LCZ levels
#' @export
#' @examples 
#' # On original LCZ levels, use the \'standard\' value for the \'repr\' argument.
#' showLCZ(redonBDT, column="LCZ_PRIMARY", repr="standard")
#' # On grouped data, use the alter value for the repr argument.
#' redonBDTgrouped<-groupLCZ(redonBDT,column="LCZ_PRIMARY",
#' urban=c("1","2","3","4","5","6","7","8","9"),
#' industry="10", vegetation=c("101","102","103","104"),
#' impervious="105",pervious="106",water="107")
#' # For repr="alter", you can specify colors and levels this way :
#' showLCZ(redonBDTgrouped,column="grouped",repr="alter",
#' LCZlevels=c("urban","industry","vegetation","impervious","pervious","water"),
#' colors=c("red","black","green","grey","burlywood","blue"),wf="BD TOPO")
#' 
showLCZ <- function(sf, title = "", wf = "", column = "LCZ_PRIMARY",
                    repr = "standard", drop = FALSE, useStandCol = FALSE, tryGroup = TRUE,
                    naAsUnclassified = TRUE, noPerc = FALSE, plotNow = TRUE,  ...) {

  datasetName <- deparse(substitute(sf))


  try(class(sf)[1] == "sf", stop("Input data must be sf object"))

  if (wf != "") { nomLegende <- paste0("LCZ from ", wf, " workflow") } else { nomLegende <- "Levels" }

  if (repr == 'standard') {
    typeLevels <- typeLevelsDefault
    sf <-
      sf %>%
        mutate(!!column := fct_recode(
          factor(subset(sf, select = column, drop = T), levels = typeLevels), !!!typeLevels))  #%>%
    # 
    if (naAsUnclassified) { sf[[column]] <- forcats::fct_na_value_to_level(sf[[column]], "Unclassified") }
    else { sf <- drop_na(sf, column) }


    areas <- LCZareas(sf, column, LCZlevels = unique(names(typeLevelsDefault)))
    colorMap <- colorMapDefault
    if (!noPerc) { etiquettes <- paste(etiquettesDefault, ": ", areas$area, "%") }
    else { etiquettes <- etiquettesDefault }


    if (wf != "") { nomLegende <- paste0("LCZ from ", wf, " workflow") } else { nomLegende <- "LCZ" }

    ###### Shows the geoms with the original values of LCZ as described by Stewardt & Oke, and produced for instance by the GeoClimate workflow

    if (title == "") {
      if (wf != "") { wtitre <- paste("LCZ from", wf, "workflow, for ", datasetName, "dataset") } else {
        wtitre <- paste("LCZ from", datasetName, "dataset")
      }
    }else {
      wtitre <- title
    }

    if (drop == TRUE) {
      presentLevels <- levels(droplevels(subset(sf, select = column, drop = T)))
      temp <- subset(sf, select = column, drop = T) %>%
        factor(levels = presentLevels)
      sf <- sf %>% mutate(!!column := temp)
      presentIndices <- sapply(presentLevels, grep, x = etiquettes) %>%
        unlist %>%
        print
      colorMap <- colorMap[presentIndices]
      etiquettes <- etiquettes[presentIndices]
    }

    pstandard <- ggplot(sf) + # data
      geom_sf(data = sf, aes(fill = .data[[column]], color = after_scale(fill))) +
      scale_fill_manual(values = colorMap, labels = etiquettes, drop = FALSE) +
      
      guides(fill = guide_legend(nomLegende)) +
      ggspatial::annotation_north_arrow(
        location = "tl",
        width = unit(0.5, "cm"),
        height = unit(0.5, "cm"),
        pad_x = unit(0.15, "cm"),
        pad_y = unit(0.15, "cm"),
        # data = subset(allLocAllWfs[allLocAllWfs$location == aLocation,], wf == "osm"),
        style = north_arrow_orienteering(
          text_size = 5,
        )) +
      ggspatial::annotation_scale(
        location = "br",
        # data = subset(allLocAllWfs[allLocAllWfs$location == aLocation,], wf == "osm"),
        width_hint = 0.4,
        height = unit(0.1, "cm"),
        pad_x = unit(0.35, "in"),
        pad_y = unit(0.06, "in"),
        text_cex = 0.5,
        text_pad = unit(0.05, "cm"),
      ) +
      ggtitle(wtitre)
  }
  #

  ###### Shows other qualitative variables, like LCZ once they are regrouped in more general classes, 
  # for instance outputs of the LCZgroup2 function.

  if (repr == "alter") {
    print(datasetName)
    levColShow <- levCol(sf = sf, column = column, drop = drop, ...)
    typeLevels <- levColShow$levelsColors
    levColCase <- levColShow$case
    ########## Multiple vectors of levels and tryGroup=TRUE, let's try to group on the fly

    if (tryGroup == TRUE &&
      (length(grep("14: ", levColCase)) != 0 || length(grep("15: ", levColCase)) != 0)) {
      message("Level names in your 1st dataset didn't match original data.
      As tryGroup=TRUE, the function groupLCZ will try to create a \"grouped\" column with level names and levels specified in (...).
      If this doesn't work, compareLCZ function may fail.")
      sfNew <- groupLCZ(sf, column = column, ...)
      sf <- sfNew %>% mutate(!!column := subset(sfNew, select = "grouped", drop = TRUE))
      # print(summary(sf1))
      levColShow <- levCol(sf, column, ...)
      typeLevels <- levColShow$levelsColors

      rm(sfNew)
    }

    message(levColCase)

    # IN CASE SOME STANDARD LEVELS ARE DETECTED, ONE MAY WANT STANDARD COLORS TO BE APPLIED

    if (useStandCol == TRUE) { typeLevels <- standLevCol(levels = names(typeLevels), colors = typeLevels, useStandCol = TRUE) }

    LCZlevels <- names(typeLevels)
    sf <- sf %>% mutate(!!column := factor(subset(sf, select = column, drop = T), levels = LCZlevels))
    areas <- LCZareas(sf, column, LCZlevels = names(typeLevels))
    if (!noPerc) { etiquettes <- paste(LCZlevels, ": ", areas$area, "%") }
    else { etiquettes <- LCZlevels }


    if (title == "") {
      if (wf != "") { wtitre <- paste("Grouped LCZ for ", wf, "workflow, applied to ", datasetName, "dataset") } else {
        wtitre <- paste("Grouped LCZ from", datasetName, " dataset")
      }
    } else {
      wtitre <- title
    }

    palter <-
      ggplot(sf) + # les données
        geom_sf(aes(fill = get(column))) +        # Le type de géométrie : ici un sf, avec fill pour remplir les polygones
        scale_fill_manual(values = typeLevels,
                          labels = etiquettes, drop = FALSE) +
        guides(fill = guide_legend(nomLegende)) +
        ggspatial::annotation_north_arrow(
          location = "tl",
          width = unit(0.5, "cm"),
          height = unit(0.5, "cm"),
          pad_x = unit(0.15, "cm"),
          pad_y = unit(0.15, "cm"),
          # data = subset(allLocAllWfs[allLocAllWfs$location == aLocation,], wf == "osm"),
          style = north_arrow_orienteering(
            text_size = 5,
          )) +
        ggspatial::annotation_scale(
          location = "br",
          # data = subset(allLocAllWfs[allLocAllWfs$location == aLocation,], wf == "osm"),
          width_hint = 0.4,
          height = unit(0.1, "cm"),
          pad_x = unit(0.35, "in"),
          pad_y = unit(0.06, "in"),
          text_cex = 0.5,
          text_pad = unit(0.05, "cm"),
        ) +
        ggtitle(wtitre)
  }

  if (repr == "standard") {
    if (plotNow) { print(pstandard) } 
    outplot <- pstandard
  }
  else {
    if (repr == "alter") {
      if (plotNow) { print(palter)}
      outplot <- palter
    }
    else { stop("the repr argument must be \"standard\" or \"alter\" ") }
  }
  return(outplot)
}