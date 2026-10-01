#' Computes the area of each level of a LCZ
#'
#' @param sf is the sf dataset containing the LCZ
#' @param column allows to specify in which column are the LCZ
#' @param LCZlevels is the vector of expected levels in column
#' @import sf
#' @importFrom dplyr mutate group_by_at summarize ungroup
#' @return The percentage of the total area covered by each LCZ
#' @export
#'
#' @examples
#' #LCZareas is not to be used directly by user.
LCZareas <- function(sf, column, LCZlevels) {
  # Creation of a colum with geometry area

  sf <- tryCatch({
    dplyr::mutate(sf, area = st_area(geometry)) %>% drop_units
  },
    error = function(...) {
      message("Some geometries don't seem valid, the function will try to make them valid, it may take a bit longer.")
      sf %>%
        st_make_valid %>%
        dplyr::mutate(area = st_area(geometry)) %>%
        drop_units
    }

  )


  # area by LCZ LCZ
  areaLCZ <- sf %>%
    st_drop_geometry %>%
    dplyr::group_by_at(.vars = column) %>%
    dplyr::summarize(area = sum(area, na.rm = T)) %>%
    drop_units %>%
    ungroup()
  areaLCZ$area <- round(areaLCZ$area / sum(areaLCZ$area, na.rm = T) * 100, digits = 2)
  areaLCZ

  areas <- data.frame(LCZlevels = LCZlevels, area = 0)
  for (i in subset(areaLCZ, select = column, drop = TRUE)) {
    if (!is.na(i)) {
      areas[areas$LCZlevels == i, 'area'] <- areaLCZ[subset(areaLCZ, select = column, drop = TRUE) == i, 'area']
    }
  }

  areas
}