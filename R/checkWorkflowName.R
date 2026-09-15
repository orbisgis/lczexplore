#' Checks if a character string can be used as a workflow name and corrects it if it contains spaces or underscores
#'
#' @param wfName contains the character string to
#' @return the character string if suitable, or a modified one to comply to what is expected for a workflow name
#' @export
#'
#' @examples
#' checkWorkflowName("BDT UTRF_test")
checkWorkflowName <- function(wfName){
  if (grepl("_", x =wfName) | grepl("\\s", x = wfName)){
    message("For Technical reason in this package, worfklow names can't contain spaces or underscores, they were replaced by dots")
  wfName<- gsub("_|\\s", ".", wfName)
  }

  if (nchar(wfName) > 10){
    message("Workflow names are used in produced graphics and as column names, long names are not suitable,
    the first 3 and last 3 letters were kept instead")
    wfName<-paste0(
      substr(wfName, 1,3),
      ".",
    substring(wfName, nchar(wfName)-3, nchar(wfName)))
    wfName<-gsub("\\.+", ".", wfName)
    }

  return(wfName)
}