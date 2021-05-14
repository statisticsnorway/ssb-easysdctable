#' Function that returns a dataset 
#'
#' @encoding UTF8
#'
#' @param dataset Name of data set within the easySdcTable  package  
#' @param path When non-NULL the data set is read from "path/dataset.RData"
#'
#' @return The dataset
#' @export
#' @importFrom utils data
#' @importFrom SSBtools Hrc2DimList SSBtoolsData
#' 
#' @note The function returns the same datasets as \code{\link{SSBtoolsData}}.
#'
#' @examples
#'  z  <- EasyData("sosialFiktiv")
#'
EasyData <- function(dataset, path = NULL) {
  if (!is.null(path)) {
    filename <- paste(path, dataset, ".RData", sep = "")
    return(get(load(filename, envir = environment())))
  }
  SSBtoolsData(dataset)
}
