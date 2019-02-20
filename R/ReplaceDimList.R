
#' ReplaceDimList
#' 
#' Replace list elements of sdcTable coded hierarchies.
#' Replacement elements can be sdcTable coded or TauArgus coded.
#'
#' @param dimList Named list of data frames (sdcTable coded)
#' @param replaceList Named list where elements are data frames (sdcTable coded) or character vectors (TauArgus coded)
#' @param total String used to name totals when TauArgus coded input
#'
#' @return Updated dimList where some or all elements are replaced 
#' @importFrom SSBtools Hrc2DimList
#' @export
#' @keywords internal
#'
#' @examples
#' # First generate dimLists
#' dimListA <- FindDimLists(SSBtoolsData("sprt_emp_withEU")[, c("geo", "eu", "age")])
#' dimListB <- FindDimLists(SSBtoolsData("sprt_emp_withEU")[, c("geo", "age")])
#' dimListC <- FindDimLists(SSBtoolsData("sprt_emp_withEU")[, c("geo", "eu")])
#' 
#' # dimListA1 and dimListA are identical
#' dimListA1 <- ReplaceDimList(dimListB, dimListC)
#' identical(dimListA, dimListA1)
#' 
#' # replaceList can be TauArgus coded
#' hcrC <- DimList2Hrc(dimListC)
#' 
#' # dimListA2 and dimListA are identical
#' dimListA2 <- ReplaceDimList(dimListB, hcrC)
#' identical(dimListA, dimListA2)
#' 
#' # Also possible when duplicated names
#' ReplaceDimList(FindDimLists(EasyData("z3")[, -7]), 
#'                FindDimLists(EasyData("z2")[, -5]))
ReplaceDimList <- function(dimList, replaceList, total = "Total") {
  for (i in seq_along(replaceList)) {
    if (is.character(replaceList[[i]])) 
      replaceList[[i]] <- Hrc2DimList(replaceList[[i]], total = total)
    else
      replaceList[[i]] <- FixDimListNames(replaceList[[i]])
  }
  names1 <- make.names(names(dimList), unique = TRUE)
  names2 <- make.names(names(replaceList), unique = TRUE)
  matchNames <- match(names1, names2)
  dimList[!is.na(matchNames)] <- replaceList[matchNames[!is.na(matchNames)]]
  dimList
}


# Same as in SSBtools
FixDimListNames <- function(x) {
  if (!any(!(c("levels", "codes") %in% names(x)))) 
    return(x)
  a <- unique(c(pmatch(c("lev", "cod", "nam"), names(x)), 1:2))
  a <- a[!is.na(a)][1:2]
  names(x)[a] <- c("levels", "codes")
  x
}





