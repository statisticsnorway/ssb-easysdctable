#' ProtectTable with output ready for SuppressDec in package RegSDC
#' 
#' Assuming correct suppression, suppressed values become decimal numbers (not whole numbers) instead of missing.
#' 
#' Within this r package this function will be used for testing
#' 
#'
#' @param data data frame
#' @param dimVar The main dimensional variables and additional aggregating variables (name or number).
#' @param freqVar Variable(s) holding counts (name or number).
#' @param ... Further parameters sent to \code{\link{ProtectTable}} 
#'
#' @return List of three matrices ready as input to \code{SuppressDec}
#' \item{x}{Sparse dummy matrix where the dimensions match z and y.}
#' \item{z}{Frequencies to be published with suppressed as NA.}
#' \item{y}{Inner cell frequencies.}
#'  
#' @importFrom SSBtools AutoHierarchies FactorLevCorr FindTableGroup FindDimLists HierarchyComputeDummy
#' @export
#' @author Øyvind Langsrud
#'
#' @examples
#' 
#' # Same examples as in ProtectTable 
#' a1 <- PTxyz(EasyData("z1"), c("region","hovedint") ,"ant")
#' a2 <- PTxyz(EasyData("z2"), c(1,3,4),5) 
#'  
#' if (require(RegSDC)) { # RegSDCdata and SuppressDec
#'   # Same data as in RegSDCdata examples (and in paper)
#'   data7 <- RegSDCdata("sec7data")
#'   data7 <- data7[!is.na(data7$y), 1:3]
#'   data7
#' 
#'   # Generate x, y, z similar to xAll, y, zAllSupp in RegSDCdata examples 
#'   # But different suppressed cells and z ordered differently
#'   a <- PTxyz(data7, 1:2, 3, maxN = 3, method = "HITAS")
#'   a
#' 
#'   # Suppressed inner cells as decimal numbers
#'   yDec <- SuppressDec(a$x, a$z, a$y, rmse = 1)
#'   yDec
#' 
#'   # All cells as decimal numbers
#'   cbind(a$z, t(a$x) %*% cbind(a$y, yDec))
#' 
#'   # As ProtectTable example
#'   z1 <- EasyData("z1")
#'   a <- PTxyz(z1, c("region", "hovedint"), "ant")
#' 
#'   # Inner cells as decimal numbers. 3 replicates.
#'   yDec <- SuppressDec(a$x, a$z, a$y, nRep = 3)
#'   yDec
#' 
#'   # All cells with 3 replicates.
#'   cbind(a$z, t(a$x) %*% cbind(a$y, yDec))
#' 
#'   # An example involving two linked tables.  
#'   # It is demonstrated that the approach to suppression is not safe.  
#'   # That is, perfect fit (whole numbers) for some suppressed cells.
#'   a <- PTxyz(EasyData("z2"), 1:4, 5)
#'   cbind(a$z, t(a$x) %*% cbind(a$y, SuppressDec(a$x, a$z, rmse = 1, nRep = 3)))[which(is.na(a$z)), ]
#' }
PTxyz = function(data, dimVar, freqVar,...){
  
  systemTime <- hasArg("systemTime") 
  
  if(length(freqVar)!=1)
    stop("Only a single freVar allowed in this implementation")
  
  # Generate dimList as in ProtectTable. So this is done twice in this implementation.
  dimLists = ProtectTable1dimList(data, dimVar, freqVar,...)
  
  if(systemTime){
    system_time <-  system.time({pt <- ProtectTable(data, dimVar, freqVar, ...)})
  } else {
    pt = ProtectTable(data, dimVar, freqVar, ...) 
  }
  
  freqVar = names(data[1, freqVar, drop=FALSE])
  
  varNames =  unique(names(dimLists))
  ptA = pt$data[,!(names(pt$data) %in% c("freq", "sdcStatus", "suppressed")), drop = FALSE]
  
  x = CrossTable2ModelMatrix(data, ptA, dimLists)
  
  rownames(x) = apply(data[, names(data) %in% names(ptA), drop = FALSE], 1,paste,collapse = "_" )
  colnames(x) = apply(ptA, 1,paste,collapse = ":" )
  y = as.matrix(data[,freqVar, drop=FALSE])
  z = as.matrix(pt$data[,"suppressed", drop=FALSE])
  rownames(z) = colnames(x)
  rownames(y) = rownames(x)
  
  if(systemTime){
    return(list(x = x, y=y, z=z, system_time = system_time))
  }
  list(x = x, y=y, z=z)
}


CrossTable2ModelMatrix <- function(data, crossTable, hierarchies = NULL, total = "Total", 
                                   hierarchyVarNames = c(mapsFrom = "mapsFrom", mapsTo = "mapsTo", sign = "sign", level = "level"), 
                                   unionComplement = FALSE) {
  cNames <- colnames(crossTable)
  if (!is.null(hierarchies)) 
    cNames <- cNames[!(cNames %in% names(hierarchies))]
  ncNames <- length(cNames)
  if (ncNames > 0) {
    lNames <- as.list(rep(total, ncNames))
    names(lNames) <- cNames
    hierarchies <- c(hierarchies, lNames)
  }
  
  Hierarchies2ModelMatrixNew(data = data, hierarchies = hierarchies, total = total, hierarchyVarNames = hierarchyVarNames, 
                             unionComplement = unionComplement, inputCrossTable = crossTable)
}

# Copy of part of ProtectTable1 used to generate dimList
ProtectTable1dimList <- function(data, dimVarInd = 1:NCOL(data), freqVarInd = NULL, findLinked = TRUE, total = "Total", 
                                 addName = FALSE, sep = ".", dimList = NULL, groupVarInd = NULL, 
                                 dimDataReturn = FALSE, ...) {
  
  if (is.null(groupVarInd)) {
    fCorr <- FactorLevCorr(data[, dimVarInd, drop = FALSE])
    nLevels <- diag(fCorr)
    tableGroup <- FindTableGroup(findLinked = findLinked, fCorr = fCorr, CheckHandling = stop)
    groupVarInd <- tableGroup$groupVarInd
  }
  
  dimLists <- FindDimLists(data[, dimVarInd, drop = FALSE], groupVarInd = groupVarInd, addName = addName, 
                           sep = sep, total = total, xReturn = dimDataReturn)
  
  if (!is.null(dimList)) {
    dimLists <- ReplaceDimList(dimLists, dimList, total = total)
  }
  dimLists
}


# Hierarchies2ModelMatrix with inputCrossTable as new parameter
Hierarchies2ModelMatrixNew <- function(data, hierarchies, inputInOutput = TRUE, crossTable = FALSE, total = "Total", 
                                       hierarchyVarNames = c(mapsFrom = "mapsFrom", mapsTo = "mapsTo", sign = "sign", level = "level"), 
                                       unionComplement = FALSE, inputCrossTable = NULL) {
  autoHierarchies <- AutoHierarchies(hierarchies = hierarchies, data = data, total = total, hierarchyVarNames = hierarchyVarNames)
  HierarchyComputeDummy(data = data, hierarchies = autoHierarchies, inputInOutput = inputInOutput, crossTable = crossTable, 
                        unionComplement = unionComplement, rowSelect = inputCrossTable)
}







if (FALSE) {
  # To obtain exact same suppression as in paper
  exeArgus <- "C:/TauArgus4.1.4/TauArgus.exe"
  pathArgus <- "C:/Users/oyl/Documents/back/tull"
  a <- PTxyz(data7, 1:2, 3, maxN = -1, 
             method = list(path = pathArgus, exe = exeArgus, method = "OPT", primSuppRules = list(list(type = "freq", n = 4, rg = 20))))
  ma <- match(colnames(RegSDCdata("sec7xAll")), colnames(a$x))
  all.equal(a$y[, 1], RegSDCdata("sec7y")[, 1])  # TRUE
  max(abs(a$x[, ma] - RegSDCdata("sec7xAll")))  # 0
  all.equal(a$z[ma, 1], RegSDCdata("sec7zAllSupp")[, 1])  # TRUE
}





