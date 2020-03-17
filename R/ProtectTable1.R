#' Easy input interface to sdcTable
#'
#' protectTable or protectLinkedTables is run with a data set at the only required input.
#'
#' @encoding UTF8
#'
#' @param data Matrix or data frame
#' @param dimVarInd Column-indices of the main dimensional variables and additional aggregating variables.
#' @param freqVarInd Column-indices of a variable holding counts or NULL in the case of micro data.
#' @param protectZeros When TRUE empty cells (count=0) is considered sensitive (i.e. same as allowZeros in primarySuppression).
#' @param maxN All cells having counts <= maxN are set as primary suppressed.
#' @param method Parameter "method" in protectTable or protectLinkedTables.
#'               Alternatively a list defining parameters for running tau-argus (see \code{\link{ProtectTable}}).
#' @param findLinked When TRUE, the function may find two linked tables and run protectLinkedTables.
#' @param total String used to name totals.
#' @param addName When TRUE the variable name is added to the level names, except for variables with most levels.
#' @param sep A character string to separate when addName apply.
#' @param removeZeros When TRUE, rows with zero count will be removed from the data.
#' @param dimList See \code{\link{ProtectTable}}.
#' @param groupVarInd Possible manual specification if list defining the hierarchical variable groups
#' @param ind1  Coding of table 1 as indices referring to elements of groupVarInd
#' @param ind2  Coding of table 2 as indices referring to elements of groupVarInd
#' @param dimDataReturn When TRUE a data frame containing the dimVarInd variables is retuned
#' @param IncProgress A function to report progress (incProgress in Shiny).
#' @param ... Further parameters sent to protectTable, protectLinkedTables or createArgusInput.
#'
#' @details One or two tables are identified automatically and subjected to cell suppression methods in package sdcTable.
#'          The tables can alternatively be specified manually by groupVarInd, ind1 and ind2 (see \code{\link{FindTableGroup}}).
#'
#' @return Output is a list of three elements.
#'
#'         \strong{table1} consists of the following elements:
#'         \item{secondary}{Output from \code{\link{protectTable}} or first element of output from \code{\link{protectLinkedTables}} 
#'         or output from \code{\link{runArgusBatchFile}}.}
#'         \item{primary}{Output from \code{\link{primarySuppression}}.}
#'         \item{problem}{Output from \code{\link{makeProblem}}.}
#'         \item{dimList}{Generated input to makeProblem.}
#'         \item{ind}{Indices referring to elements of groupVarInd in the output element common.}
#'
#'         \strong{table2} consists of elements of the same type as table1 in cases of two linked tables. Otherwise  table2 is NULL.
#'
#'         \strong{common} consists of the following elements:
#'         \item{commonCells}{Input to protectLinkedTables.}
#'         \item{groupVarInd}{List defining the hierarchical variable groups}
#'         \item{info}{A table summarizing the tables using variable names}
#'         \item{nLevels}{The number of levels of each variable (only when groupVarInd input is NULL)}
#'         \item{dimData}{Data frame containing the dimVarInd variables when dimDataReturn=TRUE. Otherwise NULL.}
#'
#' @export
#' @importFrom sdcTable makeProblem primarySuppression protectTable protectLinkedTables createArgusInput runArgusBatchFile
#' @importFrom SSBtools FindTableGroup FindDimLists FindCommonCells FactorLevCorr
#'
#' @seealso \code{\link{ProtectTable}}, 
#'         \code{\link{HierarchicalGroups}}, \code{\link{FactorLevCorr}},
#'         \code{\link{FindDimLists}}, \code{\link{FindCommonCells}}
#'
#' @examples
#' \dontrun{
#' z2 <- EasyData("z2")
#' a <- ProtectTable1(z2, c(1, 3, 4), 5)
#' head(as.data.frame(getInfo(a[[1]][[1]], type = "finalData"))  # The table (not two linked))
#' 
#' z3 <- EasyData("z3")
#' b <- ProtectTable1(z3, 1:6, 7)
#' head(as.data.frame(getInfo(b[[1]][[1]], type = "finalData")))  # First table
#' head(as.data.frame(getInfo(b[[2]][[1]], type = "finalData")))  # Second table
#' }
ProtectTable1 <- function(data, dimVarInd = 1:NCOL(data), freqVarInd = NULL, protectZeros = TRUE, 
                          maxN = 3, method = "SIMPLEHEURISTIC", findLinked = TRUE, total = "Total", addName = FALSE, 
                          sep = ".", removeZeros = FALSE, 
                          dimList = NULL,
                          groupVarInd = NULL, ind1 = NULL, ind2 = NULL, 
                          dimDataReturn = FALSE, 
                          IncProgress = IncDefault,
                          ...) {
  tauArgus <- is.list(method)
  makeMicro = FALSE
  
  if(tauArgus){
    exeTauArgus <- method$exe
    method$exe <- NULL
    if(is.null(exeTauArgus)) exeTauArgus  <- formals(runArgusBatchFile)$exe # "C:\\Tau\\TauArgus.exe"
    if(is.null(method$typ))    method$typ <- formals(createArgusInput)$typ  #  "microdata"
    if(!(method$typ %in% c("microdata","tabular")))
      stop('typ must be "microdata" or "tabular"')
    if(method$typ == "microdata") makeMicro = TRUE
  }  
  
  if(maxN>=0) primarySupp <- primarySuppression 
  else primarySupp <- function(...) NULL # Possible to ignore primarySuppression
  
  
  allowZeros <- protectZeros
  methodLinked <- method
  if (removeZeros & !is.null(freqVarInd)) 
    data <- data[data[, freqVarInd] > 0, , drop = FALSE]
  
  if (is.null(groupVarInd)) {
    fCorr <- FactorLevCorr(data[, dimVarInd, drop = FALSE])
    nLevels <- diag(fCorr)
    tableGroup <- FindTableGroup(findLinked = findLinked, fCorr = fCorr, CheckHandling = stop)
    groupVarInd <- tableGroup$groupVarInd
    ind1 <- tableGroup$table$ind1
    if (length(tableGroup$table) > 1) 
      ind2 <- tableGroup$table$ind2 else ind2 <- NULL
  } else {
    if (is.null(ind1)) 
      stop("ind1 is needed when groupVarInd is in input")
    nLevels <- NULL
  }
  
  
  linked <- !is.null(ind2)
  
  dimLists <- FindDimLists(data[, dimVarInd, drop = FALSE], groupVarInd = groupVarInd, 
                           addName = addName, sep = sep, total = total, xReturn = dimDataReturn)
  

  if (dimDataReturn) {
    dimData <- dimLists$x
    dimLists <- dimLists$dimLists
  } else dimData <- NULL
  
  
  if(!is.null(dimList)){
    dimLists <- ReplaceDimList(dimLists, dimList, total = total)
  }
  
  
  dimList1 <- dimLists[ind1]
  
  if(makeMicro){ 
    data <- MakeMicro(data,freqVarInd)
    freqVarInd <- NULL
  }
  
  IncProgress()
  
  
  problem1 <- makeProblem(data = data, dimList = dimList1, dimVarInd = match(names(dimList1), 
                                                                               colnames(data)), freqVarInd = freqVarInd)
  

  
  primary1 <- primarySupp(problem1, type = "freq", maxN = maxN, allowZeros = allowZeros)
  if(get0("doPrintDimLists",ifnotfound = FALSE)){
    print(dimList1)
    flush.console()
  }  
  
  
  
  if (linked) {
    if(tauArgus) stop("tauArgus with linked tables is not implemented")
    dimList2 <- dimLists[ind2]
    problem2 <- makeProblem(data = data, dimList = dimList2, dimVarInd = match(names(dimList2), 
                                                                               colnames(data)), freqVarInd = freqVarInd)
    primary2 <- primarySupp(problem2, type = "freq", maxN = maxN, allowZeros = allowZeros)
    commonCells <- FindCommonCells(dimList1, dimList2)
    IncProgress()
    secondary <- protectLinkedTables(objectA = primary1, objectB = primary2, 
                                     commonCells = commonCells, method = methodLinked, ...)
    if(get0("doPrintDimLists",ifnotfound = FALSE)){
      print(dimList2)
      print(commonCells)
      flush.console()
    }
  } else {
    
    
    ind2 <- NULL
    dimList2 <- NULL
    problem2 <- NULL
    primary2 <- NULL
    commonCells <- NULL
    IncProgress()
    if(!tauArgus){
      
      secondary <- list(protectTable(object = primary1, method = method, ...), NULL)
      
    } else {  
      ## tauArgus start here
      if(method$typ == "microdata"){
        batchF <- eval(as.call(c(as.name("createArgusInput"),obj=as.name("problem1"),method, ...)))
        if(get0("waitForAKeyPress",ifnotfound = FALSE)) invisible(readline(prompt="Press [enter] to continue"))
        secondary <- list(runArgusBatchFile(obj=problem1, batchF = batchF, exe = exeTauArgus), NULL)
      }
      else{  # Same as above with primary1 instead of problem1
        batchF <- eval(as.call(c(as.name("createArgusInput"),obj=as.name("primary1"),method, ...)))
        if(get0("waitForAKeyPress",ifnotfound = FALSE)) invisible(readline(prompt="Press [enter] to continue"))
        secondary <- list(runArgusBatchFile(obj=primary1, batchF = batchF, exe = exeTauArgus), NULL)
      }
    }
  }
  
  
  x <- groupVarInd
  for (i in 1:length(x)) x[[i]] <- paste((colnames(data)[dimVarInd])[x[[i]]], collapse = ", ")
  x <- cbind(as.data.frame(as.character(unlist(x))), 0, 0)
  colnames(x) <- c("Variables", "Table1", "Table2")
  x[ind1, 2] <- 1
  if (linked) 
    x[ind2, 3] <- 1
  
  return(list(table1 = list(secondary = secondary[[1]], primary = primary1, problem = problem1, 
                            dimList = dimList1, ind = ind1), table2 = list(secondary = secondary[[2]], 
                                                                           primary = primary2, problem = problem2, dimList = dimList2, ind = ind2), 
              common = list(commonCells = commonCells, groupVarInd = groupVarInd, info = x, 
                            nLevels = nLevels, dimData = dimData)))
}

# Make micro data from data with freq. 
MakeMicro <- function(x,freqInd){
  rows <- rep(seq_len(NROW(x)),x[,freqInd])
  x <- x[rows, ,drop=FALSE]
  x[,freqInd] <- 1
  row.names(x) <- NULL
  x
}
