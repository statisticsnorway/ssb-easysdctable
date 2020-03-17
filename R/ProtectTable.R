#'  Easy interface to sdcTable: Table suppression according to a frequency rule. 
#'
#'  protectTable() or protectLinkedTables() in package 'sdcTable' is run with a data set as the only required input.
#'  One (stacked) or several (unstacked) input variables can hold cell counts.
#'  Output is on a form similar to input.
#'
#' @encoding UTF8
#' 
#' @param data data frame
#' @param dimVar The main dimensional variables and additional aggregating variables (name or number).
#' @param freqVar Variable(s) holding counts or NULL in the case of micro data (name or number).
#' @param protectZeros When TRUE empty cells (count=0) is considered sensitive (i.e. same as allowZeros in  \code{\link{primarySuppression}}).
#' @param maxN All cells having counts <= maxN are set as primary suppressed.
#' @param method Parameter "method" in \code{\link{protectTable}} or \code{\link{protectLinkedTables}}.
#'        Default is "SIMPLEHEURISTIC". Other allowed values are
#'        "OPT", "HITAS" and "HYPERCUBE". The latter is not possible in cases with two linked tables.
#'        Alternatively this parameter can be a named list specifying parameters for running tau-argus (see details).
#'        Experimental wrapper methods according to \code{\link{PTwrap}} is also possible (see details).
#' @param findLinked When TRUE, the function may find two linked tables and run protectLinkedTables.
#' @param total String used to name totals.
#' @param addName When TRUE the variable name is added to the level names, except for variables with most levels.
#' @param sep A character string to separate when addName apply and when creating variable names.
#' @param removeZeros When TRUE, rows with zero count will be removed from the data within the algorithm.
#' @param dimList By default, hierarchies will be automatically found from data (see \code{\link{FindDimLists}}). 
#'   With non-NULL dimList, these will be changed. 
#'   In practice this is done by the function \code{\link{ReplaceDimList}}. 
#' @param groupVarInd Possible manual specification of list defining the hierarchical 
#'         variable groups. When NULL (default) this information will be found automatically 
#'         by \code{\link{FindTableGroup}}.
#' @param ind1  Coding of table 1 as indices referring to elements of groupVarInd. This information 
#'         will be found automatically 
#'         by \code{\link{FindTableGroup}} when groupVarInd=NULL. 
#' @param ind2  Coding of table 2 as indices referring to elements of groupVarInd (as ind1 above).
#' @param rowData Input to \code{\link{Stack}} used to generate extra dimVar variables when stacking in cases with several 
#'        freqvar variables. When NULL rowData will be created automatically by \code{\link{AutoSplit}} using varNames (see below)
#'        and the the freqvar names as input.
#' @param varNames The names of the extra dimVar variable(s) made when stacking in cases with several 
#'        freqvar variables. When length(varNames)>1 several variables may be found by \code{\link{AutoSplit}}. 
#' @param split Parameter to \code{\link{AutoSplit}} - see varNames and rowData above.  
#'        When NULL (default), automatic splitting without needing a split string.
#' @param border Parameter to \code{\link{AutoSplit}} - see varNames and rowData above.
#' @param revBorder Parameter to \code{\link{AutoSplit}} - see varNames and rowData above..
#' @param freqName Input to \code{\link{Stack}}. The name of the new freqvar variable obtained when stacking in cases with several 
#'        input freqvar variables.
#' @param totalFirst Parameter controlling how output is sorted.
#' @param numericOrder Parameter controlling how output is sorted. 
#'        Output is character but sorting can be based on the numeric input variables.
#' @param namesAsInput When TRUE those output variables (created by unstacking) that correspond to input will be named as input. 
#' @param orderAsInput When TRUE output corresponding to input will be ordered as input and kept together as one block. 
#' @param sortByReversedColumns When TRUE output will be sorted by variables in opposite order. 
#' @param doUnstack When FALSE output will not be unstacked (in cases with sever input freqvar variables)
#' @param removeTotal When TRUE the total string (see total above) will be removed from the names of output variables 
#'        created by unstacking (in cases with sever input freqvar variables). 
#' @param singleOutput When TRUE output will be in as single data set. Default is FALSE for unstacked data  
#'        (in cases with sever input freqvar variables) and otherwise TRUE.
#' @param suppression Value used for suppressed elements in suppressed output data. Default is NA.
#' @param outFreq String used to name output variable(s)
#' @param outSdcStatus String used to name output variable(s)
#' @param outSuppressed String used to name output variable(s)
#' @param infoAsFrame When TRUE output element info is a data frame (useful in Shiny).
#' @param IncProgress A function to report progress (incProgress in Shiny).
#' @param ...  Further parameters sent to \code{\link{protectTable}} (possibly via \code{\link{protectLinkedTables}})
#'        such as verbose (print output while calculating) and timeLimit. 
#'        Parameters to \code{\link{createArgusInput}} and \code{\link{PTwrap}} is also possible (see details).
#' 
#' @details One or two tables are identified automatically and subjected to cell suppression 
#'          by \code{\link{protectTable}} (single table) or \code{\link{protectLinkedTables}} (two linked tables).
#'          The tables can alternatively be specified manually by groupVarInd, ind1 and ind2.
#'          The output will be on a form similiar to input depending on whether freqVar is a single variable or not.
#'          The status of the cells are 
#'          coded as  "u" (primary suppressed), "x" (secondary suppression), and "s" (can be published).
#'          This is taken directly from the output from sdcTable. In cases with two linked tables "u" or "x" 
#'          for common cells are based on output from the first table.
#'          
#'          \strong{To run tau-argus} specify "method" as a named list containing the
#'          parameter "exe" for \code{\link{runArgusBatchFile}} and other parameters for 
#'          \code{\link{createArgusInput}}.  
#'          
#'          One may specify:  
#'          \code{method = list(exe="C:/Tau/TauArgus.exe", typ="tabular", path= getwd(),} 
#'          \code{solver= "FREE", method= "OPT")}
#'           
#'          However these values of "exe", "path" and "solver" and "method" are set by default so in this case 
#'          using "\code{method = list(typ="tabular", method= "OPT")}" is equivalent.
#'          
#'          If \code{typ="microdata"} is specified. Necessary transformation to microdata will be made. 
#'          
#'          \strong{Wrapper methods (experimental):}
#'          In the function \code{\link{PTwrap}} several additional methods are defined. 
#'          If input to ProtectTable() is one of these methods ProtectTable() will 
#'          be run via PTwrap(). So making explicit call to PTwrap() is not needed.
#'               
#'          
#'          NOTE: The use of numVarInd, weightInd and sampWeightInd in sdcTable is not implemented. This also limit possible 
#'          input to  tau-argus.
#'
#' @return When singleOutput=TRUE output is a list of two elements.
#' 
#'         \item{info}{Information as a single column character matrix. This is information about the extra 
#'                     dimVar variables created when stacking, information about the identified (linked) 
#'                     table(s) and summary output from sdcTable.}
#'         \item{data}{A data frame where variables are named according to outFreq, 
#'                     outSdcStatus and outSuppressed.}
#'         When singleOutput=FALSE output element data is replaced by three elements and these are named  
#'         according to outFreq, outSdcStatus and outSuppressed.
#'         
#' @export
#' @importFrom sdcTable summary getInfo
#' @importFrom SSBtools AutoSplit Stack SortRows Unstack
#' @importFrom utils capture.output flush.console
#' 
#' @seealso ProtectTable makes a call to the function \code{\link{ProtectTable1}}.
#'
#'
#' @examples
#'  # ==== Example 1 , 8 regions ====
#'  z1 <- EasyData("z1")        
#'  ProtectTable(z1,1:2,3)
#'  ProtectTable(z1,c("region","hovedint") ,"ant") # Input by name 
#'  # --- Unstacked input data ---
#'  z1w = EasyData("z1w") 
#'  ProtectTable(z1w,1,2:5)
#'  ProtectTable(z1w,1,2:5,varName="hovedint") 
#'  ProtectTable(z1w,1,2:5,method="HITAS")
#'  ProtectTable(z1w,1,2:5,totalFirst = TRUE)
#'  
#'  # ==== Example 2 , 11 regions ====
#'  z2 <- EasyData("z2") 
#'  ProtectTable(z2,c(1,3,4),5) # With region-variable kostragr
#'  # --- Unstacked input data ---
#'  z2w <- EasyData("z2w") 
#'  ProtectTable(z2w,1:2,4:7) # With region-variable fylke
#'  ProtectTable(z2w,1:3,4:7) # Two linked tables
#'  
#'  \dontrun{
#'  # ==== Example 3 , 36 regions ====
#'  z3 <- EasyData("z3")   
#'  ProtectTable(z3,c(1,4,5),7) # Three dimensions. No subtotals    
#'  ProtectTable(z3,1:6,7)      # Two linked tables  
#'  # --- Unstacked input data with coded column names 
#'  z3w <- EasyData("z3w")
#'  ProtectTable(z3w,1:3,4:15,varName="g12") # coding not used when single varName
#'  ProtectTable(z3w,1:3,4:15,varName=c("hovedint","mnd"))  # Two variables found automatically 
#'  ProtectTable(z3w,1:3,4:15,varName=c("hovedint","mnd"),
#'                removeTotal=FALSE) # Keep "Total" in variable names 
#'  # --- Unstacked input data with three level column name coding  
#'  ProtectTable(z3wb,1:3,4:15,varName=c("hovedint","mnd","mnd2")) # Two variables found automatically
#'  ProtectTable(z3wb,1:3,4:15,varName=c("hovedint","mnd","mnd2"), 
#'                split="_")  # Three variables when splitting
#'  ProtectTable(z3wb,1:3,4:15,varName=c("hovedint","mnd","mnd2"), 
#'                split="_",namesAsInput=FALSE,orderAsInput=FALSE) # Alternative ouput format
#'                
#'  # ====  Examples Tau-Argus ====              
#'  exeArgus <- "C:/TauArgus4.1.4/TauArgus.exe" # Change to TauArgus.exe-path in your computer
#'  pathArgus <- "C:/Users/nnn/Documents"       # Change to an existing folder 
#'  z1 = EasyData("z1") 
#'  ProtectTable(z1,1:2,3,method=list(exe=exeArgus, path=pathArgus, typ="tabular", method="OPT")) 
#'  ProtectTable(z1,1:2,3,method=list(exe=exeArgus, path=pathArgus, typ="tabular", method="MOD")) 
#'  ProtectTable(z1,1:2,3,method=list(exe=exeArgus, path=pathArgus, typ="tabular", method="GH"))
#'   ProtectTable(z1,1:2,3,maxN=-1,
#'    method=list(path=pathArgus, exe=exeArgus, method="OPT",
#'          primSuppRules= list(list(type="freq", n=4, rg=20))))
#'  z3w <- EasyData("z3")
#'  ProtectTable(z3,c(1:2,4,5),7,maxN=-1,
#'    method=list(path=pathArgus, exe=exeArgus, method="OPT",
#'          primSuppRules=list(list(type="freq", n=4, rg=20))))
#'                }
#'                
#' # ==== Examples with parameter dimList  ====
#' z2 <- EasyData("z2")
#' dList <- FindDimLists(z2[-5])
#' ProtectTable(z2[, c(1, 4, 5)], 1:2, 3, dimList = dList[c(1, 3)])
#' ProtectTable(z2[, c(1, 4, 5)], 1:2, 3, dimList = dList[2])
#' ProtectTable(z2[, c(1, 4, 5)], 1:2, 3, dimList = DimList2Hrc(dList[c(2, 3)]))              
ProtectTable  <-  function(data,
                         dimVar=1:NCOL(data),
                         freqVar=NULL,
                         protectZeros=TRUE,
                         maxN=3,
                         method="SIMPLEHEURISTIC",
                         findLinked=TRUE,
                         total="Total",
                         addName=FALSE,
                         sep="_",
                         removeZeros=FALSE,
                         dimList = NULL,
                         groupVarInd=NULL,
                         ind1=NULL,
                         ind2=NULL, 
                         rowData=NULL,
                         varNames=paste("var",1:100,sep=""),
                         split=NULL,
                         border=sep,
                         revBorder=FALSE, 
                         freqName="values",
                         totalFirst=FALSE,
                         numericOrder=TRUE,
                         namesAsInput=TRUE,
                         orderAsInput=TRUE,
                         sortByReversedColumns=FALSE, 
                         doUnstack=TRUE,
                         removeTotal=TRUE,
                         singleOutput=NULL,   # eller TRUE/FALSE
                         suppression=NA,
                         outFreq="freq",
                         outSdcStatus="sdcStatus",
                         outSuppressed="suppressed",
                         infoAsFrame = FALSE,
                         IncProgress = IncDefault,
                         ...) {
  IncProgress()
  tauArgus <- is.list(method)
  if(!tauArgus) 
    if(method %in% c("Simple","SimpleSingle","TauArgus","TauArgusOPT","TauArgusMOD","TauArgusGH")){
      sysCall <- sys.call()
      sysCall[[1]] <- as.name("PTwrap")
      parentFrame = parent.frame()
      return(eval(sysCall, envir=parentFrame))
  }
  
  if (is.character(dimVar)) 
    dimVarInd <- match(dimVar, names(data)) else dimVarInd <- dimVar
    if (is.character(freqVar)) 
      freqVarInd <- match(freqVar, names(data)) else freqVarInd <- freqVar
      
      tryAutoSplit <- (length(varNames) > 1)
      
      
      
      stacked <- FALSE
      if (length(freqVarInd) > 1) {
        stacked <- TRUE
        if (orderAsInput & !namesAsInput) 
          stop("orderAsInput=TRUE combined with namesAsInput=FALSE is not implemented")
        if (orderAsInput & !doUnstack) 
          warning("orderAsInput=TRUE ignored when doUnstack=FALSE")
        stackVar <- freqVarInd
        dataOrig <- data
        if (is.null(rowData)) 
          rowData <- AutoSplit(colnames(data)[freqVarInd], split = split, border = border, 
                               revBorder = revBorder, noSplit = !tryAutoSplit, varNames = varNames) else rownames(rowData) <- colnames(data)[freqVarInd]
        
        varNames <- colnames(rowData)
        data <- Stack(dataOrig, stackVar = freqVarInd, blockVar = dimVarInd, rowData = rowData, 
                      valueName = freqName, indName = NULL)
        colnamesOrig <- colnames(dataOrig)
        dimVarNamesOrig <- colnamesOrig[dimVarInd]
        dimVarNames <- c(dimVarNamesOrig, varNames)
        dimVarInd <- match(dimVarNames, colnames(data))  ######### New dimVarInd refer to stacked data
        freqVarInd <- match(freqName, colnames(data))  ######### New freqVarInd refer to stacked data
        
      }
      
      IncProgress()
      pt <- ProtectTable1(data = data, dimVarInd = dimVarInd, freqVarInd = freqVarInd, 
                          protectZeros = protectZeros, maxN = maxN, method = method, findLinked = findLinked,
                          total = total, addName = addName, sep = sep, removeZeros = removeZeros, 
                          dimList = dimList,
                          groupVarInd = groupVarInd, 
                          ind1 = ind1, ind2 = ind2, dimDataReturn = TRUE, IncProgress = IncProgress, ...)
      
      if(infoAsFrame){
          i00 <- as.data.frame(rbind(
                 ## c("method",method),
                 c("protectZeros",protectZeros),
                 c("maxN",maxN)
          ),stringsAsFactors=FALSE)
          if(!tauArgus) names(i00) <- c("method",method)   #c("Parameter","Choice")
          else names(i00) <- c("method","TauArgus")
          if (stacked) 
          i0 <- data.frame(InputName=rownames(rowData),as.data.frame(as.matrix(rowData),stringsAsFactors=FALSE),stringsAsFactors=FALSE) else i0 <- NULL
          i1 <- as.data.frame(as.matrix(pt$common$info),stringsAsFactors=FALSE)
          if(!tauArgus){
            i2 <- as.data.frame(capture.output(sdcTable::summary(pt$table1[[1]])),stringsAsFactors=FALSE)
            names(i2) = "Summary1sdcTable"            
          } else {
            i2 <- as.data.frame(capture.output(print(method)),stringsAsFactors=FALSE)
            names(i2) = "TauArgus"            
          }   # i2 = NULL  
          if (!is.null(pt$table2[[1]])) {
            i3 <- as.data.frame(capture.output(sdcTable::summary(pt$table2[[1]])),stringsAsFactors=FALSE)
            names(i3) = "Summary2sdcTable"
          } else i3 <- NULL
        info <- RbindAllwithNames(i00,i0,i1,i2,i3,toRight=TRUE,extra="= = =")
        colnames(info)[1] <- "Info"
      } else {
        if (stacked) 
          i0 <- capture.output(print(rowData)) else i0 <- NULL
      
        i1 <- capture.output(print(pt$common$info))
        if(!tauArgus){ 
          i2 <- capture.output(sdcTable::summary(pt$table1[[1]])) ## Wrong in html Vignette without "sdcTable::"
        } else 
          i2 <- capture.output(print(method)) ## Wrong in html Vignette without "sdcTable::"
          #i2 = NULL  
        if (!is.null(pt$table2[[1]])) 
          i3 <- capture.output(sdcTable::summary(pt$table2[[1]])) else i3 <- NULL
      
        info <- c(i0, "==========", i1, "==========", i2, "==========", i3)
        info <- as.matrix(info, ncol = 1)  # One element pr row when printed
      }
      
      gVC <- GroupVarCombined(pt$common$groupVarInd, totalFirst)
      
      nDim <- length(gVC)
      
      try( {    # Include in try as extra safety. Sorting can be omitted"
        sortedLists <- vector("list", nDim)
        names(sortedLists) <- names(gVC)
        for (i in seq_len(nDim)) {
          if(is.null(dimList)){
            if (numericOrder) 
              sortedLists[[i]] <- SortedFromData(pt$common$dimData, ind = gVC[[i]], 
                                                 total = total, xNumeric = data[, dimVarInd, drop = FALSE]) 
            else 
              sortedLists[[i]] <- SortedFromData(pt$common$dimData, ind = gVC[[i]], total = total)
          } else{
            sortedLists[[i]] <- SortedFromDimList(pt$table1$dimList[names(gVC)[i]][[1]],pt$table2$dimList[names(gVC)[i]][[1]])
          }
        }
      }, silent = TRUE)
      
      
      if (is.null(pt[[2]][[1]])) {
        if(!tauArgus){ 
          finalData <- as.data.frame(getInfo(pt[[1]][[1]], type = "finalData"))
        } else{
          finalData <- as.data.frame(pt[[1]][[1]])  ## Start treating tauArgus
          names(finalData)[names(finalData)=="freq"] <- "Freq"
          if(!is.null(finalData$cellvalue)){
            if(is.null(finalData$Freq)) finalData$Freq  <- finalData$cellvalue
            finalData$cellvalue <- NULL
          }
          if(!is.null(finalData$sdcStatus_argus)){
            finalData$sdcStatus  <- finalData$sdcStatus_argus
            finalData$sdcStatus_argus <- NULL
          }
        }                                          ## End treating tauArgus
      } else {
        t1 <- as.data.frame(getInfo(pt[[1]][[1]], type = "finalData"))
        t2 <- as.data.frame(getInfo(pt[[2]][[1]], type = "finalData"))
        if (dim(t1)[2] != dim(t2)[2]) 
          stop("Output from linked tables: Something is wrong!")
        
        b <- merge(t1, t2, all = TRUE, by = seq_len(dim(t1)[2] - 2), suffixes = c("", ".y"))
        ######## MERK
        if (sum(abs(b$Freq - b$Freq.y), na.rm = TRUE) > 0) 
          stop("Output from protectLinkedTables: Something is wrong!")
        
        if (sum(abs(as.integer(b$sdcStatus == "s") - as.integer(b$sdcStatus.y == 
                                                                "s")), na.rm = TRUE) > 0) {
          b$sdcStatus[!is.na(b$sdcStatus) & b$sdcStatus.y == "s"] <- "s"
          warning("Non-unique suppression-output form protectLinkedTables")
        }
        
        if (sum(!(is.na(b$Freq) == is.na(b$sdcStatus))) > 0) 
          stop("Output from protectLinkedTables: Something is wrong!")
        
        nat1 <- is.na(b$Freq)
        b$sdcStatus[nat1] <- b$sdcStatus.y[nat1]
        b$Freq[nat1] <- b$Freq.y[nat1]
        
        finalData <- b[, !(colnames(b) %in% c("Freq.y", "sdcStatus.y")), drop = FALSE]
        
      }
      
      
      okSortTry = FALSE
      try( {    # Include in try as extra safety. Sorting can be omitted"
        
        if (sortByReversedColumns) 
          fd <- finalData[, rev(seq_len(nDim)), drop = FALSE] else fd <- finalData[, seq_len(nDim), drop = FALSE]
        for (i in seq_len(nDim)) {
          fd[, names(sortedLists)[i]] <- as.integer(factor(fd[, names(sortedLists)[i]], 
                                                         levels = sortedLists[[i]]))
        }
      
        if (sum(is.na(fd))) 
          stop("Something went wrong when sorting output")
      
      
        finalData <- finalData[SortRows(fd, index.return = TRUE), , drop = FALSE]
        okSortTry = TRUE
      }, silent = TRUE)
      
      if(!okSortTry)
        warning("Something went wrong when sorting output. Output is not sorted.")
      
      rownames(finalData) <- NULL
      
      suppressed <- finalData$Freq
      
      if(protectZeros)
        suppressed[!finalData$sdcStatus == "s"] <- suppression
      else
        suppressed[!(finalData$sdcStatus == "s" | finalData$sdcStatus == "z") ] <- suppression
      
      
      finalData$supp6547524 <- suppressed
      
      IncProgress()
      cat("\n")
      
      attributes(finalData)$index <- NULL  # avoid attribute
      
      if(get0("doReturnExtraFinalData",ifnotfound = FALSE))
        extraFinalData <- list(inputData=data[,c(freqVarInd, dimVarInd),drop=FALSE],finalData=finalData)
      
      if (stacked & doUnstack) {
        if (is.null(singleOutput)) 
          singleOutput <- FALSE
        
        mainVar1 <- match("Freq", names(finalData))
        mainVar2 <- match("sdcStatus", names(finalData))
        mainVar3 <- match("supp6547524", names(finalData))
        
        stackVar <- match(varNames, names(finalData))
        stackVar <- stackVar[!is.na(stackVar)]
        
        stackVarNames <- names(finalData)[stackVar]
        
        blockVar <- match(dimVarNamesOrig, names(finalData))
        blockVar <- blockVar[!is.na(blockVar)]
        
        
        
        x1 <- Unstack(finalData, mainVar = mainVar1, stackVar = stackVar, blockVar = blockVar, 
                      sep = sep)
        
        
        
        x2 <- Unstack(finalData, mainVar = mainVar2, stackVar = stackVar, blockVar = blockVar, 
                      sep = sep)
        
        x3 <- Unstack(finalData, mainVar = mainVar3, stackVar = stackVar, blockVar = blockVar, 
                      sep = sep)
        
        
        if (namesAsInput) {
          w <- rbind(cbind(a12345645 = 1, rowData), cbind(a12345645 = 1, rowData))
          ww <- Unstack(w, stackVar = match(stackVarNames, names(w)), sep = sep)$rowData
          w2 <- cbind(outputNames = rownames(ww), ww, stringsAsFactors = FALSE)
          w1 <- cbind(inputNames = rownames(rowData), rowData, stringsAsFactors = FALSE)
          namesFrame <- merge(w1, w2)
          
          indNames <- match(namesFrame$outputNames, colnames(x1$data))
          indNames2 <- match(namesFrame$outputNames, colnames(x2$data))
          indNames3 <- match(namesFrame$outputNames, colnames(x3$data))
          
          
          if (sum(as.integer(indNames != indNames2))) 
            stop("Problems with namesAsInput")
          if (sum(as.integer(indNames != indNames3))) 
            stop("Problems with namesAsInput")
          
          colnames(x1$data)[indNames] <- namesFrame$inputNames
          colnames(x2$data)[indNames] <- namesFrame$inputNames
          colnames(x3$data)[indNames] <- namesFrame$inputNames
        }
        
        if (orderAsInput) {
          
          namesOrig <- dimVarNamesOrig[dimVarNamesOrig %in% names(gVC)]
          
          rInput <- unique(apply(CharacterDataFrame(dataOrig[, namesOrig, drop = FALSE]), 
                                 1, paste, collapse = "_"))
          rOutput <- apply(x1$data[, namesOrig, drop = FALSE], 1, paste, collapse = "_")
          rI <- match(rInput, rOutput)
          rI <- rI[!is.na(rI)]
          rAll <- seq_len(length(rOutput))
          rO <- rAll[!(rAll %in% rI)]
          if (totalFirst) 
            rr <- c(rO, rI) else rr <- c(rI, rO)
          
          cI <- match(colnames(dataOrig), colnames(x1$data))
          cI <- cI[!is.na(cI)]
          cAll <- seq_len(length(colnames(x1$data)))
          cO <- cAll[!(cAll %in% cI)]
          if (totalFirst) 
            cc <- c(cO, cI) else cc <- c(cI, cO)
          
          x1$data <- x1$data[rr, cc, drop = FALSE]
          x2$data <- x2$data[rr, cc, drop = FALSE]
          x3$data <- x3$data[rr, cc, drop = FALSE]
          rownames(x1$data) <- NULL
          rownames(x2$data) <- NULL
          rownames(x3$data) <- NULL
        }
        
        if (removeTotal) {
          colnames(x1$data) <- RemoveTotal(colnames(x1$data), total = total, sep = sep)
          colnames(x2$data) <- RemoveTotal(colnames(x2$data), total = total, sep = sep)
          colnames(x3$data) <- RemoveTotal(colnames(x3$data), total = total, sep = sep)
        }
        
        if (!singleOutput) {
          output <- list(info = info, x1 = x1$data, x2 = x2$data, x3 = x3$data)
          names(output) <- c("info", outFreq, outSdcStatus, outSuppressed)
        } else {
          bv <- seq_len(NCOL(x1$data)) %in% seq_len(length(blockVar))
          block <- x1$data[, bv, drop = FALSE]
          x1 <- x1$data[, !bv, drop = FALSE]
          x2 <- x2$data[, !bv, drop = FALSE]
          x3 <- x3$data[, !bv, drop = FALSE]
          names(x1) <- paste(outFreq, names(x1), sep = sep)
          names(x2) <- paste(outSdcStatus, names(x2), sep = sep)
          names(x3) <- paste(outSuppressed, names(x3), sep = sep)
          output <- list(info = info, data = cbind(block, x1, x2, x3))
        }
        if(get0("doReturnExtraFinalData",ifnotfound = FALSE))
          output <- c(output,extraFinalData)
        return(output)
      }
      if (is.null(singleOutput)) 
        singleOutput <- TRUE
      
      if (!stacked & orderAsInput) {
        # Fungerer ikke å ha denne før unstack
        rInput <- unique(apply(pt$common$dimData[, names(gVC), drop = FALSE], 1, 
                               paste, collapse = "_"))
        rOutput <- apply(finalData[, names(gVC), drop = FALSE], 1, paste, collapse = "_")
        rI <- match(rInput, rOutput)
        rI <- rI[!is.na(rI)]
        rAll <- seq_len(length(rOutput))
        rO <- rAll[!(rAll %in% rI)]
        
        if (totalFirst) 
          finalData <- finalData[c(rO, rI), , drop = FALSE] else finalData <- finalData[c(rI, rO), , drop = FALSE]
        rownames(finalData) <- NULL
      }
      
      names(finalData)[match(c("Freq", "sdcStatus", "supp6547524"), names(finalData))] <- c(outFreq, 
                                                                                            outSdcStatus, outSuppressed)
      
      
      if (singleOutput) 
        output <- list(info = info, data = finalData) else {
          output <- list(info = info, x1 = finalData[, !(names(finalData) %in% c(outSdcStatus, 
                                                                                 outSuppressed))], x2 = finalData[, !(names(finalData) %in% c(outFreq, 
                                                                                                                                              outSuppressed))], x3 = finalData[, !(names(finalData) %in% c(outFreq, 
                                                                                                                                                                                                           outSdcStatus))])
          names(output) <- c("info", outFreq, outSdcStatus, outSuppressed)
        }
      if(get0("doReturnExtraFinalData",ifnotfound = FALSE))
        output <- c(output,extraFinalData) 
      return(output)
}



GroupVarCombined <- function(x, reverse = FALSE) {
  l <- sapply(x, length)
  ml <- max(l)
  naml <- rep(NA, ml)
  for (i in 1:length(x)) x[[i]] <- c(x[[i]], naml)[seq_len(ml)]
  un <- unique(names(x))
  z <- vector("list", length(un))
  names(z) <- un
  for (i in seq_len(length(un))) {
    z[[i]] <- unique(as.vector(t(as.matrix(data.frame(x[names(x) == un[i]])))))
    z[[i]] <- c(z[[i]][!is.na(z[[i]])], 0)  # 0 as total code
    if (reverse) 
      z[[i]] <- rev(z[[i]])
  }
  z
}



if (FALSE) {
  # Generering av testdata
  z <- EasyData("sosialFiktiv")
  z8 <- z[z$fylke <= 10 & z$kostragr == 300, ]  # 8 regions
  
  z11 <- z[z$fylke <= 10 & (z$kostragr == 300 | z$kostragr == 400), ]  # 11 regions
  
  z8$kostragr <- "A"
  z8$kostragr[z8$region %in% c(43200, 51400, 62000, 83400)] <- "B"
  z36 <- z[z$fylke >= 11 & z$fylke <= 14 & z$kostragr <= 500, ]  # 36 regions
  
  z1 <- Unstack(z8, mainVar = match("ant", names(z8)), stackVar = match(c("hovedint"), 
                                                                        names(z8)), blockVar = match(c("region"), names(z8)))
  
  z2 <- Unstack(z11, mainVar = match("ant", names(z8)), stackVar = match(c("hovedint"), 
                                                                         names(z8)), blockVar = match(c("region", "fylke", "kostragr"), names(z8)))
  
  
  x1 <- aggregate(z1$data[, 2:5], list(region = z1$data$region), sum)
  x2 <- aggregate(z2$data[, 4:7], list(region = z2$data$region, fylke = z2$data$fylke, 
                                       kostragr = z2$data$kostragr), sum)
  y1 <- aggregate((z8[, 7]), list(region = z8$region, hovedint = z8$hovedint), 
                  sum)
  y2 <- aggregate((z11[, 7]), list(region = z11$region, fylke = z11$fylke, kostragr = z11$kostragr, 
                                   hovedint = z11$hovedint), sum)
  names(y1)[3] <- "ant"
  names(y2)[5] <- "ant"
  
  y3 <- z36
  mnd <- gsub("_", "m", y3$mnd)
  mnd <- gsub("6", "06", mnd)
  mnd <- gsub("9", "09", mnd)
  mnd <- gsub("1m", "01m", mnd)
  mnd <- gsub("5", "05", mnd)
  mnd <- paste("m", mnd, sep = "")
  y3$mnd <- mnd
  mnd <- gsub("-", "M", y3$mnd2)
  mnd <- gsub("6", "06", mnd)
  mnd <- gsub("1M", "01M", mnd)
  mnd <- gsub("5", "05", mnd)
  mnd <- paste("M", mnd, sep = "")
  y3$mnd2 <- mnd
  
  y3Unstack <- Unstack(y3, mainVar = match("ant", names(y3)), stackVar = match(c("hovedint", 
                                                                                 "mnd"), names(y3)), extraVar = match(c("mnd2"), names(y3)), blockVar = match(c("region", 
                                                                                                                                                                "fylke", "kostragr"), names(y3)))
  
  x3 <- y3Unstack$data
  
  
  x3b <- x3
  names(x3b)[4:15] <- paste(names(x3)[4:15], y3Unstack$rowData[, 3], sep = "_")
  
  ind <- NULL
  for (i in 1:NROW(y1)) ind <- c(ind, rep(i, y1$ant[i]))
  y1micro <- y1[ind, ]
  
  
  # rename and save
  if (FALSE) {
    z1 <- y1
    z1micro <- y1micro
    z2 <- y2
    z3 <- y3
    z1w <- x1
    z2w <- x2
    z3w <- x3
    z3wb <- x3b
    save(z1, file = "C:/R/easysdctable/data/z1.RData")
    save(z1micro, file = "C:/R/easysdctable/data/z1micro.RData")
    save(z2, file = "C:/R/easysdctable/data/z2.RData")
    save(z3, file = "C:/R/easysdctable/data/z3.RData")
    save(z1w, file = "C:/R/easysdctable/data/z1w.RData")
    save(z2w, file = "C:/R/easysdctable/data/z2w.RData")
    save(z3w, file = "C:/R/easysdctable/data/z3w.RData")
    save(z3wb, file = "C:/R/easysdctable/data/z3wb.RData")
  }
}




SortedFromDimList <- function(dimList1, dimList2 = NULL) {
  if (!is.null(dimList2)) {
    dimList <- rbind(dimList1, dimList2)
    return(unique(dimList[order(c(-5 * as.integer(factor(dimList1$levels)), -4 * 
                                    as.integer(factor(dimList2$levels))), c(dimList1$codes, dimList2$codes)), ])[, 2])
  }
  return(unique(dimList1[order(c(-5 * as.integer(factor(dimList1$levels))), c(dimList1$codes)), ])[, 2])
}

uniqueIndex <- function(x, ordered = FALSE) {
  ui <- seq_len(length(x))[!duplicated(x)]
  if (!ordered) 
    return(ui)
  ui[order(x[ui])]
}

SortedFromData <- function(xCharacter, ind, total, xNumeric = NULL) {
  z <- NULL
  for (i in ind) {
    if (i == 0) 
      x <- total else {
        if (!is.null(xNumeric)) 
          ui <- uniqueIndex(xNumeric[, i], ordered = TRUE) else ui <- uniqueIndex(xCharacter[, i], ordered = TRUE)
          x <- xCharacter[ui, i]
      }
    z <- c(z, x)
  }
  z
}

CharacterDataFrame <- function(x) {
  for (i in seq_len(NCOL(x))) x[, i] <- as.character(x[, i])
  x
}

RemoveTotal <- function(x, total = "Total", sep = "_") {
  x <- gsub(paste(total, sep, sep = ""), "", x)
  gsub(paste(sep, total, sep = ""), "", x)
}
