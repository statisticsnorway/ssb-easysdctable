#'  Wrapper to ProtectTable() with additional methods (experimental) 
#'
#'  Additional values of "method" is possible. Each new method (wrapper method) will make a call 
#'  to ProtectTable() using a specific parameter setting. 
#'  
#' @encoding UTF8
#' 
#' @param ...    Parameters to ProtectTable 
#' @param maxN   Parameter to ProtectTable
#' @param method Parameter to ProtectTable or a wrapper method (see details) 
#' @param exeArgus    Parameter to \code{\link{runArgusBatchFile}}
#' @param pathArgus   Parameter to \code{\link{createArgusInput}}
#' @param solverArgus Parameter "solver" to \code{\link{createArgusInput}}
#' @param methodArgus Parameter "method" to \code{\link{createArgusInput}}
#' @param rgArgus Parameter "rg" in "primSuppRules" in \code{\link{createArgusInput}}
#' 
#' @details The wrapper methods are:
#' 
#'  \strong{Simple:}  "SIMPLEHEURISTIC" with detectSingletons=FALSE
#'  
#'  \strong{SimpleSingle:}  "SIMPLEHEURISTIC" with detectSingletons=TRUE
#'  
#'  \strong{TauArgus:} Tau-argus will be run according to the settings of the other input parameters.
#'  
#'  Using \code{rgArgus=0} is equivalent to calling ProtectTable() with \cr 
#'  \code{method = list(exe=exeArgus, typ="tabular", path=pathArgus,} \cr
#'  \code{solver=solverArgus, method=methodArgus)))}
#'  
#'  Other values of \code{rgArgus} is equivalent to calling ProtectTable() with \cr
#'  \code{method = list(exe=exeArgus, typ="microdata", path=pathArgus,}\cr 
#'  \code{solver=solverArgus, method=methodArgus,}\cr
#'  \code{primSuppRules=list(list(type="freq", n=maxN+1, rg=rgArgus )))))}
#'  
#'  \strong{TauArgusOPT:} As "TauArgus" with \code{methodArgus="OPT"}
#'  
#'  \strong{TauArgusMOD:} As "TauArgus" with \code{methodArgus="MOD"}
#'  
#'  \strong{TauArgusGH:}  As "TauArgus" with \code{methodArgus="GH"}
#'
#' @return See \code{\link{ProtectTable}}
#'         
#' @export
#'
PTwrap = function(..., maxN=3, method, exeArgus="C:/Tau/TauArgus.exe", 
                  pathArgus=getwd(), solverArgus= "FREE", methodArgus="OPT", rgArgus=0 ){
  
  if(method=="Simple") return(
    ProtectTable(..., maxN=maxN, method="SIMPLEHEURISTIC", detectSingletons=FALSE))
  
  if(method=="SimpleSingle") return(
    ProtectTable(..., maxN=maxN, method="SIMPLEHEURISTIC", detectSingletons=TRUE))
  
  
  if(method=="TauArgus" & rgArgus==0) return(
    ProtectTable(..., maxN=maxN, 
        method = list(exe=exeArgus, typ="tabular", path=pathArgus, solver=solverArgus, method=methodArgus)))
  
  if(method=="TauArgus" & rgArgus!=0) return(
    ProtectTable(..., maxN=-1, # No primary suppression by sdcTable
                 method = list(exe=exeArgus, typ="microdata", path=pathArgus, solver=solverArgus, method=methodArgus,
                 primSuppRules=list(list(type="freq", n=maxN+1, rg=rgArgus )))))
  
  if(method=="TauArgusOPT")
    return(PTwrap(..., maxN=maxN, method="TauArgus", exeArgus=exeArgus, pathArgus=pathArgus, solverArgus=solverArgus,
                  methodArgus="OPT"))
  if(method=="TauArgusMOD")
    return(PTwrap(..., maxN=maxN, method="TauArgus", exeArgus=exeArgus, pathArgus=pathArgus, solverArgus=solverArgus,
                  methodArgus="MOD"))
  if(method=="TauArgusGH")
    return(PTwrap(..., maxN=maxN, method="TauArgus", exeArgus=exeArgus, pathArgus=pathArgus, solverArgus=solverArgus,
                  methodArgus="GH"))
  
  return(ProtectTable(..., maxN=maxN, method=method)) # When no new method
  
  } 
  
  
  