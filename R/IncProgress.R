#' Default IncProgress function
#' @export
#' 
#' @note 
#' Instead of using \code{IncProgress = IncDefault} in ProtectTable1/ProtectTable one could use \cr
#' \code{IncProgress = function(){cat("."); flush.console()}} \cr
#' but this results in wrong 
#' "usage line" in the documentation since ";" is not included.
#' 
#' @keywords internal
#'
#' @examples
#'  IncDefault() 
IncDefault =  function(){
  cat(".")
  flush.console()}