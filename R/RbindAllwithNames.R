
# Special version of RbindAll in SSBtools used when infoAsFrame = TRUE in protectTable

RbindAllwithNames <- function(...,toRight=FALSE,extra=""){
  x = list(...)
  sap = sapply(x,is.null)
  if(any(sap)) x = x[!sap]
  for(i in seq_len(length(x))){
    c1 = colnames(x[[i]])
    c2 = c1
    c2[] = extra
    a = rbind(c1,x[[i]],c2)
    if(toRight) a = a[rev(seq_len(NCOL(a)))]
    colnames(a) = NULL
    rownames(a) = NULL
    colnames(a) = colnames(a, do.NULL=FALSE) 
    x[[i]] = a
  }
  a = RbindAllnoNA(x)
  if(toRight){
    a = a[rev(seq_len(NCOL(a)))]
  }
  colnames(a)[] =""
  a
}


RbindAllnoNA <- function(...){
  x = list(...)
  if(length(x)==1) # Handle list input
    if(is.list(x[[1]]))
      if(!is.data.frame(x[[1]]))
        x = x[[1]]
      n = length(x)
      allColnames = NULL
      for(i in seq_len(n)) 
        allColnames = unique(c(allColnames,colnames(x[[i]])))
      for(i in seq_len(n))
        x[[i]][, c(as.character(setdiff(allColnames, colnames(x[[i]]))))] <- "" #NA
      eval(parse(text = paste("rbind(",paste("x[[",seq_len(n),"]],",collapse = ""),"deparse.level = 0)")))
}