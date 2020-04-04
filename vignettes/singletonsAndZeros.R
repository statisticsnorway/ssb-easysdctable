## ----include = FALSE----------------------------------------------------------
library(knitr)
library(easySdcTable)
P <- function(df, caption = NULL){
  if(is.null(caption)){
    caption = deparse(substitute(df))
  }
  kable(df, "html", row.names = FALSE, caption = c("\n", caption), align = "r")
}

## ----comment=NA, tidy = TRUE, results = "hide", message = FALSE---------------
library(easySdcTable)

## ----comment=NA, tidy = FALSE-------------------------------------------------
data1a = data.frame(row = c("r1","r2"), A=c(0,2), B=c(1,0), H=c(7,0), M=c(1,2), W=c(0,8))
data1b = data.frame(row = c("r1","r2"), A=c(1,1), B=c(1,0), H=c(7,0), M=c(1,2), W=c(0,8))
data0a = data.frame(row = c("r1","r2"), A=c(5,5), B=c(0,9), H=c(7,9), M=c(0,5), W=c(9,8))
data0b = data.frame(row = c("r1","r2"), A=c(0,0), B=c(0,9), H=c(7,9), M=c(0,2), W=c(9,8))

## ----comment=NA, echo=FALSE---------------------------------------------------
P(data1a)
P(data1b)
P(data0a)
P(data0b)

## ----comment=NA, tidy = TRUE, results = "hide"--------------------------------
s1a = ProtectTable(data1a, 1, 2:6, protectZeros = FALSE,  method = "SIMPLEHEURISTIC", suppression=".")$suppressed

## ----comment=NA, echo=FALSE---------------------------------------------------
P(s1a)

## ----comment=NA, tidy = TRUE, results = "hide"--------------------------------
s1aSingle = ProtectTable(data1a, 1, 2:6, protectZeros = FALSE,  method = "SIMPLEHEURISTIC", detectSingletons=TRUE,
                         suppression=".")$suppressed

## ----comment=NA, echo=FALSE---------------------------------------------------
P(s1aSingle)

## ----comment=NA, tidy = TRUE, results = "hide"--------------------------------
s1bSingle = ProtectTable(data1b, 1, 2:6, protectZeros = FALSE,  method = "SIMPLEHEURISTIC", detectSingletons=TRUE,
                         suppression=".")$suppressed

## ----comment=NA, echo=FALSE---------------------------------------------------
P(s1bSingle)

## ----comment=NA, tidy = TRUE, results = "hide"--------------------------------
s1bThreshold4 = ProtectTable(data1b, 1, 2:6, protectZeros = FALSE,  method = "SIMPLEHEURISTIC", threshold=4,
                         suppression=".")$suppressed

## ----comment=NA, echo=FALSE---------------------------------------------------
P(s1bThreshold4)

## ----comment=NA, tidy = TRUE, results = "hide"--------------------------------
s0a = ProtectTable(data0a, 1, 2:6, protectZeros = TRUE,  method = "SIMPLEHEURISTIC", suppression=".")$suppressed

## ----comment=NA, echo=FALSE---------------------------------------------------
P(s0a)

## ----comment=NA, tidy = TRUE, results = "hide"--------------------------------
s0b = ProtectTable(data0b, 1, 2:6, protectZeros = TRUE,  method = "SIMPLEHEURISTIC", suppression=".")$suppressed

## ----comment=NA, echo=FALSE---------------------------------------------------
P(s0b)

## ----comment=NA, tidy = TRUE, results = "hide"--------------------------------
s0aThreshold1 = ProtectTable(data0a, 1, 2:6, protectZeros = TRUE,  method = "SIMPLEHEURISTIC", threshold = 1, suppression=".")$suppressed

## ----comment=NA, echo=FALSE---------------------------------------------------
P(s0aThreshold1)

## ----comment=NA, tidy = TRUE, results = "hide"--------------------------------
s0bThreshold1 = ProtectTable(data0b, 1, 2:6, protectZeros = TRUE,  method = "SIMPLEHEURISTIC", threshold = 1, suppression=".")$suppressed

## ----comment=NA, echo=FALSE---------------------------------------------------
P(s0bThreshold1)

