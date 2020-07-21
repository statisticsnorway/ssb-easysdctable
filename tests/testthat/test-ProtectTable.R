library(RegSDC)
library(Matrix)

PTxyzTest = function(..., rmse = 1, nRep = 2){
  a <- PTxyz(..., IncProgress=NULL)
  s <- Matrix::crossprod(a$x,SuppressDec(a$x, a$z, a$y, rmse = rmse, nRep = nRep))[which(is.na(a$z)), ,drop=FALSE]
  rowSumsDes <- Matrix::rowSums(RoundWhole(s))
  rowSumsRoundDes <- Matrix::rowSums(round(s))
  expect_false(any(rowSumsDes==rowSumsRoundDes))
}


# Data for testing threshold and detectSingletons
z <- data.frame(a = rep(1:5, each = 7), b = 1:7, y = 4:10, y0 = 4:10, y1 = 4:10)
z$y0[(z$y + 1.7 * z$a) > 12] <- 0
z$y1[(z$y + 1.6 * z$a) > 9.7] <- 1  # 9


test_that("Simple works", {
  PTxyzTest(EasyData("z1"), c("region","hovedint") ,"ant", method = "Simple")
  # PTxyzTest(EasyData("z3") ,1:6,7, method = "SIMPLEHEURISTIC") # linked tables, fails
  PTxyzTest(z, 1:2,"y0", protectZeros = TRUE,  method = "Simple")
  PTxyzTest(z, 1:2,"y1", protectZeros = FALSE,  method = "Simple")
})

test_that("SimpleSingle works", {
  PTxyzTest(z, 1:2, "y0", protectZeros = TRUE)
  w <- ProtectTable(z, 1:2, "y0", protectZeros = TRUE, IncProgress = NULL)$data
  expect_true(sum(w[w$b == 3 & is.na(w$suppressed), "freq", drop = TRUE]) > 0)
  
  PTxyzTest(z, 1:2, "y1", protectZeros = FALSE)
  w <- ProtectTable(z, 1:2, "y1", protectZeros = FALSE, IncProgress = NULL)$data
  expect_true(sum(w[w$b == 1 & is.na(w$suppressed), "freq", drop = TRUE] - 1) > 0)
})

test_that("Gauss works", {
  PTxyzTest(EasyData("z1"), c("region","hovedint") ,"ant", method = "Gauss", printInc=FALSE)
  PTxyzTest(EasyData("z3") ,1:6,7, method = "Gauss", printInc=FALSE) 
  PTxyzTest(z, 1:2, "y0", protectZeros = TRUE, method = "Gauss", printInc=FALSE)
  w <- ProtectTable(z, 1:2, "y0", protectZeros = TRUE, method = "Gauss", IncProgress = NULL, printInc=FALSE)$data
  expect_true(sum(w[w$b == 3 & is.na(w$suppressed), "freq", drop = TRUE]) > 0)
  PTxyzTest(z, 1:2, "y1", protectZeros = FALSE, method = "Gauss", printInc=FALSE)
  w <- ProtectTable(z, 1:2, "y1", protectZeros = FALSE, method = "Gauss", IncProgress = NULL, printInc=FALSE)$data
  expect_true(sum(w[w$b == 1 & is.na(w$suppressed), "freq", drop = TRUE] - 1) > 0)
})


Gauss6 <- function(...) {
  m <- NULL
  singletonMethod <- c("none", "subSum", "anySum", "subSumAny", "subSpace", "subSumSpace")
  for (i in seq_along(singletonMethod)) {
    a <- ProtectTable(..., method = "Gauss", singletonMethod = singletonMethod[i], IncProgress = NULL, printInc = FALSE)
    m <- cbind(m, a$data$suppressed)
  }
  colnames(m) <- singletonMethod
  expect_identical(m[, "anySum"], m[, "subSumAny"])
  expect_identical(m[, "subSpace"], m[, "subSumSpace"])
  k <- apply(m, 2, sumIsNa)[c(1, 2, 3, 5)]
  expect_true(min(diff(k)) > 0)
  m
}

sumIsNa <- function(x) sum(is.na(x))

test_that("Gauss ok singleton methods", {
  m0 <- Gauss6(z, 1:2, "y0")
  m1 <- Gauss6(z, 1:2, "y1", protectZeros = FALSE)
})
