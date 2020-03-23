library(RegSDC)
library(Matrix)

PTxyzTest = function(..., rmse = 1, nRep = 2){
  a <- PTxyz(..., IncProgress=NULL)
  s <- Matrix::crossprod(a$x,SuppressDec(a$x, a$z, rmse = rmse, nRep = nRep))[which(is.na(a$z)), ,drop=FALSE]
  rowSumsDes <- Matrix::rowSums(s)
  rowSumsRoundDes <- Matrix::rowSums(round(s))
  expect_false(any(rowSumsDes==rowSumsRoundDes))
}


# Data for testing threshold and detectSingletons
z <- data.frame(a = rep(1:5, each = 7), b = 1:7, y = 4:10, y0 = 4:10, y1 = 4:10)
z$y0[(z$y + 1.7 * z$a) > 12] <- 0
z$y1[(z$y + 1.6 * z$a) > 9.7] <- 1  # 9


test_that("Simple works", {
  PTxyzTest(EasyData("z1"), c("region","hovedint") ,"ant", method = "Simple")
  PTxyzTest(EasyData("z3") ,1:6,7, method = "SIMPLEHEURISTIC") 
  PTxyzTest(z, 1:2,"y0", protectZeros = TRUE,  method = "Simple")
  PTxyzTest(z, 1:2,"y1", protectZeros = FALSE,  method = "Simple")
})

test_that("SimpleSingle works", {
  PTxyzTest(z, 1:2, "y0", protectZeros = TRUE)
  w <- ProtectTable(z, 1:2, "y0", protectZeros = TRUE, IncProgress = NULL)$data
  expect_true(sum(w[w$b == 3 & is.na(w$suppressed), "freq", drop = TRUE]) > 0)
  
  PTxyzTest(z, 1:2, "y1", protectZeros = FALSE)
  W <- ProtectTable(z, 1:2, "y1", protectZeros = FALSE, IncProgress = NULL)$data
  expect_true(sum(W[w$b == 1 & is.na(W$suppressed), "freq", drop = TRUE] - 1) > 0)
  expect_true(sum(W[w$b == 1 & is.na(W$suppressed), "freq", drop = TRUE] - 1) > 0)
})


