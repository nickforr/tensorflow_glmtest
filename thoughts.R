

# might need to loop through slices?
#[Still wonder if tensors would be better...]

rtnIndices <- matrix(c(1, 1.02, 1.04, 1, 1.03, 1.05), nrow = 3)
salIndices <- matrix(c(1, 1.01, 1.02, 1, 1, 1.01), nrow = 3)

cbnRate <- c(0, 0.1, 0.15)

initialPot <- 100
sal <- 20
cbns <- diag(cbnRate) %*% (sal * salIndices)
adjCbns <- cbns / rtnIndices

nproj <- 3
iproj <- 2

x <- (diag(c(rep.int(0, iproj - 1), rep.int(1, nproj - iproj + 1))) %*% rtnIndices) %*% diag(adjCbns[iproj, ])



denomIndex <- rtnIndices[2, ]
cbnSim <- matrix(c(3, 5) / denomIndex, ncol = 2)


x <- c(0, 1, 1)

y <- crossprod(cbnSim, x)

z <- y %*% rtnIndices


a <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3)
b <- c(10, 20)
c <- diag(b)

d <- a %*% c
dd <- crossprod(a, b)


