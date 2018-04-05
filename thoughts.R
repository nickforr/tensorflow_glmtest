

# might need to loop through slices?
#[Still wonder if tensors would be better...]

rtnIndices <- matrix(c(1, 1.02, 1.04, 1, 1.03, 1.05), nrow = 3)

denomIndex <- rtnIndices[2, ]
cbnSim <- matrix(c(3, 5) / denomIndex, ncol = 2)


x <- c(0, 1, 1)

y <- crossprod(cbnSim, x)

z <- y %*% rtnIndices
