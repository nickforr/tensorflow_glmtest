

Rcpp::sourceCpp("test.cpp")
x <- as.numeric(3:12)
y = as.numeric(1:10)
a <- mmultTest(x, y)
aa <- mmultTest2(x, y)
all.equal(a, aa)

Rcpp::sourceCpp("cppArmaAnnuity.cpp")

a <- matrix(1:12, 3, 4)

b <- testFill(a)
c <- transTestTwo(a)
all.equal(b, c)
