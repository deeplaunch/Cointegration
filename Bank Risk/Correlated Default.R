n = 10; d = 2; rho = 0.2
R = matrix(rho, n, n)
diag(R) = 1

library(MASS)
x = mvrnorm(n = 10000, matrix(0, 10), R)

numdef = rep(0, 10000)
for (j in 1: 10000) {numdef[j] = length(which(x[j,]<= -d))}

hist(numdef)

# When correlation are high, both left and right tails get higher
# (No default more likely, as well as large number of defaults)
