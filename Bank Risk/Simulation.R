## Generate bivariate random variables with correlation

e1 = rnorm(10000)
e2 = rnorm(10000)

z1 = e1
rho = 0.6
z2 = rho * e1 + sqrt(1-rho^2) * e2


# Check it works
cor(e1,e2)
cor(z1, z2)

mean(z1)
sd(z1)

mean(z2)
sd(z2)

# proven by Choesky Decomposition
# 2D
A = matrix(c(1,0.6,0.6,1),2,2)
chol(A)
t(chol(A))
t(chol(A)) %*% chol(A)
# 3D
A = matrix(0.01, 3,3)
A[1,1] = 0.02
A[2,2] = 0.03
A[3,3] = 0.04
A
L = t(chol(A))
L

z = matrix(rnorm(300000),100000,3)
x = L %*% z
dim(x)
cov(x)

# Geometric Brownian Motion (stochastic differential equation)
