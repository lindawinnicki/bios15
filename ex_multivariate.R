library(MASS)
library(ellipse)
C = matrix(c(0.7, 0.2,-0.3,
0.2, 1.2, 0.4,
-0.3, 0.4, 0.6),
nrow=3)

# check its symmetry 
transpose_C <- t(C)
if (all(t(C) == transpose_C)) print("yup")
t(C) == transpose_C

C_cor <- matrix(nrow = 3, ncol = 3)

C_cor <- cov2cor(C)

for (element in C){
  if (element %in% diag(C)) {
    var <- element
  }
}


X <- data.frame(mvrnorm(200, c(0,0,0), C))
colnames(X) <- c("z1", "z2", "z3")

means = c(apply(X[,1:2], 2, mean)) # X[,1:2] we extract all rows, but only col 1 and 2

plot(X$z1, X$z2, las=1)
lines(ellipse(cov(X$z1, X$z2), centre = means), col = "skyblue")

cov(X[,1:2])
?apply
?ellipse
# plot(ellipse(0.8), type = 'l')
?mvrnorm
