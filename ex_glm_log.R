library(MuMIn)
# setting the scene
x = seq(from=0, to=1, by=0.01) # p-values
v_b = x*(1-x) # theoretical binomial variance σ2 = np(1−p)

par(mfrow=c(1,1))
plot(
  x, v_b,
  type="l", 
  xlab="Probability", 
  ylab="Theoretical variance", 
  las=1
)
# variance is largest when p = 0.5, when theres a 50% prob. of "yes"
# smallest when we have almost no "yes" / almost all "yes"

#log = natural logarithm, NOT log10 transformation ####
logit = function(x) log(x/(1-x))
invlogit = function(x) 1/(1+exp(-x))

logit_x = logit(x)
print(x[1:10])
print(logit_x[1:10])
print(invlogit(logit_x[1:10])) # the inverse function reverses the logit function


x = runif(200)
logit_x = logit(x)
invlogit_x = invlogit(x)


par(mfrow=c(1,2))
hist(x, las=1)
hist(logit_x, las=1)
# hist(invlogit_x, las = 1)

par(mfrow=c(1,1))
plot(x)
points(invlogit_x, col = "red")
points(logit_x, col = "skyblue")

par(mfrow=c(1,3))
plot(x)
plot(logit_x, col = "skyblue")
plot(invlogit_x, col = "red")

xx = seq(-5, 5, 0.01)

par(mfrow=c(1,2))
hist(xx, las=1)
hist(logit(xx), las=1)

par(mfrow=c(1,1))
plot(xx)
points(invlogit(xx), col = "red")
points(logit(xx), col = "skyblue")

plot(xx, 
  invlogit(xx), 
  type="l", las=1,
  xlab="Logit (x)",
  ylab="P"
)
plot(x, 
  invlogit(logit_x), 
  las=1)

# probit link transformation ####
par(mfrow=c(1,1))
plot(xx, invlogit(xx), type="l", las=1,
xlab="Logit/Probit (x)",
ylab="P")

# probit link - corresponds to the quantile distribution of the standard normal distribution (which is why we can use the pnorm function to compute the inverse).
lines(xx, pnorm(xx), lty=2)
legend("topleft", legend=c("Logit", "Probit"),
lty=c(1,2), bty="n")

print(x[1:10])
print(pnorm(x[1:10]))
pnorm(0)

# Logistic regression ####
x = rnorm(200, 10, 3)
eta =-2 + 0.4*x + rnorm(200, 0, 2) # formulating a linear predictor η

plot(eta)

p = invlogit(eta) #transforming the predicted values into probabilities

plot(p)
?rbinom
y = rbinom(200, 1, p) # finally binarizing the data by sampling from the binomial distribution

par(mfrow=c(1,3))
plot(x, eta, las=1)
plot(x, p, las=1, ylab = "probability")
plot(x, y, las=1)

m = glm(y~x, family=binomial(link="logit"))
summary(m)
# the summary parameters are on the linked scale, here logit
#exercise plot ####

x <- rnorm(200, 10, 3) # i guess these are observations

# linear predictor ? 
eta =-2 + 0.4*x + rnorm(200, 0, 2)

par(mfrow = c(1,1))
plot(x, eta) #i guess its somewhat linear

#transforming into probabilities ?
p = invlogit(eta) 
y = rbinom(200, 1, p)

# model fitting
m <- glm(y ~ x, binomial(link = "logit"))
summary(m)
x_coefs <- summary(m)$coef

# im not sure
x_pred <- seq(min(x), max(x), by = 0.01)
y_hat <- x_coefs[1,1] + x_coefs[2,1]*x_pred
p_hat <- invlogit(y_hat)

p_invlogit <- invlogit(p)

x_50 <- -x_coefs[1]/x_coefs[2]

plot(x, y)
lines(x_pred, p_hat)
abline(h = 0.5, v = x_50, lty = 2)

# normal r2 does not work for logistic regression ####
# There are however several ‘Pseudo-r2’ available, typically based on comparing the likelihood of the model to that of a null model (a similar model but with only an intercept).
# install.packages("tidyverse")

r.squaredGLMM(m)
#                   R2m       R2c
# theoretical 0.2195086 0.2195086
# delta       0.1426155 0.1426155

# The coefficient of discrimination is defined as D=π1−π0
# π1 define the amt of successes and π0 the failures

mean(p_hat[which(y==1)]) - mean(p_hat[which(y==0)]) 
# predicted ^ [1] -0.003837369

y_hat <- x_coefs[1,1] * x_coefs[2,1] * x
p_hat <- invlogit(y_hat)

mean(p_hat[which(y==1)]) - mean(p_hat[which(y==0)]) 
# ^ actual [1] -0.01199074

# Poisson and negative-binomial regression ####
x <- rpois(200, 2)
# lambda is the mean (and also the variance) of that Poisson distribution
# For low values the distribution is highly skewed, for high values the distribution approaches a Gaussian (normal) distribution. However, the variance is still constrained to be the same as the mean, which is not the case for the normal distribution.
hist(x, las = 1)

