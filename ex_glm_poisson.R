library(MuMIn)
library(MASS)
# Poisson and negative-binomial regression ####
x <- rpois(200, 3)
# ?rpois
# lambda is the mean (and also the variance) of that Poisson distribution

# For low values the distribution is highly skewed, for high values the distribution approaches a Gaussian (normal) distribution. However, the variance is still constrained to be the same as the mean, which is not the case for the normal distribution.
hist(x, las = 1)

x <- seq(0, 20, 1)
y <- dpois(x, 1)
# ?dpois

plot(x,y, 
  type = "b", 
  las=1, 
  xlab="k", 
  ylab="P(x=k)", 
  pch=16, 
  col=1)
points(x, dpois(x, lambda=3), 
type="b", 
pch=16, 
col=2)
points(x, dpois(x, lambda=10), 
type="b", 
pch=16, 
col=3)
legend("topright", col=1:3, pch=16,
legend=c(expression(paste(lambda, " = 1")),
expression(paste(lambda, " = 3")),
expression(paste(lambda, " = 10"))))


# A better option (to log-transforming) is to analyze the data in a GLM framework with Poisson-distributed errors and a log link function.

x <- rnorm(200, 10, 3)
eta <- -2 + 0.2*x
y <- ceiling(exp(eta + rpois(200, 0.3)))

par(mfrow=c(1,2))
plot(x, eta, las=1)
plot(x, y, las=1)

m <- glm(y~x, family = "poisson")
summary(m)
# As in all GLMs the parameters are reported on the link scale, here log. To plot the fitted regression line, we have to back-transform the predicted values.
mean(predict(m))
?predict

xx = seq(min(x), max(x), 0.01)
y_hat = predict(m, newdata=list(x=xx), type="response", se.fit=T)
# new_data = list(x = xx) “Predict the outcome for new values of 𝑥, stored in the object xx"
# type = "response" "Give me predictions of E(y), not the linear predictor η. For GLM Poisson, regressrion, give me the count data"
# se.fit = TRUE: This asks R to also compute the standard error of the prediction.
par(mfrow = c(1,1))
plot(x, y, las=1, col="darkgrey", pch=16)
lines(xx, y_hat$fit)

upper <- y_hat$fit + 1.96*y_hat$se.fit
lower <- y_hat$fit - 1.96*y_hat$se.fit
lines(xx, upper, lty=2) # upper band
lines(xx, lower, lty=2) # lower band

polygon(c(xx, rev(xx)),
c(y_hat$fit+1.96*y_hat$se.fit,
rev(y_hat$fit-1.96*y_hat$se.fit)),
col = rgb(0,1,0,.5), border = FALSE)

# As in logistic regression the normal r2 is not valid, and we can use e.g. the r.squaredGLMM function to obtain a Pseudo r2.
# If a model is as bad as a null model, the Pseudo r2 will be 0.
r.squaredGLMM(m)

# The deviance is a measure of how far our model is from a “perfect” or saturated model, i.e. one that perfectly explains the data.
1-(m$deviance/m$null.deviance)

# In real count data the variance often increase disproportionally compared to the mean, a phenomenon called overdispersion.
# We can quantify overdispersion in the data based on the fitted model by calculating the ratio of the residual deviance to the residual degrees of freedom.
# lets create some serious overdispersion

x <- rnorm(200, 10, 3)
eta <- -2 + .2*x
y <- floor(exp(eta + rnbinom(200, 1, mu=.8)))
#rnbinom, negative binomial, 1 for the dispersion parameters ("number of failures"), and mu is the mean of the dist.
par(mfrow=c(1,2))
plot(x, eta, las=1)
plot(x, y, las=1)

m = glm(y~x, family="poisson")
summary(m)
# here the overdispersion is serious - estimates of the model not to be trusted
# In this case, we need an alternative link function that allows the variance to increase more than the mean. The negative binomial distribution is a good option. The negative binomial is similar to the Poisson distribution, but includes an additional parameter modelling the disproportionate increase in variance with increasing mean.

m = glm.nb(y~x)
summary(m)
211/222
