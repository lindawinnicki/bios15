set.seed(85)
x = rnorm(n=200, mean=10, sd=2) # normally distr. data points made
y = 0.4*x + rnorm(200, 0, 1) # y = kx + m, m from normal distribution (mean = 0, sd = 1)
plot(x, y, las=1,
xlab="Leaf length (mm)",
ylab="Leaf width (mm)")


model = lm(y ~ x) #linear model
str(model)
summary(model)

cf = model$coefficients

# predicted values
predvals = cf[1] + cf[2]*x # the values of the regression line
?par

par(mfrow=c(1,2))
plot(x, y, las=1,
xlab="Leaf length (mm)",
ylab="Leaf width (mm)")
lines(x, predvals) # this makes a line within the data points
abline(model) # built in function, making the same line ^but not within
segments(x, y, x, predvals) # shows the residuals as vertical lines between observed and predicted values
hist(residuals(model), xlab="", las=1) # this is what is meant by that the residuals are "fine"

?seq
newx = seq(min(x), max(x), length.out=200)
predy = cf[1] + cf[2]*newx
plot(x, y, las=1,
xlab="Leaf length (mm)",
ylab="Leaf width (mm)")
lines(newx, predy)

par(mfrow=c(2,2))
plot(model) #gives lots of diff plots

summary(model)
#Multiple R-squared:  0.4311
# "43.1% of the variance in y is explained by x"
# or..
cor(x,y)^2
# [1] 0.4310643
# can be calculated by the variance of the the predicted values / by the variance of the observed values
var(predvals) / var(y)
# [1] 0.4310643
cf[1]^2*var(x) #gives the same as var(prevdvals)