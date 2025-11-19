set.seed(187)
x1 = rnorm(200, 10, 2)
x2 = 0.5*x1 + rnorm(200, 0, 4) # creates a vector, and then another, adding them together, you end up with a new vector
y = 0.7*x1 + 2.2*x2 + rnorm(200, 0, 4)

model = lm(y~x1+x2)

summary(model)

# R-squared ####
#                          Total variance #

# Multiple R-squared:  0.8683,	Adjusted R-squared:  0.8669 
# r^2 = 0.8683, which means that approx. 86.8% of the variance of predicted y is explained by x1 and x2

# Can also be manually calculated:

coefs = summary(m)$coef
y_hat = coefs[1,1] + coefs[2,1]*x1 + coefs[3,1]*x2 # predicted values

var(y_hat) # predicted variance 
var(y) # total variance
var(y_hat) / var(y) # [1] 0.8682827

#                      Variance of x1 and x2 #

# To compute the predicted values associated only with x1, we keep x2 constant at its mean, and vice versa for the variance associated with x2.

y_hat1 <- coefs[1,1] + coefs[2,1]*x1 + coefs[3,1]*mean(x2)

var(y_hat1) / var(y) #[1] 0.01635149
# 1.6% of variance explained by x1

y_hat2 = coefs[1,1] + coefs[2,1]*mean(x1) + coefs[3,1]*x2

var(y_hat2) / var(y) #[1] 0.8059861
# 80.6% of variance explained by x2

# BUT, this is not all - because y is explained by 86.8%, but x1 and x2 together (0.8059861 + 0.01635149 = 0.8223376) only explain approx. 82.2%. 

# Recall - Var(x+ y) = Var(x) + Var(y) + 2Cov(x,y)
var(y_hat1) + var(y_hat2) + 2*cov(y_hat1, y_hat2) #[1] 85.4221
var(y_hat) #[1] 85.4221

# As before, we can also do this by computing V(x) = βx^2σx^2
coefs[2,1]^2*var(x1)
var(y_hat1)
coefs[3,1]^2*var(x2)
var(y_hat2)

# Covariance Matrix ####

t(coefs[2:3,1]) %*% cov(cbind(x1,x2)) %*% coefs[2:3,1]

# Z transformation ####

# If all variables had the same variance, then the variance explained would be directly proportional to the regression slope. The most common way to standardize predictor variables is to scale them to zero mean and unit variance, a so-called z-transform

x1_z <- ((x1 - mean(x1))/sd(x1))
x2_z = ((x2- mean(x2))/sd(x2))

# The resulting variable will have a mean of zero and a standard deviation (and variance) of one (remember to check that this is indeed the case).

mean(z_x1)
sd(z_x1)
var(z_x1)

model_z <- lm(y ~ z_x1 + z_x2)
summary(model_z)

# Multiple R-squared, F, DF, SE etc. has not changed - but the parameter Estimates has
# v v v v v v v v v v v v v v v v v 

# Residual standard error: 3.618 on 197 degrees of freedom
# Multiple R-squared:  0.8683,	Adjusted R-squared:  0.8669 
# F-statistic: 649.3 on 2 and 197 DF,  p-value: < 2.2e-16

# First, the intercept can now be interpreted as the mean of y, because it represents the value of y when both predictors have a value of 0 (i.e. their mean after the z-transform).
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  19.4090     0.2558  75.866  < 2e-16 *** <---- mean of y
# z_x1          1.2683     0.2618   4.845 2.56e-06 ***
# z_x2          8.9047     0.2618  34.017  < 2e-16 ***

# Second, the slopes now have units of standard deviations, i.e. they describe the change in y per standard deviation change in each predictor. This shows directly that the predictor x2 explains more variance in y than does x1.


# Mean similarity / natural log transformation ####

# Another useful transformation could be a natural log-transform, or similarly mean-scaling, which would give the slopes units of means, and allow interpreting the change in yper percent change in x. These proportional slopes are technically called elasticities.

x1_m <- ((x1-mean(x1))/mean(x1))
x2_m <- ((x2-mean(x2))/mean(x2))

model_mean <- lm(y ~ m_x1 + m_x2)

summary(model_mean)

# Multicollinearity ####

# A rule of thumb is that such multicollinearity becomes a potential problem when the correlation between the predictors is greater than 0.6 or 0.7.

# One way of assessing the degree of multicollinearity is to compute variance inflation factors, defined as:
# VIF = 1/1-r^2 (r^2 for that specific covariate). lets find out :D
model_covariate <- lm(x1 ~ x2)
summary(model_covariate) #r-squared: 0.04004
r2 <- summary(model_covariate)$r.squared
1/(1-r2) #VIF
#[1] 1.041714
# Rules of thumb for what constitutes severe variance inflation range from VIF > 3 to VIF > 10. When this occurs, the parameter estimates become associated with excessive variance and are thus less reliable.
# so our VIF is still low enough to be reliable - and the two predictors are not strongly correlated