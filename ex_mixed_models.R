rm(list = ls())
library(glmmTMB)
library(ggplot2)

# set.seed(145)
x1 = rnorm(200, 10, 2) # mean 10 sd 2

groupmeans = rep(rnorm(10, 20, 4), each=20) # mean 20, sd 4
groupID = as.factor(rep(paste0("Group", 1:10), each=20))
y = 2 + 1.5*x1 + groupmeans + rnorm(200, 0, 2)

par(mfrow = c(1,1))
plot(x1, y, col=as.numeric(groupID), las=1)
# we could...
# ... be interested in how much do group differences contribute to the overall spread or variation in our outcome variable?
# ... be looking at the relationship between one predictor variable and the outcome - should we take into account whether individuals belong to certain groups?.

# variance component analysis ####
df_mixed <- data.frame(y, x1, groupID)
head(df_mixed)

m <- glmmTMB(y ~ 1 + (1 | groupID), data = df_mixed)
summary(m)

# VarCorr instead of coefs
VarCorr(m)
VarCorr(m)$cond$groupID
VarAmongGroups <- attr(VarCorr(m)$cond$groupID, "stddev")^2
# Among groups - refers to the differences between group means or averages.

VarWithinGroups <- attr(VarCorr(m)$cond, "sc")^2
# Within groups - refers to the spread or dispersion of individual data points within each group.

VarAmongGroups
var(groupmeans)
# > VarAmongGroups
# (Intercept) 
#    9.204914 
# > var(groupmeans)
# [1] 9.805633
# imagine each mean to have this little cloud of uncertainty around it, so just computing the mean from the raw numbers makes the raw variation "inflated" 
# what the mixed model does, is:
mean_var_noise <- mean(tapply(y, INDEX = groupID, FUN = var)/20)
var(groupmeans) - mean_var_noise #[1] 9.212205
#basically, it calculates the mean variational noise, the "cloud" around each group mean variation, takes the mean to get an overall mean for all the groups, and subtracts that from the raw mean variance. 

#calculating the percentage of the variation explained by groups:
VarAmongGroups / (VarAmongGroups + VarWithinGroups) * 100 # 43.6801

# to interpret the differences of the variances, we could scale them to CV (SD / mean). To maintain additive properties, we square the CV (var / mean^2)

CV_among <- sqrt(VarAmongGroups) / mean(y)
CV_within <- sqrt(VarWithinGroups) / mean(y)

CV_squared_among <- VarAmongGroups / mean(y)^2
CV_squared_within <- VarWithinGroups / mean(y)^2

#then they make a table for no good reason
df_table <- data.frame(Mean = mean(x1))

# Data exercise: Variance partitioning with random-effects models. ####
df_butterflies <- read.csv("./data/butterflies.csv")

head(df_butterflies)
m <- glmmTMB(DevelopmentTime ~ LarvalHost * MaternalHost + (1 | MotherID), data = df_butterflies)
summary(m)

AIC(m)


VarAmongMotherID <- attr(VarCorr(m)$cond$MotherID, "stddev")^2
VarWithinGroups <- attr(VarCorr(m)$cond, "sc")^2

CV_squared_among <- VarAmongMotherID / mean(df_butterflies$DevelopmentTime)^2
CV_squared_within <- VarWithinGroups / mean(df_butterflies$DevelopmentTime)^2

CV_squared_total <- CV_squared_among + CV_squared_within

df_table <- data.frame(Mean = mean(df_butterflies$DevelopmentTime), 
SD = sd(df_butterflies$DevelopmentTime), 
Var_Among = VarAmongMotherID / (VarAmongMotherID + VarWithinGroups) * 100,
Var_Within = VarWithinGroups / (VarAmongMotherID + VarWithinGroups) * 100,
CV_squared_among, 
CV_squared_within, 
CV_squared_total)

df_table = apply(df_table, MARGIN=2, FUN=round, digits=5)
df_table
# group_means <- tapply(df_butterflies$DevelopmentTime, df_butterflies$MotherID, mean)

var(group_means)

# comparing with and without random effects ####

m_naive <- lm(y ~ x1, data = df_mixed)
m_random <- glmmTMB(y ~ x1 + (1 | groupID), df_mixed)

summary(m_naive)
summary(m_random)
AIC(m_naive)

plot(x1, y, las=1, col = groupID)
newx = seq(min(x1), max(x1), length.out=200)
coef(m_random)

for(i in 1:length(levels(groupID))){
y_hat = coef(m_random)$cond$groupID[i,1] + coef(m_random)$cond$groupID[i,2]*newx
lines(newx, y_hat, col=i)
}

coef(m_random)$cond$groupID[10,1]

# f <- function(x,y) 2 * x^2 - 3 * x * y + 8 * y^2 + 2 * x - 4 * y + 4
# x <- seq(-20, 20, length = 100)
# y <- seq(-20, 20, length = 100)

# z <- outer(x, y, f)

# persp(z, phi = 30, theta = 90)
# contour(z)
# ?persp

bees <- read.csv("./data/Eulaema.csv")
head(bees)

m <- glmmTMB(Eulaema_nigrita ~ MAP + (1 | SA), data = bees, "nbinom2")
summary(m)

newTseason <- seq(min(bees$MAP), max(bees$MAP), length = 72)
plot(bees$MAP, bees$Eulaema_nigrita)
for (i in 1:length(levels(bees$SA))){
  y_hat = exp(coef(m)$cond$SA[i,1] + coef(m)$cond$SA[i,2]*newTseason)
  lines(newTseason, y_hat)
}

coef(m)



?levels
