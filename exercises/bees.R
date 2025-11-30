library(dplyr)
library(ggplot2)
library(MASS)
library(car)

#reading the data ####
bees <- read.csv("./data/Eulaema.csv")
# * The mean annual temperature (MAT) is given in degrees Celsius times 10
# * The annual precipitation in mm
# * The temperature seasonality (Tseason) as 100 times the standard deviation of the monthly temperature
# * The precipitation seasonality (Pseason) as the CV of monthly precipitation (given as a percent, i.e. times 100).
# * The effort variable in the log number of hours of sampling

head(bees[168:171, ])
str(bees)
unique(bees$method)
mean(bees$Eulaema_nigrita)
median(bees$Eulaema_nigrita)
max(bees$Eulaema_nigrita)

# subset(bees, Eulaema_nigrita == 1054)

#filtering out the outlier ?
# bees <- bees |> 
#   filter(Eulaema_nigrita != 1054)
# hist(bees$Eulaema_nigrita)

bees$method <- as.factor(bees$method)

# ggplot(data = bees,
#   aes(
#     method, 
#     Eulaema_nigrita
#   )
# ) + 
#   geom_boxplot() +
#   facet_wrap(bees$SA)

ggplot(data = bees,
  aes(
    Tseason, 
    Eulaema_nigrita,
    color = MAT
  )
) + 
  geom_count() 
  # geom_line(aes(y = altitude), color = "blue") + 
  # geom_bar(aes(y = Pseason), stat = "identity", fill = "gray")

#models ####
# general linear model
m <- glm(Eulaema_nigrita ~ MAT + Tseason + Pseason + offset(effort),
    family = "poisson", data = bees)

summary(m)
plot(m)
deviance(m) / df.residual(m) #dispersion statistic
# [1] 186.3061, very high overdispersion, lets try negative binomial

# negative binomial
# offsets makes the effort exactly proportional for bee counts, that is, more effort -> more bees. no errors, no variation, its perfectly linear. 
m_nb <- glm.nb(Eulaema_nigrita ~ MAT + MAP + Tseason + Pseason + offset(effort), data = bees)
vif(m_nb) # seems fine

plot(m_nb)
deviance(m_nb) / df.residual(m_nb)
# [1] 1.2088, overdispersion handled well

# comparing the two models
plot(fitted(m), bees$Eulaema_nigrita)
points(fitted(m_nb), bees$Eulaema_nigrita, col = "red")
abline(0, 1, col = "skyblue")

# response = the actual count data, not in log scale
plot(residuals(m_nb), predict(m_nb, type = "response"))
abline(h = 0, col = "red")




# some notes from the summary ####
summary(m_nb)

# Call:
# glm.nb(formula = Eulaema_nigrita ~ MAT + MAP + Tseason + Pseason + 
#     offset(effort), data = bees, init.theta = 0.7910403184, link = log)

# Coefficients:
#               Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  5.0095771  1.4341650   3.493 0.000478 ***
# MAT         -0.0056387  0.0047198  -1.195 0.232206    
# MAP         -0.0015825  0.0002363  -6.697 2.13e-11 ***
# Tseason     -0.0012540  0.0001884  -6.655 2.82e-11 ***
# Pseason      0.0146916  0.0042890   3.425 0.000614 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# (Dispersion parameter for Negative Binomial(0.791) family taken to be 1)

#     Null deviance: 291.34  on 177  degrees of freedom
# Residual deviance: 209.12  on 173  degrees of freedom
# AIC: 1836

# Number of Fisher Scoring iterations: 1


#               Theta:  0.7910 
#           Std. Err.:  0.0773 

#  2 x log-likelihood:  -1823.9940 
cor(bees$MAP, bees$Pseason)


# MAP estimate
exp(-0.0015825) # MAP estimate -> [1] 0.9984188
# for each mm, bee count decreases by 0.16%
# stadardized value MAP
exp(-0.0015825 * sd(bees$MAP))  #[1] 0.5233039
# for each SD of the MAP estimate, we a predicted decrease of 47.67% 


# MAT estimate
exp(-0.0056387) # MAT -> [1] 0.9943772
# for each MAT value (degree celcius / 10), bee count decreases by 0.57% 
# stadardized value MAT
-0.0056387 * 10 # -> coef per every degree C
sd(bees$MAT) / 10 # -> sd per every degree C
exp((-0.0056387 * 10) * (sd(bees$MAT) / 10)) # [1] 0.8802881
# for each SD of the MAP estimate, the count is predcited to decrease by 11.97%

# Tseason estimate
exp(-0.0012540) #Tseason -> [1] 0.9987468
# 1 unit of Tseason = 1% of a degree standard deviation (SD / 100).
# for each Tseason unit, bee count decreases by 0.13%
# over a range
exp(-0.0012540 * 100) # [1] 0.882144
# so for each SD unit, bee count decreases by 11.79%
# across the range?
range(bees$Tseason) # [1]  542 3283
# 542 / 100 and 3283 / 100 corresponds to the variability in degree celcius. this means that for locales of the lowest Tseason value experience a change of 5.42 degrees per month. 
exp(-0.0012540 * (3283-542)) # [1] 0.03215414
# across the whole range, bee counts are predicted to drop by 96.78% !


# P season estimate
exp(0.0146916) #Pseason -> [1] 1.0148
# for each Pseason unit (1% CV), we see a 1.48% increase of bee counts
# each unit = 1% increase in precipitation variability ?
# standardized
exp(0.0146916 * sd(bees$Pseason))
# predicted bee counts increase by 36.56% for a site that is 1 SD higher in Pseason than average
# across rnage?
range(bees$Pseason) # [1] 10 92
# tbh im not sure how to interpret this, but lets go
exp(0.0146916 * (92-10)) # [1] 3.335796
# across the whole range, bee count is predicted to increase by 233.58%
82/2 # 41 is in the middle of the range, let's predict that
exp(0.0146916 * 41) # [1] 1.826416
# according to the model - compared to the lowest, the sites with median precipitation seasonality is predicted to increase bee count by 82.64%




# making a dataset ####
MAT_mean <- mean(bees$MAT)
MAP_mean <- mean(bees$MAP)
MAP_seq <- seq(min(bees$MAP), max(bees$MAP), length = 200)
effort_mean <- mean(bees$effort, na.rm = TRUE) # note to self: try with mean centered values
Tseason_seq <- seq(min(bees$Tseason), max(bees$Tseason), length = 200)
Pseason_seq <- seq(min(bees$Pseason), max(bees$Pseason), length = 200)


# set for Tseason ####
df_bees_T <- data.frame(
  MAP = MAP_mean,
  MAT = MAT_mean,
  effort = effort_mean,
  Tseason = Tseason_seq,
  Pseason = mean(bees$Pseason) # we only want 1 variying response
)

# predicted counts on response scale
pred_T <- predict(m_nb, newdata=df_bees_T, type="response")
# type = "response" gives the real counts, not log-counts

# 95% confidendence bands, has to be on the log scale
pred_T_se <- predict(m_nb, newdata = df_bees_T, type = "link", se.fit = TRUE)
# "link" -> linear predictor (log-counts in a log-link model)

highest_T <- exp(pred_T_se$fit + 1.96*pred_T_se$se.fit)
lowest_T <- exp(pred_T_se$fit - 1.96*pred_T_se$se.fit)

# now dataset for Pseason ####
df_bees_P <- data.frame(
  MAP = MAP_mean,
  MAT = MAT_mean,
  effort = effort_mean,
  Tseason = mean(bees$Tseason),
  Pseason = Pseason_seq
)

# prediction values on count scale
pred_P <- predict(m_nb, newdata = df_bees_P, type = "response")

# confidence bands (based on normality, therefor link)
# CI=η±1.96⋅SE(η) (log-scale)
pred_P_se <- predict(m_nb, newdata = df_bees_P, type = "link", se.fit = TRUE)

# transformation to count scale
highest_P <- exp(pred_P_se$fit + (1.96 * pred_P_se$se.fit))
lowest_P <- exp(pred_P_se$fit - (1.96 * pred_P_se$se.fit))
# fit holds the values of predicted bee counts
# se.fit holds the SE values of the predictions (SE(η))

# MAP values
df_bees_MAP <- data.frame(
  MAP = MAP_seq,
  MAT = MAT_mean,
  effort = effort_mean,
  Tseason = mean(bees$Tseason),
  Pseason = mean(bees$Pseason) # we only want 1 variying response
)

pred_MAP <- predict(m_nb, newdata = df_bees_MAP, type = "response")

pred_MAP_se <- predict(m_nb, newdata = df_bees_MAP, type = "link", se.fit = TRUE)

highest_MAP <- exp(pred_MAP_se$fit + 1.96 * pred_MAP_se$se.fit)
lowest_MAP <- exp(pred_MAP_se$fit - 1.96 * pred_MAP_se$se.fit)

# other data set just for funz (can be ignored)####

# mean centered set
# df_bees_T_center <- data.frame(
#   MAT = 0,
#   effort = 0,
#   Tseason = seq(min(bees$Tseason), max(bees$Tseason), length = 200),
#   Pseason = 0 # we only want 1 variying response
# )

# mean centered values
# pred_T_center <- predict(m_nb, newdata=df_bees_T_center, type="response")

# mean centered values
# pred_T_se_center <- predict(m_nb, newdata=df_bees_T_center, type="link", se.fit=TRUE)

# mean centered values
# upper_T_center <- exp(pred_T_se_center$fit + 1.96*pred_T_se_center$se.fit)
# lower_T_center <- exp(pred_T_se_center$fit - 1.96*pred_T_se_center$se.fit)

# if effort was considered a predictor, and not proportional:
# m_nb_2 <- glm.nb(Eulaema_nigrita ~ MAT + Tseason + Pseason + effort, data = bees)

# if effort was considered a predictor, and not proportianl
# pred_T_se_2 <- predict(m_nb_2, newdata=df_bees_T, type="link", se.fit=TRUE)

# if effort was considered a predictor, and not proportianl
# upper_T_2 <- exp(pred_T_se_2$fit + 1.96*pred_T_se_2$se.fit)
# lower_T_2 <- exp(pred_T_se_2$fit - 1.96*pred_T_se_2$se.fit)

# plotting ####
par(mfrow = c(1,2))
plot(Tseason_seq, pred_T, 
  type="l",
  las = 1, 
  col="skyblue",
  # ylim = c(0,400),
  xlab="Temperature Seasonality (Tseason)",
  ylab="Predicted bee count",   
  main="Effect of Tseason on Bee Counts",)
lines(Tseason_seq, highest_T, col="skyblue", lty=2)
lines(Tseason_seq, lowest_T, col="skyblue", lty=2)
# points(bees$Tseason, bees$Eulaema_nigrita, col = rgb(0,0,0,0.3))

range(bees$Tseason)
range(pred_T)
max(pred_T) / min(pred_T) # fold-change
(max(pred_T) - min(pred_T)) / min(pred_T) * 100 # percent change


plot(Pseason_seq, pred_P, 
  type="l", 
  las = 1,
  col="skyblue",
  # ylim = c(0, 400),
  xlab="Precipitation Seasonality (Pseason)",
  ylab="Predicted bee count",
  main="Effect of Pseason on Bee Counts")
lines(Pseason_seq, highest_P, col="skyblue", lty=2)
lines(Pseason_seq, lowest_P, col="skyblue", lty=2)
# points(bees$Pseason, bees$Eulaema_nigrita, col = rgb(0, 0, 0, 0.3))

par(mfrow = c(1,1))
plot(MAP_seq, pred_MAP, 
  type="l", 
  las = 1,
  col="skyblue",
  # ylim = c(0, 400),
  xlab="Mean Average Precipitation [mm]",
  ylab="Predicted bee count",
  main="Effect of MAP on Bee Counts")
lines(MAP_seq, highest_MAP, col="skyblue", lty=2)
lines(MAP_seq, lowest_MAP, col="skyblue", lty=2)


# plot(Tseason_seq, pred_T_2, type="l", col="blue",
#      xlab="Temperature Seasonality (Tseason)",
#      ylab="Predicted bee count",
#      main="Effect of Tseason on Bee Counts 2")
# lines(Tseason_seq, upper_T_2, col="blue", lty=2)
# lines(Tseason_seq, lower_T_2, col="blue", lty=2)

# plot(Tseason_seq, pred_T_center, type="l", col="blue",
#      xlab="Temperature Seasonality (Tseason)",
#      ylab="Predicted bee count",
#      main="Effect of Tseason on Bee Counts")
# lines(Tseason_seq, upper_T_center, col="blue", lty=2)
# lines(Tseason_seq, lower_T_center, col="blue", lty=2)

?hist
?geom_point
?aes
?predict
?predict.glm
max(bees$Eulaema_nigrita)