library(dplyr)
library(ggplot2)
library(MASS)
source("./functions/f_CV.R")
source("./functions/f_SE.R")

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

#filtering out the outlier
# bees <- bees |> 
#   filter(Eulaema_nigrita != 1054)
# hist(bees$Eulaema_nigrita)

par(mfrow = c(1,2))

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

m <- glm(Eulaema_nigrita ~ MAT + Tseason + Pseason + offset(effort),
    family = "poisson", data = bees)

summary(m)

deviance(m) / df.residual(m)

1-(m$deviance/m$null.deviance)
# [1] 186.3061, very high overdispersion, lets try negative binomial

# offsets makes the effort exactly proportional for bee counts, that is, more effort -> more bees. no errors, no variation, its perfectly linear. 
m_nb <- glm.nb(Eulaema_nigrita ~ MAT + Tseason + Pseason + offset(effort), data = bees)

summary(m_nb)
plot(m_nb)
deviance(m_nb) / df.residual(m_nb)
# [1] 1.22788, overdispersion handled well

# if effort was considered a predictor, and not proportional:
# m_nb_2 <- glm.nb(Eulaema_nigrita ~ MAT + Tseason + Pseason + effort, data = bees)

# making a dataset ####
MAT_mean <- mean(bees$MAT, na.rm = TRUE)
effort_mean <- mean(bees$effort, na.rm = TRUE) #try with mean center values
Tseason_seq <- seq(min(bees$Tseason), max(bees$Tseason), length = 200)
Pseason_seq <- seq(min(bees$Pseason), max(bees$Pseason), length = 200)

# lets create the set for Tseason ####
df_bees_T <- data.frame(
  MAT = MAT_mean,
  effort = effort_mean,
  Tseason = Tseason_seq,
  Pseason = mean(bees$Pseason) # we only want 1 variying response
)

# mean centered set
# df_bees_T_center <- data.frame(
#   MAT = 0,
#   effort = 0,
#   Tseason = seq(min(bees$Tseason), max(bees$Tseason), length = 200),
#   Pseason = 0 # we only want 1 variying response
# )

# predicted counts on response scale
pred_T <- predict(m_nb, newdata=df_bees_T, type="response")
# type = "response" gives the real counts, not log-counts

# if effort was considered a predictor, and not proportianl
# pred_T_2 <- predict(m_nb_2, newdata=df_bees_T, type="response")

# mean centered values
# pred_T_center <- predict(m_nb, newdata=df_bees_T_center, type="response")

# 95% confidendence bands
pred_T_se <- predict(m_nb, newdata=df_bees_T, type="link", se.fit=TRUE)
# "link" -> linear predictor (log-counts in a log-link model)

# if effort was considered a predictor, and not proportianl
# pred_T_se_2 <- predict(m_nb_2, newdata=df_bees_T, type="link", se.fit=TRUE)

# mean centered values
# pred_T_se_center <- predict(m_nb, newdata=df_bees_T_center, type="link", se.fit=TRUE)

upper_T <- exp(pred_T_se$fit + 1.96*pred_T_se$se.fit)
lower_T <- exp(pred_T_se$fit - 1.96*pred_T_se$se.fit)

# if effort was considered a predictor, and not proportianl
# upper_T_2 <- exp(pred_T_se_2$fit + 1.96*pred_T_se_2$se.fit)
# lower_T_2 <- exp(pred_T_se_2$fit - 1.96*pred_T_se_2$se.fit)

# mean centered values
# upper_T_center <- exp(pred_T_se_center$fit + 1.96*pred_T_se_center$se.fit)
# lower_T_center <- exp(pred_T_se_center$fit - 1.96*pred_T_se_center$se.fit)

# now dataset for Pseason ####
df_bees_P <- data.frame(
  MAT = MAT_mean,
  effort = effort_mean,
  Tseason = mean(bees$Tseason),
  Pseason = Pseason_seq
)

# prediction values on count scale
pred_P <- predict(m_nb, newdata = df_bees_P, type = "response")

# confidence bands (based on normality, therefor link)
# CI=η±1.96⋅SE(η) (log-scale)
pred_P_se <- predict(m_nb, newdata = df_bees_P, type = "link", se.fit = T)

# transformation to count scale
upper_P <- exp(pred_P_se$fit + (1.96 * pred_P_se$se.fit))
lower_P <- exp(pred_P_se$fit - (1.96 * pred_P_se$se.fit))
# fit holds the values of predicted bee counts
# se.fit holds the SE values of the predictions (SE(η))


# plotting ####
par(mfrow = c(1,2))
plot(Tseason_seq, pred_T, 
  type="l",
  las = 1, 
  col="blue",
  ylim = c(0,400),
  xlab="Temperature Seasonality (Tseason)",
  ylab="Predicted bee count",   
  main="Effect of Tseason on Bee Counts",)
lines(Tseason_seq, upper_T, col="blue", lty=2)
lines(Tseason_seq, lower_T, col="blue", lty=2)
points(bees$Tseason, bees$Eulaema_nigrita, col = rgb(0,0,0,0.3))

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

plot(Pseason_seq, pred_P, 
  type="l", 
  las = 1,
  col="blue",
  ylim = c(0, 400),
  xlab="Precipitation Seasonality (Pseason)",
  ylab="Predicted bee count",
  main="Effect of Pseason on Bee Counts")
lines(Pseason_seq, upper_P, col="blue", lty=2)
lines(Pseason_seq, lower_P, col="blue", lty=2)
points(bees$Pseason, bees$Eulaema_nigrita,
  col = rgb(0, 0, 0, 0.3))

?hist
?geom_point
?aes
?predict
?predict.glm
max(bees$Eulaema_nigrita)