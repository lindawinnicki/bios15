# Choose any dataset you may have involving a continuous response variable and a continuous predictor. Fit a simple linear regression, interpret the results, produce a nice figure including the fitted regression line, and write simple methods and results presenting the analysis and results.

birds = read.csv("./data/bird_allometry.csv")
# head(birds)
# This dataset contains body mass and brain mass for males and females of different bird species. The scaling of brain size (or other body parts) with body size is referred to as the study of allometry, and you may want to read about these analyses before fitting your models. 
# As a hint, the scaling of parts of a body with body size is expected to follow a power-law relationship on the form y= axb, which can be linearized through the logarithmic transformation log(y) = log(a) + b * log(x).

# split data into females and males
females = birds[birds$Sex == "f",]
males = birds[birds$Sex == "m",]
# unique(birds$Genus_Species) # A LOT

# we need to log-transform and fit into a model

model_females = lm(log(brain_mass) ~ log(body_mass), data = females)
summary(model_females)

# regression values (predicted values)
cf_females = model_females$coefficients
cf_females
predicted_females = cf_females[1] + cf_females[2]*log(females$body_mass)
plot(
  log(females$body_mass), log(females$brain_mass), 
  las=1,
  xlab = "Body mass (log transformed) [g]",
  ylab = "Brain mass (log-transformed) [g]",
  main = "Body vs Brain Mass in Female Birds"
)
lines(log(females$body_mass), predicted_females, col = "skyblue")


model_males = lm(log(brain_mass) ~ log(body_mass), data = males)
summary(model_males)

cf_males = model_males$coefficients
cf_males
predicted_males = cf_males[1] + cf_males[2]*log(males$body_mass)
plot(
  log(males$body_mass), log(males$brain_mass), 
  las=1,
  xlab = "Body mass (log transformed) [g]",
  ylab = "Brain mass (log-transformed) [g]",
  main = "Body vs Brain Mass in Male Birds"
)
lines(log(males$body_mass), predicted_males, col = "red")
lines(log(females$body_mass), predicted_females, col = "blue")

par(mfrow=c(1,1))
hist(residuals(model_females))
hist(residuals(model_males))
