library(ggplot2)
# read the data####
df_butterflies <- read.csv("./data/butterflies.csv")
# names(df_butterflies)
# head(df_butterflies)

# renaming
df_butterflies$LarvalHost = paste0(df_butterflies$LarvalHost, "L")
df_butterflies$MaternalHost = paste0(df_butterflies$MaternalHost, "M")

# mean and sd values
mn_t <- tapply(df_butterflies$DevelopmentTime, list(df_butterflies$MaternalHost, df_butterflies$LarvalHost), mean)
sd_t <- tapply(df_butterflies$DevelopmentTime, list(df_butterflies$MaternalHost, df_butterflies$LarvalHost), sd)
mn_t
sd_t

# model ####
dev_model <- lm(DevelopmentTime ~ MaternalHost * LarvalHost, data = df_butterflies)
summary(dev_model)

dev_model2 <- lm(DevelopmentTime ~ LarvalHost * MaternalHost, data = df_butterflies)
summary(dev_model2)

anova(dev_model)
# Analysis of Variance Table

# Response: DevelopmentTime
#                          Df  Sum Sq Mean Sq F value    Pr(>F)    
# MaternalHost              1  623.61  623.61  177.90 < 2.2e-16 ***
# LarvalHost                1 2682.41 2682.41  765.21 < 2.2e-16 ***
# MaternalHost:LarvalHost   1   80.80   80.80   23.05 2.561e-06 ***
# Residuals               283  992.05    3.51                      


# plot ####
boxplot(DevelopmentTime ~ MaternalHost * LarvalHost, data = df_butterflies,
col = c("skyblue", "red"))

ggplot(
  data = df_butterflies,
  aes(x = MaternalHost, y = DevelopmentTime, fill = LarvalHost)
) + 
  geom_boxplot() + 
  points(mn_t, col = "skyblue") +
  theme_classic()



summary(dev_model)

getwd()
