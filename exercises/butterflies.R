# read the data

library(ggplot2)

df_butterflies <- read.csv("./data/butterflies.csv")
# names(df_butterflies)
# head(df_butterflies)

# # splitting the data based on maternal host
m_barbarea <- df_butterflies[df_butterflies$MaternalHost == "Barbarea",]
# m_berteroa <- df_butterflies[df_butterflies$MaternalHost == "Berteroa",]

# # splitting the data based on larval host
l_barbarea <- df_butterflies[df_butterflies$LarvalHost == "Barbarea",]
# l_berteroa <- df_butterflies[df_butterflies$LarvalHost == "Berteroa",]

barbarea_ = m_barbarea + l_barbarea

dev_model <- lm(DevelopmentTime ~ MaternalHost * LarvalHost, data = df_butterflies)
anova(dev_model)
# Analysis of Variance Table

# Response: DevelopmentTime
#                          Df  Sum Sq Mean Sq F value    Pr(>F)    
# MaternalHost              1  623.61  623.61  177.90 < 2.2e-16 ***
# LarvalHost                1 2682.41 2682.41  765.21 < 2.2e-16 ***
# MaternalHost:LarvalHost   1   80.80   80.80   23.05 2.561e-06 ***
# Residuals               283  992.05    3.51                      

ggplot(
  data = df_butterflies,
  aes(x = MaternalHost, y = DevelopmentTime, fill = LarvalHost)
) + 
  geom_boxplot()

summary(dev_model)




getwd()
