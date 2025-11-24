library(dplyr)
source("./functions/f_CV.R")
source("./functions/f_SE.R")

bees <- read.csv("./data/Eulaema.csv")
# * The mean annual temperature (MAT) is given in degrees Celsius times 10
# * The annual precipitation in mm
# * The temperature seasonality (Tseason) as 100 times the standard deviation of the monthly temperature
# * The precipitation seasonality (Pseason) as the CV of monthly precipitation (given as a percent, i.e. times 100).
# * The effort variable in the log number of hours of sampling
head(bees)
str(bees)
unique(bees$method)

#filtering out the outlier
bees <- bees |> 
  filter(Eulaema_nigrita != 1054)
hist(bees$Eulaema_nigrita)

par(mfrow = c(1,2))

bees$SA <- as.factor(bees$SA)

ggplot(data = bees,
  aes(
    method, 
    Eulaema_nigrita
  )
) + 
  geom_boxplot() +
  facet_wrap(bees$SA)

ggplot(data = bees,
  aes(
    altitude, 
    Eulaema_nigrita,
    color = Pseason
  )
) + 
  geom_point() +
  # geom_line(aes(y = altitude), color = "blue") + 
  geom_bar(aes(y = Pseason), stat = "identity", fill = "gray")

m <- glm(bees$Eulaema_nigrita ~ bees$effort + bees$MAT + bees$altitude + bees$MAP, "poisson")
summary(m)


?hist
?geom_point
?aes
?ylim
max(bees$Eulaema_nigrita)
