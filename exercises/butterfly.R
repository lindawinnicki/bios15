library(ggplot2)
source("./functions/f_SE.R")

# read the data####
df_butterflies <- read.csv("./data/butterflies.csv")

# df_butterflies$LarvalHost = paste0(df_butterflies$LarvalHost, "L")
# df_butterflies$MaternalHost = paste0(df_butterflies$MaternalHost, "M")

#  some statistics
means = tapply(df_butterflies$DevelopmentTime, list(df_butterflies$MaternalHost, df_butterflies$LarvalHost), mean)
# sds = tapply(df_butterflies$DevelopmentTime, list(df_butterflies$MaternalHost, df_butterflies$LarvalHost), sd)
SEs = tapply(df_butterflies$DevelopmentTime, list(df_butterflies$MaternalHost, df_butterflies$LarvalHost), SE)

# data set for plotting mean values and standard error bars
df_dev_means <- data.frame(
  MaternalHost = c("Barbarea", "Berteroa", "Barbarea", "Berteroa"),
  LarvalHost = c("Barbarea", "Berteroa", "Berteroa", "Barbarea"),
  MeanDev = c(means[1,1], means[2,2], means[1,2], means[2,1]),
  MeanSE = c(SEs[1,1], SEs[2,2], SEs[1,2], SEs[2,1]))

# amt of mothers in data
length(unique(df_butterflies$MotherID))

# amt of larvae in the data
length(unique(df_butterflies$LarvalID))

# models ####
# linear model, 2 factors
lm_butterfly <- lm(DevelopmentTime ~ MaternalHost * LarvalHost, data = df_butterflies)
summary(lm_butterfly)
# anova 1 and 2-way
anova(lm_butterfly)



#plots ####
# linear plot
ggplot(
  data = df_dev_means, 
  aes( #mapping
    LarvalHost, MeanDev, 
    colour = MaternalHost, 
    group = MaternalHost)) +
  geom_point(size = 3) + #adding the points
  geom_errorbar(aes( #adding error bars
    ymin = MeanDev - MeanSE,
    ymax = MeanDev + MeanSE,
  ),
  width = 0.06,
  linetype = 1
  ) +
  scale_y_continuous( # y-axis modification
    limits = c(21.5, 31.5), 
    breaks = seq(21.5, 31.5, by = 2)) +
  geom_line(size = 1) + #adding lines
  labs( #labels
    x = "Larval Host Plant", 
    y = "Mean Development Time [days]", 
    color = "Maternal Host Plant") +
  scale_colour_brewer(palette = "Set1") +
  theme_minimal() + 
  theme(
    legend.position = c(0.25, 0.85),
    legend.text = element_text(size = 14),  # legend item text size
    legend.title = element_text(size = 14),  # legend title size
    text = element_text(family = "Palatino"),
    axis.title.x = element_text(size = 16), # x-axis title size
    axis.title.y = element_text(size = 16), # y-axis title size
    axis.text.x = element_text(size = 12),  # x-axis tick labels
    axis.text.y = element_text(size = 12)   # y-axis tick labels
)
?labs
