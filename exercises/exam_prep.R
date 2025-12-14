rm(list = ls())

library(tidyverse)
library(glmmTMB)
library(ggplot2)
library(performance)
library(DHARMa)

df_eucalypt <- read.csv("./data/exam2023_data-2.csv")

df_eucalypt$seedlings_tot <- df_eucalypt$euc_sdlgs0_50cm + 
  df_eucalypt$euc_sdlgs50cm.2m + 
  df_eucalypt$euc_sdlgs.2m

df_eucalypt$Season <- as.factor(df_eucalypt$Season)
df_eucalypt$Property <- as.factor(df_eucalypt$Property)
df_eucalypt$Landscape.position <- as.factor(df_eucalypt$Landscape.position)

# model_cols <- c("seedlings_tot",
  # "Season",
  # "Property",
  # "ExoticAnnualGrass_cover",
  # "NativePerennialGrass_cover",
  # "Euc_canopy_cover",
  # "Distance_to_Eucalypt_canopy.m.",
  # "annual_precipitation",
  # "precipitation_warmest_quarter",
  # "precipitation_coldest_quarter",
  # "MrVBF",
  # "SRad_Jan",
  # "SRad_Jul",
  # "Landscape.position")
# 

model_cols_new <- c("seedlings_tot",
  "Season",
  "Property",
  "BareGround_cover",
  "Litter_cover",
  "MossLichen_cover",
  "Rock_cover"
)
#

# df_model <- df_eucalypt[, model_cols]
df_model_new <- df_eucalypt[, model_cols_new]

# m <- glmmTMB(seedlings_tot ~ ExoticAnnualGrass_cover + NativePerennialGrass_cover + Euc_canopy_cover + Distance_to_Eucalypt_canopy.m. + annual_precipitation + precipitation_warmest_quarter + precipitation_coldest_quarter + Landscape.position + MrVBF + SRad_Jan + SRad_Jul + (1 | Season) + (1 | Property), poisson, data = df_model) # model not happy with NA values

# na_rows <- which(!complete.cases(df_model))
# cat("rows", na_rows, "will be removed, in total:", length(na_rows), "rows")
# # 5 rows are fine, lets remove them
# df_model_clean <- na.omit(df_model)
# # seem to have removed the whole rows, good!

# removing NA values ####
na_rows <- which(!complete.cases(df_model_new))
cat("rows", na_rows, "will be removed, in total:", length(na_rows), "rows")
df_model_clean_new <- na.omit(df_model_new)

df_long_cover = df_model_new %>%
  pivot_longer(cols = c(BareGround_cover, Litter_cover, MossLichen_cover, Rock_cover), names_to = "Cover_Type", values_to = "Cover")

ggplot(df_long_cover, aes(x = Cover_Type, y = Cover, fill = Cover_Type)) +
  geom_boxplot(alpha = 0.7) +       
  xlab("Cover Type") +
  ylab("Cover") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# scaling ####
cont_var <- setdiff(model_cols_new, c("seedlings_tot", "Season","Property"))
df_model_clean_new[cont_var] <- lapply(df_model_clean_new[cont_var], scale)
# summary stats ####
# m <- glmmTMB(seedlings_tot ~ ExoticAnnualGrass_cover + NativePerennialGrass_cover + Euc_canopy_cover + Distance_to_Eucalypt_canopy.m. + annual_precipitation + precipitation_warmest_quarter + precipitation_coldest_quarter + Landscape.position + MrVBF + SRad_Jan + SRad_Jul + Season + (1 | Property), "nbinom2", data = df_model_clean)
# season was first added as random effect, but since there are only 3 seasons in the data, the model wasnt happy.. set as a fixed effect instead :/

#  Family: nbinom2  ( log )
# Formula:          seedlings_tot ~ ExoticAnnualGrass_cover + 
#     NativePerennialGrass_cover +  
#     Euc_canopy_cover + Distance_to_Eucalypt_canopy.m. + annual_precipitation +  
#     precipitation_warmest_quarter + precipitation_coldest_quarter +  
#     Landscape.position + MrVBF + SRad_Jan + SRad_Jul + Season +      (1 | Property)
# Data: df_model_clean

#       AIC       BIC    logLik -2*log(L)  df.resid 
#     903.0     972.3    -433.5     867.0       328 

# Random effects:

# Conditional model:
#  Groups   Name        Variance Std.Dev.
#  Property (Intercept) 2.036    1.427   
# Number of obs: 346, groups:  Property, 18

# Dispersion parameter for nbinom2 family (): 0.171 

# Conditional model:
#                                  Estimate Std. Error z value Pr(>|z|)  
# (Intercept)                     4.197e-01  1.070e+01   0.039   0.9687  
# ExoticAnnualGrass_cover         6.742e-03  1.391e-02   0.485   0.6279  
# NativePerennialGrass_cover     -6.543e-03  1.450e-02  -0.451   0.6518  
# Euc_canopy_cover                1.463e-02  1.461e-02   1.002   0.3165  
# Distance_to_Eucalypt_canopy.m. -2.832e-02  1.294e-02  -2.188   0.0287 *
# annual_precipitation           -6.905e-03  5.730e-02  -0.120   0.9041  
# precipitation_warmest_quarter  -5.649e-02  1.982e-01  -0.285   0.7757  
# precipitation_coldest_quarter   3.000e-02  9.143e-02   0.328   0.7428  
# Landscape.positionflat          2.222e+00  1.256e+00   1.769   0.0768 .
# Landscape.positionslope         2.254e+00  1.113e+00   2.026   0.0428 *
# Landscape.positiontoe_of_slope  3.565e+00  1.448e+00   2.462   0.0138 *
# MrVBF                          -1.226e-01  1.665e-01  -0.736   0.4615  
# SRad_Jan                        1.718e-05  5.560e-05   0.309   0.7574  
# SRad_Jul                       -7.895e-06  3.893e-05  -0.203   0.8393  
# SeasonSpring 2006              -1.968e-01  4.952e-01  -0.397   0.6910  
# SeasonWinter 2006               3.253e-01  4.882e-01   0.666   0.5052  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# plot(df_eucalypt$Landscape.position, df_eucalypt$seedlings_tot)

# summary stats ####
# m <- glmmTMB(seedlings_tot ~ 
#   Euc_canopy_cover + 
#   Distance_to_Eucalypt_canopy.m. + 
#   annual_precipitation + 
#   Landscape.position + 
#   MrVBF + 
#   SRad_Jan + 
#   SRad_Jul + 
#   Season + 
#   (1 | Property), 
# "nbinom2", data = df_model_clean)

# Family: nbinom2  ( log )
# Formula:          seedlings_tot ~ Euc_canopy_cover + Distance_to_Eucalypt_canopy.m. +  
#     annual_precipitation + Landscape.position + MrVBF + SRad_Jan +  
#     SRad_Jul + Season + (1 | Property)
# Data: df_model_clean

#       AIC       BIC    logLik -2*log(L)  df.resid 
#     897.3     951.2    -434.7     869.3       332 

# Random effects:

# Conditional model:
#  Groups   Name        Variance Std.Dev.
#  Property (Intercept) 2.485    1.576   
# Number of obs: 346, groups:  Property, 18

# Dispersion parameter for nbinom2 family (): 0.171 

# Conditional model:
#                                  Estimate Std. Error z value Pr(>|z|)  
# (Intercept)                    -4.114e+00  9.269e+00  -0.444   0.6572  
# Euc_canopy_cover                1.264e-02  1.406e-02   0.899   0.3687  
# Distance_to_Eucalypt_canopy.m. -3.133e-02  1.284e-02  -2.441   0.0146 *
# annual_precipitation           -2.819e-03  2.975e-03  -0.947   0.3435  
# Landscape.positionflat          2.220e+00  1.245e+00   1.784   0.0745 .
# Landscape.positionslope         2.275e+00  1.090e+00   2.087   0.0369 *
# Landscape.positiontoe_of_slope  3.509e+00  1.420e+00   2.471   0.0135 *
# MrVBF                          -1.093e-01  1.714e-01  -0.637   0.5240  
# SRad_Jan                        2.629e-05  5.435e-05   0.484   0.6285  
# SRad_Jul                       -1.487e-05  3.875e-05  -0.384   0.7011  
# SeasonSpring 2006              -2.056e-01  4.146e-01  -0.496   0.6200  
# SeasonWinter 2006               1.587e-01  4.259e-01   0.373   0.7094  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# summary stats for the new ####

m_new <- glmmTMB(seedlings_tot ~ 
  BareGround_cover + 
  Litter_cover + 
  MossLichen_cover + 
  Rock_cover + 
  # Distance_to_Eucalypt_canopy.m. + 
  # Season + 
  (1 | Property),
  # (1 | Season),
df_model_clean_new, "nbinom1")

#  Family: nbinom1  ( log )
# Formula:          seedlings_tot ~ BareGround_cover + Litter_cover + MossLichen_cover +  
#     Rock_cover + (1 | Property)
# Data: df_model_clean_new

#       AIC       BIC    logLik -2*log(L)  df.resid 
#     859.8     886.7    -422.9     845.8       339 

# Random effects:

# Conditional model:
#  Groups   Name        Variance Std.Dev.
#  Property (Intercept) 1.244    1.116   
# Number of obs: 346, groups:  Property, 18

# Dispersion parameter for nbinom1 family (): 14.1 

# Conditional model:
#                   Estimate Std. Error z value Pr(>|z|)   
# (Intercept)      -0.526004   0.481716  -1.092  0.27486   
# BareGround_cover  0.008704   0.007498   1.161  0.24570   
# Litter_cover      0.002740   0.005275   0.519  0.60351   
# MossLichen_cover  0.021867   0.006819   3.207  0.00134 **
# Rock_cover        0.008590   0.011885   0.723  0.46986   
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

summary(m_new)

# making a data set ####
BareGround_cover_mean <- mean(df_model_clean_new$BareGround_cover)
BareGround_cover_seq <- seq(min(df_model_clean_new$BareGround_cover), max(df_model_clean_new$BareGround_cover), length = 350)

Litter_cover_mean <- mean(df_model_clean_new$Litter_cover)
Litter_cover_seq <- seq(min(df_model_clean_new$Litter_cover), max(df_model_clean_new$Litter_cover), length = 350)

Rock_cover_mean <- mean(df_model_clean_new$Rock_cover)
Rock_cover_seq <- seq(min(df_model_clean_new$Rock_cover), max(df_model_clean_new$Rock_cover), length = 350)

MossLichen_cover_mean <- mean(df_model_clean_new$MossLichen_cover)
MossLichen_cover_seq <- seq(min(df_model_clean_new$MossLichen_cover), max(df_model_clean_new$MossLichen_cover), length = 350)

df_lichen_plot <- data.frame(BareGround_cover = BareGround_cover_mean,
  Litter_cover = Litter_cover_mean,
  Rock_cover = Rock_cover_mean,
  MossLichen_cover = MossLichen_cover_seq
)

df_rock_plot <-  data.frame(BareGround_cover = BareGround_cover_mean,
  Litter_cover = Litter_cover_mean,
  Rock_cover = Rock_cover_seq,
  MossLichen_cover = MossLichen_cover_mean
)

df_bare_plot <-  data.frame(BareGround_cover = BareGround_cover_seq,
  Litter_cover = Litter_cover_mean,
  Rock_cover = Rock_cover_mean,
  MossLichen_cover = MossLichen_cover_mean
)

df_litter_plot <-  data.frame(BareGround_cover = BareGround_cover_mean,
  Litter_cover = Litter_cover_seq,
  Rock_cover = Rock_cover_mean,
  MossLichen_cover = MossLichen_cover_mean
)

# predicted counts on response scale
pred_lichen <- predict(m_new, 
  newdata = df_lichen_plot, 
  type = "response", 
  re.form = NA # to ignore random effects
)

# 95% confidendence bands, has to be on the log scale
pred_lichen_se <- predict(m_new, 
  newdata = df_lichen_plot, 
  type = "link", 
  se.fit = TRUE, 
  re.form = NA # to ignore random effects
)

highest <- exp(pred_lichen_se$fit + (1.96 * pred_lichen_se$se.fit))
lowest <- exp(pred_lichen_se$fit - (1.96 * pred_lichen_se$se.fit))

par(mfrow = c(1, 1))

plot(MossLichen_cover_seq, pred_lichen, 
  type = "l",
  las = 1, 
  col = "skyblue",
  ylim = c(min(pred_lichen), max(pred_lichen)),
  xlab = "Moss & Lichen Cover [SD]",
  ylab = "Predicted Eucalyptus [count]",   
  main = "Effect of Moss & Lichen Cover on Eucalyptus abundance")
polygon(c(df_lichen_plot$MossLichen_cover, rev(df_lichen_plot$MossLichen_cover)),
        c(highest, rev(lowest)),
        col = rgb(0.2,0.5,0.8,0.2), border = NA)
lines(MossLichen_cover_seq, highest, col = "skyblue", lty = 2)
lines(MossLichen_cover_seq, lowest, col = "skyblue", lty = 2)

pred_rock <- predict(m_new, 
  newdata = df_rock_plot, 
  type = "response", 
  re.form = NA # to ignore random effects
)

# 95% confidendence bands, has to be on the log scale
pred_rock_se <- predict(m_new, 
  newdata = df_rock_plot, 
  type = "link", 
  se.fit = TRUE, 
  re.form = NA # to ignore random effects
)

rock_highest <- exp(pred_rock_se$fit + (1.96 * pred_rock_se$se.fit))
rock_lowest <- exp(pred_rock_se$fit - (1.96 * pred_rock_se$se.fit))

plot(Rock_cover_seq, pred_rock, 
  type = "l",
  las = 1, 
  col = "skyblue",
  ylim = c(min(rock_lowest), max(rock_highest)),
  xlab = "Rock Cover [SD]",
  ylab = "Predicted Eucalyptus [count]",   
  main = "Effect of Rock Cover on Eucalyptus abundance")
polygon(c(df_rock_plot$Rock_cover, rev(df_rock_plot$Rock_cover)),
        c(rock_highest, rev(rock_lowest)),
        col = rgb(0.2,0.5,0.8,0.2), border = NA)
lines(Rock_cover_seq, rock_highest, col = "skyblue", lty = 2)
lines(Rock_cover_seq, rock_lowest, col = "skyblue", lty = 2)

pred_litter <- predict(m_new, 
  newdata = df_litter_plot, 
  type = "response", 
  re.form = NA # to ignore random effects
)

pred_litter_se <- predict(m_new, 
  newdata = df_litter_plot, 
  type = "link", 
  se.fit = TRUE, 
  re.form = NA # to ignore random effects
)

litter_highest <- exp(pred_litter_se$fit + (1.96 * pred_litter_se$se.fit))
litter_lowest <- exp(pred_litter_se$fit - (1.96 * pred_litter_se$se.fit))

plot(Litter_cover_seq, pred_litter, 
  type = "l",
  las = 1, 
  col = "skyblue",
  ylim = c(min(litter_lowest), max(litter_highest)),
  xlab = "Litter Cover [SD]",
  ylab = "Predicted Eucalyptus [count]",   
  main = "Effect of Litter Cover on Eucalyptus abundance")
polygon(c(df_litter_plot$Litter_cover, rev(df_litter_plot$Litter_cover)),
        c(litter_highest, rev(litter_lowest)),
        col = rgb(0.2,0.5,0.8,0.2), border = NA)
lines(Litter_cover_seq, litter_highest, col = "skyblue", lty = 2)
lines(Litter_cover_seq, litter_lowest, col = "skyblue", lty = 2)

pred_bare <- predict(m_new, 
  newdata = df_bare_plot, 
  type = "response", 
  re.form = NA # to ignore random effects
)

pred_bare_se <- predict(m_new, 
  newdata = df_bare_plot, 
  type = "link", 
  se.fit = TRUE, 
  re.form = NA # to ignore random effects
)

bare_highest <- exp(pred_bare_se$fit + (1.96 * pred_bare_se$se.fit))
bare_lowest <- exp(pred_bare_se$fit - (1.96 * pred_bare_se$se.fit))

plot(BareGround_cover_seq, pred_bare, 
  type = "l",
  las = 1, 
  col = "skyblue",
  ylim = c(min(bare_lowest), max(bare_highest)),
  xlab = "Bare Cover [SD]",
  ylab = "Predicted Eucalyptus [count]",
  main = "Effect of Bare Cover on Eucalyptus abundance")
polygon(c(BareGround_cover_seq, rev(BareGround_cover_seq)),
        c(bare_highest, rev(bare_lowest)),
        col = rgb(0.2,0.5,0.8,0.2), border = NA)
lines(BareGround_cover_seq, bare_highest, col = "skyblue", lty = 2)
lines(BareGround_cover_seq, bare_lowest, col = "skyblue", lty = 2)





?scale
check_collinearity(m)
check_collinearity(m_new)
AIC(m)
AIC(m_new)
testDispersion(m_new)

deviance(m_new) / df.residual(m_new) #[1] 0.4928805

plot(fitted(m), df_model_clean$seedlings_tot)
abline(0, 1, col = "skyblue")

plot(residuals(m, type = "response"), predict(m, type = "response"))
abline(h = 0, col = "red")
#  this does not look like what i expected it to


summary(m)

layout(matrix(1:4, nrow = 2, byrow = TRUE))

par(mar = c(4, 5, 2.5, 1))

## plot 1
plot(MossLichen_cover_seq, pred_lichen, 
  type = "l",
  las = 1, 
  col = "skyblue",
  ylim = c(min(lowest), max(highest)),
  xlab = "Moss & Lichen Cover [SD]",
  ylab = "Predicted Eucalyptus [count]",   
  main = "Effect of Moss & Lichen Cover \n on Eucalyptus abundance")
polygon(c(df_lichen_plot$MossLichen_cover, rev(df_lichen_plot$MossLichen_cover)),
        c(highest, rev(lowest)),
        col = rgb(0.2,0.5,0.8,0.2), border = NA)
lines(MossLichen_cover_seq, highest, col = "skyblue", lty = 2)
lines(MossLichen_cover_seq, lowest, col = "skyblue", lty = 2)

## plot 2
plot(Rock_cover_seq, pred_rock, 
  type = "l",
  las = 1, 
  col = "skyblue",
  ylim = c(min(rock_lowest), max(rock_highest)),
  xlab = "Rock Cover [SD]",
  ylab = "Predicted Eucalyptus [count]",   
  main = "Effect of Rock Cover \n on Eucalyptus abundance")
polygon(c(df_rock_plot$Rock_cover, rev(df_rock_plot$Rock_cover)),
        c(rock_highest, rev(rock_lowest)),
        col = rgb(0.2,0.5,0.8,0.2), border = NA)
lines(Rock_cover_seq, rock_highest, col = "skyblue", lty = 2)
lines(Rock_cover_seq, rock_lowest, col = "skyblue", lty = 2)

## plot 3
plot(Litter_cover_seq, pred_litter, 
  type = "l",
  las = 1, 
  col = "skyblue",
  ylim = c(min(litter_lowest), max(litter_highest)),
  xlab = "Litter Cover [SD]",
  ylab = "Predicted Eucalyptus [count]",   
  main = "Effect of Litter Cover \n on Eucalyptus abundance")
polygon(c(df_litter_plot$Litter_cover, rev(df_litter_plot$Litter_cover)),
        c(litter_highest, rev(litter_lowest)),
        col = rgb(0.2,0.5,0.8,0.2), border = NA)
lines(Litter_cover_seq, litter_highest, col = "skyblue", lty = 2)
lines(Litter_cover_seq, litter_lowest, col = "skyblue", lty = 2)

## plot 4
plot(BareGround_cover_seq, pred_bare, 
  type = "l",
  las = 1, 
  col = "skyblue",
  ylim = c(min(bare_lowest), max(bare_highest)),
  xlab = "Bare Cover [SD]",
  ylab = "Predicted Eucalyptus [count]",
  main = "Effect of Bare Cover on \n Eucalyptus abundance")
polygon(c(BareGround_cover_seq, rev(BareGround_cover_seq)),
        c(bare_highest, rev(bare_lowest)),
        col = rgb(0.2,0.5,0.8,0.2), border = NA)
lines(BareGround_cover_seq, bare_highest, col = "skyblue", lty = 2)
lines(BareGround_cover_seq, bare_lowest, col = "skyblue", lty = 2)

layout(1)
