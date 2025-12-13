rm(list = ls())

library(dplyr)
library(glmmTMB)
library(ggplot2)
library(performance)
library(DHARMa)

df_eucalypt <- read.csv("./data/exam2023_data-2.csv")

str(df_eucalypt)

df_eucalypt$seedlings_tot <- df_eucalypt$euc_sdlgs0_50cm + 
  df_eucalypt$euc_sdlgs50cm.2m + 
  df_eucalypt$euc_sdlgs.2m

df_eucalypt$Season <- as.factor(df_eucalypt$Season)
df_eucalypt$Property <- as.factor(df_eucalypt$Property)
df_eucalypt$Landscape.position <- as.factor(df_eucalypt$Landscape.position)

model_cols <- c("seedlings_tot",
  "Season",
  "Property",
  "ExoticAnnualGrass_cover",
  "NativePerennialGrass_cover",
  "Euc_canopy_cover",
  "Distance_to_Eucalypt_canopy.m.",
  "annual_precipitation",
  "precipitation_warmest_quarter",
  "precipitation_coldest_quarter",
  "MrVBF",
  "SRad_Jan",
  "SRad_Jul",
  "Landscape.position")
# 

model_cols_new <- c("seedlings_tot",
  "Season",
  "Property",
  "BareGround_cover",
  "Litter_cover",
  "MossLichen_cover",
  "Rock_cover",
  "annual_precipitation",
  "Distance_to_Eucalypt_canopy.m.",
  "MrVBF",
  "Landscape.position")
#

df_model <- df_eucalypt[, model_cols]
df_model_new <- df_eucalypt[, model_cols_new]

hist(df_eucalypt$seedlings_tot)

m <- glmmTMB(seedlings_tot ~ ExoticAnnualGrass_cover + NativePerennialGrass_cover + Euc_canopy_cover + Distance_to_Eucalypt_canopy.m. + annual_precipitation + precipitation_warmest_quarter + precipitation_coldest_quarter + Landscape.position + MrVBF + SRad_Jan + SRad_Jul + (1 | Season) + (1 | Property), poisson, data = df_model) # model not happy with NA values

na_rows <- which(!complete.cases(df_model))
cat("rows", na_rows, "will be removed, in total:", length(na_rows), "rows")
# 5 rows are fine, lets remove them
df_model_clean <- na.omit(df_model)
# seem to have removed the whole rows, good!


na_rows <- which(!complete.cases(df_model_new))
cat("rows", na_rows, "will be removed, in total:", length(na_rows), "rows")
df_model_clean_new <- na.omit(df_model_new)



# summary stats ####
m <- glmmTMB(seedlings_tot ~ ExoticAnnualGrass_cover + NativePerennialGrass_cover + Euc_canopy_cover + Distance_to_Eucalypt_canopy.m. + annual_precipitation + precipitation_warmest_quarter + precipitation_coldest_quarter + Landscape.position + MrVBF + SRad_Jan + SRad_Jul + Season + (1 | Property), "nbinom2", data = df_model_clean)
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

plot(df_eucalypt$Landscape.position, df_eucalypt$seedlings_tot)




# summary stats ####
m <- glmmTMB(seedlings_tot ~ 
  Euc_canopy_cover + 
  Distance_to_Eucalypt_canopy.m. + 
  annual_precipitation + 
  Landscape.position + 
  MrVBF + 
  SRad_Jan + 
  SRad_Jul + 
  Season + 
  (1 | Property), 
"nbinom2", data = df_model_clean)
#  Family: nbinom2  ( log )
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
  (1 | Property) + 
  (1 | Season),
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
Litter_cover_mean <- mean(df_model_clean_new$Litter_cover)
Rock_cover_mean <- mean(df_model_clean_new$Rock_cover)
MossLichen_cover_seq <- seq(min(df_model_clean_new$MossLichen_cover), max(df_model_clean_new$MossLichen_cover), length = 346)

df_lichen_plot <- data.frame(BareGround_cover = BareGround_cover_mean,
  Litter_cover = Litter_cover_mean,
  Rock_cover = Rock_cover_mean,
  MossLichen_cover = MossLichen_cover_seq,
  Property = mean(df_model_clean_new$Property),
  Season = mean(df_model_clean_new$Season)
)

# predicted counts on response scale
pred_lichen <- predict(m_new, newdata=df_lichen_plot, type="response")
print(pred_lichen)

# 95% confidendence bands, has to be on the log scale
pred_lichen_se <- predict(m_new, newdata = df_lichen_plot, type = "link", se.fit = TRUE)

highest <- exp(pred_lichen_se$fit + (1.96 * pred_lichen_se$se.fit))
lowest <- exp(pred_lichen_se$fit - (1.96 * pred_lichen_se$se.fit))

plot(MossLichen_cover_seq, pred_lichen, 
  type="l",
  las = 1, 
  col="skyblue",
  # ylim = c(0,400),
  xlab="MossLichen_cover_seq",
  ylab="Predicted Eucalyptus count",   
  main="Effect of MossLichen_cover_seq on Eucalyptus abundance",)
lines(MossLichen_cover_seq, highest, col="skyblue", lty=2)
lines(MossLichen_cover_seq, lowest, col="skyblue", lty=2)

?scale
check_collinearity(m)
check_collinearity(m_new)
AIC(m)
AIC(m_new)
testDispersion(m_new)

deviance(m) / df.residual(m) #[1] 0.5420553
plot(fitted(m), df_model_clean$seedlings_tot)
abline(0, 1, col = "skyblue")

plot(residuals(m, type = "response"), predict(m, type = "response"))
abline(h = 0, col = "red")
# this does not look like what i expected it to




summary(m)



library(dplyr)

mean_var <- df_model_clean_new %>%
  group_by(Property) %>%              # or Season, or any grouping, or none
  summarise(
    mean_count = mean(seedlings_tot),
    var_count  = var(seedlings_tot)
  )

plot(log(mean_var$mean_count),
     log(mean_var$var_count),
    #  type = "l",
     xlab = "log(mean)",
     ylab = "log(variance)",
     main = "Log–log mean–variance plot")

plot(mean_var$mean_count,
     mean_var$var_count,
    #  type = "l",
     xlab = "log(mean)",
     ylab = "log(variance)",
     main = "Log–log mean–variance plot")
