library(ggplot2)

seeds <- read.csv("./data/dormancy.csv")

head(seeds)
# • pop = Population ID
# • mother = Maternal plant ID
# • crossID = Unique cross identifier
# • blocktray = Sowing tray (experimental block)
# • timetosowing = Time in days from seed dispersal to watering
# • MCseed = Population-mean-centered seed mass in mg
# • nseed = Number of seeds sown
# • germ2 = Proportion of seeds germinated

# ggplot(seeds, 
#   aes(
#     pop, 
#     timetosowing, 
#     group = germ2,
#     fill = germ2
#   )) +
#   geom_boxplot()

ggplot(seeds, aes(pop, timetosowing)) +
  geom_boxplot() +
  facet_wrap(~ germ2)


dplyr::count(seeds, pop, germ2)

unique(seeds$pop)

subdat_CC = seeds[seeds$pop=="CC",]
germ_CC = subdat_CC$germ2 * subdat_CC$nseed #Successes
notgerm_CC = subdat_CC$nseed - germ_CC #Failures

subdat_LM = seeds[seeds$pop=="LM",]
germ_LM = subdat_LM$germ2 * subdat_LM$nseed #Successes
notgerm_LM = subdat_LM$nseed - germ_LM #Failures

subdat_PM = seeds[seeds$pop=="PM",]
germ_PM = subdat_PM$germ2 * subdat_PM$nseed #Successes
notgerm_PM = subdat_PM$nseed - germ_PM #Failures

subdat_T = seeds[seeds$pop=="PM",]
germ_T = subdat_T$germ2 * subdat_T$nseed #Successes
notgerm_T = subdat_T$nseed - germ_T #Failures

# seeds$nseed
# seeds$germ2

# Analyse the data to estimate the pattern of germination success in response to variation in the duration of after-ripening. Are the patterns similar in different populations? Are there other factors affecting germination success? Produce relevant summary statistics, parameter estimates, and graphs.


# CC ####
ggplot(subdat_CC,
  aes(germ_CC, timetosowing)
) + 
  geom_jitter()

mod1_CC = glm(cbind(germ_CC, notgerm_CC)~ timetosowing, "binomial", data=subdat_CC)

summary(mod1_CC)

mod2_CC = glm(germ2 ~ timetosowing, "binomial", weights=nseed, data=subdat_CC)

logLik(mod1_CC) == logLik(mod2_CC)

# LM ####
ggplot(subdat_LM,
  aes(germ_LM, timetosowing)
) + 
  geom_jitter()

mod1_LM = glm(cbind(germ_LM, notgerm_LM)~ timetosowing, "binomial", data=subdat_LM)

summary(mod1_LM)

mod2_LM = glm(germ2 ~ timetosowing, "binomial", weights=nseed, data=subdat_LM)

logLik(mod1_LM) == logLik(mod2_LM)

# PM ####
ggplot(subdat_PM,
  aes(germ_PM, timetosowing)
) + 
  geom_jitter()

mod1_PM = glm(cbind(germ_PM, notgerm_PM)~ timetosowing, "binomial", data=subdat_PM)

summary(mod1_PM)

mod2_PM = glm(germ2 ~ timetosowing, "binomial", weights=nseed, data=subdat_PM)

logLik(mod1_PM) == logLik(mod2_PM)

# T ####
ggplot(subdat_T,
  aes(germ_T, timetosowing)
) + 
  geom_jitter()

mod1_T = glm(cbind(germ_T, notgerm_T)~ timetosowing, "binomial", data=subdat_T)

summary(mod1_T)

mod2_T = glm(germ2 ~ timetosowing, "binomial", weights=nseed, data=subdat_T)

summary(mod2_T)

logLik(mod1_T) == logLik(mod2_T)


#Can you use the fitted models to estimate the duration of after-ripening required for the expected germination rate to be 
# 0.5?
