set.seed(12)
x1 = rnorm(200, 10, 3)

group = as.factor(sample(c("A", "B"), 200, replace = TRUE))
?sample # 200 = size, with replacement
table(group)
table(group) / 200

y = 0.5*x1 + rnorm(200, 0, 4)

y[group=="A"] = y[group=="A"] + rnorm(length(y[group=="A"]), 2, 1)

m1 = lm(y ~ x1 * group)
m2 = lm(y ~ x1 + group)
m3 = lm(y ~ x1)
m4 = lm(y ~ group)
m5 = lm(y ~ 1) #null ?

mlist <- list(m1, m2, m3, m4, m5)
AICTab <- AIC(m1, m2, m3, m4, m5)
AICTab$logLik = unlist(lapply(mlist, logLik)) # this is the log likelihood
AICTab <- AICTab[order(AICTab$AIC, decreasing = FALSE),]
AICTab$delta = round(AICTab$AIC - min(AICTab$AIC), 2)
lh <- exp(-0.5*AICTab$delta) # this is the nominator (täljare) of the weigth formula
AICTab$w = round(lh/sum(lh), 2) # and now the nämnare, make it into a new column for weigths

print(AICTab)

# Pick any of the datasets we have worked with in this course that includes more than one candidate predictor variable. Use AIC to perform model selection, produce a neat summary table with an informative legend, and interpret the results (biologically and statistically).
# I will use the bees data
bees <- read.csv("./data/Eulaema.csv")

str(bees)
# we already know to use a negative binomial dist, but lets imagine we didnt know..
# and lets also compare the same predictors we already used in the exercise 

m1 <- glm(Eulaema_nigrita ~ MAT + MAP + Tseason + Pseason + offset(effort), data = bees, family = "poisson")
m2 <- glm.nb(Eulaema_nigrita ~ MAT + MAP + Tseason + Pseason + offset(effort), data = bees)
# lets try to not offset effort, but use it as a predictor
m3 <- glm(Eulaema_nigrita ~ MAT + MAP + Tseason + Pseason + effort, data = bees, family = "poisson")
m4 <- glm.nb(Eulaema_nigrita ~ MAT + MAP + Tseason + Pseason + effort, data = bees)
m_null_p <- glm(Eulaema_nigrita ~ 1, data = bees, family = "poisson")
m_null_nb <- glm.nb(Eulaema_nigrita ~ 1, data = bees)

# now lets make a table as before
mlist <- list(m1, m2, m3, m4, m_null_p, m_null_nb)
AICTab <- AIC(m1, m2, m3, m4,  m_null_p, m_null_nb)
AICTab$logLik <- unlist(lapply(mlist, logLik))
AICTab <- AICTab[order(AICTab$AIC, decreasing = FALSE),]
AICTab$delta = round(AICTab$AIC - min(AICTab$AIC), 2)
lh <- exp(-0.5 * AICTab$delta)
AICTab$w <- round(lh/sum(lh), 2)
AICTab

# oop....... shouldnt have offseted the effort, instead it should have been used as a predictor