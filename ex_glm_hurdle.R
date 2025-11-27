# In biology we often have count data with many zeros. This is one of the causes of overdispersion as discussed above. Another way to deal with this is to split the analysis into two independent components, where the first models the zeros, and the second models the counts given that at least 1 entity was observed.

x <- rnorm(200, 10, 3)
eta <- -2 + .2*x
y <- floor(exp(eta + rnbinom(200, 1, mu=.8)))

y1 <- ((y>1)*1)
# TRUE * 1 = 1

plot(y1)
m1 = glm(y1~x, family="binomial" (link="logit"))

y2 = y
y2[which(y==0)] = NA
m2 = glm(y2~x, family="poisson", na=na.exclude)