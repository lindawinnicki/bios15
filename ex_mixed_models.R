set.seed(145)
x1 = rnorm(200, 10, 2) # mean 10 sd 2

groupmeans = rep(rnorm(10, 20, 4), each=20) # mean 20, sd 4
groupID = as.factor(rep(paste0("Group", 1:10), each=20))
y = 2 + 1.5*x1 + groupmeans + rnorm(200, 0, 2)

par(mfrow = c(1,1))
plot(x1, y, col=as.numeric(groupID), las=1)
# we could partly be interested in quantifying the contribution of group differences to the overall variance
# partly in accounting for group membership when estimating the relationship between y and x1.

