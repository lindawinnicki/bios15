set.seed(100)
groups = as.factor(rep(c("Low", "Medium", "High"), each = 50)) # y-values
x = c(rnorm(50, 10, 3), rnorm(50, 13, 3), rnorm(50, 14, 3)) # norm. distr. x-value
plot(groups, x, las=1, xlab="", ylab="Body size (g)")

normal_data <- data.frame(groups, x)

print(normal_data)

mean(normal_data$)
tapply(normal_data$x, normal_data$groups, mean)

plot(model)

# plot(x, groups, las=1, xlab="", ylab="Body size (g)")

boxplot(x ~ groups, xlab = "", ylab = "Body size [g]", outline = FALSE)
points(jitter(as.numeric(groups), amount = 0.1), x, pch = 16, col = rgb(0, 0, 0, 0.2), 
# method = "jitter"
)

model = lm(x ~ groups)
anova(model)
SS_T = 1200.43 + 319.97 # total sum of squares

var(normal_data$x)  #[1] 10.20403
SS_T/149 # also gives the total variance ([1] 10.20403)

sum(x)

# SS_groups <- 319.97
# SS_residuals <- 1200.43

# pie(c(SS_groups, SS_residuals), 
#     labels = c("Between groups (21%)", "Within groups (79%)"), 
#     col = c("skyblue", "lightgray"), main="Variance Partitioning")
