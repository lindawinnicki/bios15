# EXERCISE: Use non-parametric bootstrapping to derive a 95% confidence interval for the CV of a variable.
# Start by writing a function that computes the CV for a variable (see the Appendix for a brief introduction
# to writing functions in R). Then, simulate a random variable and write a loop that samples many times from
# this variable and computes the CV.
source("./functions/f_CV.R") #importing the CV function

rvar <- round(rnorm(100, 10, 4), 1)  #the random variable (vector)

result <- NULL #will be used in for loop
for (i in 1:1000){
  sample <- sample(rvar, replace = TRUE) #bootstrap variable with replacement, i.e. we put it back into the "bag" after having pulled it out once
  result[i] <- CV(sample) #computing CV
}

# result now carries the sampling distribution of the coefficient variance

hist(result, las = 1, main = "") #histogram

quantile(result, c(0.025, 0.975)) #0.3663145 0.4673050

qnorm(c(0.025, 0.975)) #-1.959964  1.959964

#theoretical 95% confidence interval
mean(result) #0.4167312
sd(result) #0.02586803
(0.02586803 * 1.959964) #0.05070041
(0.4167312+0.05070041) #  -> 0.4674316  vs. 0.4673050
(0.4167312-0.05070041) #  ->  0.3660308 vs. 0.3663145

#very close!