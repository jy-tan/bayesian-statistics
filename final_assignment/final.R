set.seed(0183140)

n <- 5
sy <- 18
a <- 2
b <- 2
S <- 100

theta.samples <- runif(S)


logf <- function(theta, sy, n, a, b) { 
  (n+a-1)*log(theta) + (b-1+sy)*log(1-theta)
} 

logw <- logf(theta.samples, sy = 18, n = 5, a = 2, b = 2) - 
  dunif(theta.samples, log=TRUE) 
w <- exp(logw - max(logw))
W <- w/sum(w) 
(est <- sum(W*theta.samples)) 
(se <- sqrt(sum((W*(theta.samples - est))^2)))
  
mat <- matrix(c(17.4066,393.5803,
                393.5803,9017.527),
              nrow = 2, byrow = TRUE)
solve(mat)

# find critical value for (alpha = 0.05), df = (1,22)
qf(0.95, df1 = 1, df2 = 22)

# find p-value for F statistic with df = (1,22)
pf(4.30095, df1 = 1, df2 = 22, lower.tail = FALSE)

# find t-statistic for (alpha = 0.05), df = 22
qt(0.975, df = 22)

# find p-value for t-statistic with df = 22
pt(2.073873, df = 22, lower.tail = FALSE)*2
# this has already accounted for both tails
# directly compare this with alpha

