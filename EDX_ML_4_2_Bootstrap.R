n <- 10^6
income <- 10^(rnorm(n, log10(45000), log10(3)))
qplot(log10(income), bins=30, color=I("black"))

m <- median(income)
m 

set.seed(1)