library(WebPower)

sessionInfo()

### Conventional criteria Cohen (1988)
#small
wp.regression(n = NULL, p1 = 3, p2 = 2, f2 = 0.02, alpha = 0.05, power = 0.8)

#medium
wp.regression(n = NULL, p1 = 3, p2 = 2, f2 = 0.15, alpha = 0.05, power = 0.8)

#large
wp.regression(n = NULL, p1 = 3, p2 = 2, f2 = 0.35, alpha = 0.05, power = 0.8)

### Adjusted criteria Kenny (2018)
#small
wp.regression(n = NULL, p1 = 3, p2 = 2, f2 = 0.005, alpha = 0.05, power = 0.8) 

#medium
wp.regression(n = NULL, p1 = 3, p2 = 2, f2 = 0.010, alpha = 0.05, power = 0.8) 

#large
wp.regression(n = NULL, p1 = 3, p2 = 2, f2 = 0.025, alpha = 0.05, power = 0.8) 

