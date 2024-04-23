library(causact)

rainFlag = rbern(n = 4000, prob = 0.5)
rainTime = runif(n = 4000, min=1, max=2)
X = rainFlag * rainTime
B = runif(4000, min=2, max=3)
A = runif(4000, min=1, max=2)

C = A + B + X
D = sum(C > 6)
D/4000
