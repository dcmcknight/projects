x1 = rnorm(100)
x2 = rnorm(100)
x3 = rnorm(100)

t =  data.frame(a = c(x1), b = c(x1+x2), c = c(x1+x2+x3))

plot(t)