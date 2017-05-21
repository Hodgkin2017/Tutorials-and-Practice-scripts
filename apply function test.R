m <- matrix(data=cbind(rnorm(30, 0), rnorm(30, 2), rnorm(30, 5)), nrow=30, ncol=3)
m

apply(m, 1, mean)
apply(m, 2, mean)

apply(m, 2, function(x) length(x[x<0]))
apply(m, 2, function(x) x[x<0])
apply(m, 2, function(x) is.matrix(x))

apply(m, 2, is.vector)

apply(m, 2, length(x[x<0]))


apply(m, 2, function(x) mean(x[x>0]))

sapply(1:3, function(x) x^2)
sapply(m, function(x) x^2)
m

lapply(1:3, function(x) x^2)

sapply(1:3, function(x) x^2, simplify=F)

unlist(lapply(1:3, function(x) x^2))

sapply(1:3, function(x) mean(m[,x]))
apply(m,2,function(x) mean(x))

sapply(1:3, function(x, y) mean(y[,x]), y=m)


