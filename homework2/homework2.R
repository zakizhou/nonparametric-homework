run.times = 200


# constant parameters for simulating
n.1 = 1000
n.2 = 1500
rate.1 = 1 / 2
rate.2 = 1 / 3

# functions for simulating
Simulate = function(n.1, n.2, rate.1, rate.2) {
  Xs = rexp(n = n.1, rate = rate.1)
  Ys = rexp(n = n.2, rate = rate.2)
  
  # change vector to array to avoid for loop and make use of broadcast
  # otherwise speed will be very low
  Xs.v = array(Xs, dim = c(n.1, 1))
  Ys.v = array(Ys, dim = c(n.2, 1))
  
  # upper triangle sum of X.sub
  X.sub = apply(Xs.v, MARGIN = 1, FUN = function(x) x - Xs)
  X.sub.square = X.sub ** 2
  X.sub.upper.tri.indictor = upper.tri(X.sub.square)
  X.sub.upper.tri.sum = sum(X.sub.square * X.sub.upper.tri.indictor)
  
  # upper triangle sum of X.mul
  X.mul = apply(Xs.v, MARGIN = 1, FUN = function(x) x * Xs)
  X.mul.upper.tri.indictor = upper.tri(X.mul)
  X.mul.upper.tri.sum = sum(X.mul * X.mul.upper.tri.indictor)
  
  # upper triangle sum of Y.sub
  Y.sub = apply(Ys.v, MARGIN = 1, FUN = function(y) y - Ys)
  Y.sub.square = Y.sub ** 2
  Y.sub.upper.tri.indictor = upper.tri(Y.sub.square)
  Y.sub.upper.tri.sum = sum(Y.sub.square * Y.sub.upper.tri.indictor)
  
  # upper triangle sum of Y.mul
  Y.mul = apply(Ys.v, MARGIN = 1, FUN = function(y) y * Ys)
  Y.mul.upper.tri.indictor = upper.tri(Y.mul)
  Y.mul.upper.tri.sum = sum(Y.mul * Y.mul.upper.tri.indictor)
  
  sigma.square.hat.0.1 = 1 / 2 * X.sub.upper.tri.sum / dim(combn(n.1, 2))[2] * Y.mul.upper.tri.sum / dim(combn(n.2, 2))[2]
  sigma.square.hat.1.0 = 1 / 2 * X.mul.upper.tri.sum / dim(combn(n.1, 2))[2] * Y.sub.upper.tri.sum / dim(combn(n.2, 2))[2]
  
  p =  n.1 / (n.1 + n.2)
  
  sigma.square.hat = 1 / p * sigma.square.hat.0.1 + 1 / (1 - p) * sigma.square.hat.1.0
  sigma.hat = sqrt(sigma.square.hat)
  
  x.mul.y = apply(Xs.v, MARGIN = 1, FUN = function(x) x * Ys)
  x.mul.y.mean = mean(x.mul.y)
  return(c(x.mul.y.mean, sigma.hat))
}


objects = c()
for(i in 1:200) {
  print(i)
  r = Simulate(n.1, n.2, rate.1, rate.2)
  objects = cbind(objects, r, deparse.level = 0)
}

numerator = sqrt(n.1 + n.2) * (objects[1,] - 1 / (rate.1 * rate.2))
denominator = objects[2,]

finals = numerator / denominator


plot(density(finals))

x = seq(-3, 3, 0.02)
y = dnorm(x)
lines(x, y, col="red")

