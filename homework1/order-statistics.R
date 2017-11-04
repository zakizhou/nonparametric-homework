require(graphics)

n = 10000
run.times = 1000
total.times <- n * run.times
norm.samples.vector <- rnorm(total.times)
norm.samples.matrix <- matrix(norm.samples.vector, nrow = n)
norm.samples.max <- apply(norm.samples.matrix, 2, max)
b.n <- qnorm(1-1/n)
a.n <- b.n
gumbel.samples = a.n * (norm.samples.max - b.n)

plot(density(gumbel.samples))

CalculateGulbelDesity <- function(x, mu = 0, beta = 1) {
  return(1/beta * exp(-(x + exp(-(x - mu)/beta))))
}
x.vector <- seq(from = -2, to = 5, length.out = 100)
y.vector = CalculateGulbelDesity(x.vector)
lines(x.vector, y.vector, col="green")


require(graphics)
n = 10000
run.times = 1000
total.times <- n * run.times
norm.samples.vector <- rnorm(total.times)
norm.samples.matrix <- matrix(norm.samples.vector, nrow = n)
norm.samples.median <- apply(norm.samples.matrix, 2, median)


norm.samples.median <- sqrt(n) * (norm.samples.median - qnorm(1/2))
plot(density(norm.samples.median))
sd <- 1 / (2 * dnorm(qnorm(1/2)))
x.vector <- seq(from = -4, to = 4, length.out = 100)
y.vector = dnorm(x.vector, sd=sd)
lines(x.vector, y.vector, col="green")