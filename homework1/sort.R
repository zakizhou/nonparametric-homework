insertion.sort <- function(array){
  size <- length(array)
  for(i in 2:size){
    for(j in (i-1):1){
      if(array[j] > array[j + 1]){
        temp <- array[j]
        array[j] <- array[j + 1]
        array[j + 1] <- temp
      }
    }
  }
  return(array)
}


bubble.sort <- function(array){
  size <- length(array)
  for(i in size:2){
    for(j in 1:(i-1)){
      if(array[j] > array[j + 1]){
        temp <- array[j]
        array[j] <- array[j + 1]
        array[j + 1] <- temp
      }
    }
  }
  return(array)
}

array <- rnorm(10000)

# show correct results
insertion.sort(rnorm(10))
bubble.sort(rnorm(10))

# record run time
RecordRunTime <- function(sort.algorithm, run.times) {
  length.run.times <- length(run.times)
  record <- vector(length = length.run.times)
  for(i in 1:length.run.times) {
    start.time <- Sys.time()
    sorted <- sort.algorithm(rnorm(run.times[i]))
    end.time <- Sys.time()
    duration <- as.vector(end.time - start.time)[1]
    record[i] <- duration
  }
  return(record)
}

run.times.lenth <- 10
run.times <- seq(1, run.times.lenth) * 1000
sort.algorithms <- c(sort, bubble.sort, insertion.sort)
num.sort.algorthms <- length(sort.algorithms)
records <- matrix(nrow = num.sort.algorthms, ncol = run.times.lenth)

for(i in 1:length(sort.algorithms)) {
  sort.algorithm <- sort.algorithms[[i]]
  records[i,] <- RecordRunTime(sort.algorithm, run.times)
}

matplot(t(records), type = c("b"),pch=1,col = 1:3) 
legend("topleft", legend = c("sort", "bubble-sort", "insertion-sort"), col=1:3, pch=1) 

array <- rnorm(10000)
start.time <- Sys.time()
sorted <- insertion.sort(array)
end.time <- Sys.time()
print(end.time - start.time)

start.time <- Sys.time()
sorted <- sort(array)
end.time <- Sys.time()
print(end.time - start.time)