data <- read.csv("D:\\nonparamtric-statistics\\homework2\\mobile-phone-activity\\sms-call-internet-mi-2013-11-01.csv")
# fill na
for(i in 1:ncol(data)){
  data[is.na(data[,i]), i] <- mean(data[,i], na.rm = TRUE)
}

phone.calls.times = as.vector(table(data$CellID))
each.call.duration = as.array(data$callout, dim =c (length(data$callout, 1)))[1:10000]
total.call.durant = apply(each.call.duration, MARGIN = 1, FUN = function(x) mean(x * phone.calls.times))