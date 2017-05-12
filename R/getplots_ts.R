ts_desc <- function(ts.data){
  if(class(ts.data)=="ts"){
    list(start=start(ts.data),end=end(ts.data),frequency=frequency(ts.data),
         cycle=cycle(ts.data))
  }
}

getplots_ts <- function(data){
  par(mfrow=c(2,2))
  hist(data);plot(data);plot(aggregate(data),ylab="Mean of each cycle");
  boxplot(data~cycle(data))
}

diff.plot <- function(data,diff=1,log=T){
  par(mfrow=c(1,1))
  if (log==T) plot(diff(log(data),diff))
  else plot(diff(data,diff))
}

acf.plot  <- function(data,diff=0,log=F) {
  if(log==T&&diff >= 1) acf(diff(log(data),diff))
  else if(log==T&&diff < 1) acf(log(data))
    else if(log==F&&diff >= 1) acf(diff(data),diff)
      else acf(data)

}

pacf.plot  <- function(data,diff=0,log=F) {
  if(log==T&&diff >= 1) pacf(diff(log(data),diff))
  else if(log==T&&diff < 1) pacf(log(data))
  else if(log==F&&diff >= 1) pacf(diff(data),diff)
  else pacf(data)

}

detect_tsouliers <- function(arima){
  cat("Additive outliers:\n")
  ao <- detectAO(fit.out)
  cat("\n \n")
  cat("Intelligent outliers:\n")
  io <- detectIO(fit.out)

}
