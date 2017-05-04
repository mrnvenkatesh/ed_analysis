impute_outliers <- function(x,method = 'whisker'){
  if(length(boxplot.stats(x)$out)>0){
    out <- boxplot.stats(x)$out
    max.whisker <- boxplot.stats(x)$stats[5]
    out.index <- which(x %in% (boxplot.stats(x)$out))
    if(method=='mean'){
      mean=mean(x)
      x[out.index] <- mean
      cat(" outliers: ", out,"\n","Replaced by: ",mean)
    }
    else if(method=="median"){
      med=median(x)
      x[out.index] <- med
      cat(" outliers: ", out,"\n","Replaced by: ",med)
    }
      else{
        x[out.index] <- max.whisker
        cat(" outliers: ", out,"\n","Replaced by: ",max.whisker)
      }
    }

  else
    print("There are no outliers")
}
