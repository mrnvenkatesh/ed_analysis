#' Imputes outliers
#'
#' @param x A numeric vector
#' @param method Method to impute the outliers. Methods that can be used are 'whisker','mean' and 'median'.
#' @examples
#' impute_outliers(mpg$hwy,method = 'whisker')
#' impute_outliers(mpg$hwy,method = 'mean')


impute_outliers <- function(x,method = 'whisker'){
  
  
  if(length(boxplot.stats(x)$out)>0){
    out <- boxplot.stats(x)$out
    max.whisker <- boxplot.stats(x)$stats[5]
    min.whisker <- boxplot.stats(x)$stats[1]
    out.index <- which(x %in% (boxplot.stats(x)$out))
    if(method=='mean'){
      mean=mean(x)
      x[out.index] <- mean
      #assign('imputed',x,envir=.GlobalEnv)
      
      cat(" outliers: ", out,"\n","Replaced by: ",mean,"\n")
      return(list(imputed=x))
    }
    else if(method=="median"){
      med=median(x)
      x[out.index] <- med
      cat(" outliers: ", out,"\n","Replaced by: ",med,"\n")
      return(list(imputed=x))
    }
      else{
        min.outlier=F
        if(length(boxplot.stats(x)$out[boxplot.stats(x)$out<min.whisker])>0){
          
         less.vals <- x[x<min.whisker]
         out.min.index <- which(x %in% less.vals)
         cat(" outliers: ", x[out.min.index],"\n","Replaced by: ",min.whisker,"\n")
         x[out.min.index] <- min.whisker
         min.outlier=T
         
         list(imputed=x)
        }
        if(length(boxplot.stats(x)$out[boxplot.stats(x)$out>max.whisker])>0){
          
          #out.index <- out.index-out.min.index
          #print(out.index)
          #print(out.index[-out.min.index])
          if(min.outlier){
            cat(" outliers: ", x[out.index[-out.min.index]],"\n","Replaced by: ",max.whisker,"\n")
            x[out.index[-out.min.index]] <- max.whisker
          }
          else{
           
            x[out.index] <- max.whisker
            cat(" outliers: ", out,"\n","Replaced by: ",max.whisker,"\n")
          }

          list(imputed=x)
        }

      }
    }

  else
    print("There are no outliers")
}

