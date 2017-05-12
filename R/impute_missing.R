impute_missing <- function(x,m=1,maxit = 50, seed = 10,printFlag = F){
  imputed <- mice(x,m=m,maxit=maxit , method=method , seed=seed,printFlag=printFlag)
  imputed$imp
}

