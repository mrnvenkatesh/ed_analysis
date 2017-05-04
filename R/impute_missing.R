impute_missing <- function(x,m=1,maxit = 500, method = 'pmm', seed = NA,printFlag = F){
  imputed <- mice(x,m,maxit , method , seed,printFlag)
  imputed$imp
}

