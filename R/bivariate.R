bivariate <- function(var1,var2){
  if((class(var1)=='numeric'|class(var1)=='integer')&&(class(var2)=='numeric'|class(var2)=='integer')){
    dat <- data.frame(var1,var2)

    print(ggplot(dat,aes(var1,var2))+geom_point())
    
    list(correlation=cor(var1,var2),covariance=cov(var1,var2))
  }

  else CrossTable(var1,var2)
}

