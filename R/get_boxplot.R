

get_boxplot <- function(var1,var2=NA){
  if(!is.na(var2) && class(var2)=="factor")
    print(ggplot(data.frame(var1,var2),aes(var2,var1,fill=var2))+
            geom_boxplot(outlier.colour = 'red'))
  else if(class(var1)=="numeric"|class(var1)=="integer"){
    print(ggplot(data.frame(var1),aes(1,var1))+
            geom_boxplot(outlier.colour = 'red'))
  }
  else stop("Please check the datatype of the variable passed. \n When passing two variables, var1 and var2 must be continous and discrete respectively \n When a single variable is passed make sure it is a continous variable")
}

