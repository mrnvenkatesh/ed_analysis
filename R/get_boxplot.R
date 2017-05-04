get_boxplot <- function(data){
  data <- as.data.frame(data)
  nam <- names(data)
  for(i in nam){
    if(class(data[,i])=="numeric"|class(data[,i])=="integer")
    {
      num.var <- i

      print(ggplot(data,aes(1,data[,num.var]))+geom_boxplot(outlier.colour = 'red'))

    }
  }
}




