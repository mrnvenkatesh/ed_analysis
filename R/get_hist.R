get_hist <- function(data){
  data <- as.data.frame(data)
  nam <- names(data)
  print("Histogram is generated for ")
  for(i in nam){
    if(class(data[,i])=="numeric"|class(data[,i])=="integer")
    {
      num.var <- i
      print(num.var)
      print(ggplot(data,aes(data[,num.var]))+geom_histogram(bins=50,fill='red',colour='black')+
              geom_vline(aes(xintercept = mean(data[,num.var])),
                         linetype='dashed',color='blue',size=1))
      
    }
  }
}
