get_hist <- function(data,interval=50){
  data <- as.data.frame(data)
  nam <- names(data)
  print("Histogram is generated for ")
  for(i in nam){
    if(class(data[,i])=="numeric"|class(data[,i])=="integer"|class(data[,i])=="factor"){
      if(class(data[,i])=="factor"){
        num.var <- i
        print(num.var)
        print(ggplot(data,aes(data[,num.var]))+
                geom_bar(fill='red',colour='black'))
      }
      else{
        
      
      num.var <- i
      print(num.var)
      print(ggplot(data,aes(data[,num.var]))+
              geom_histogram(bins=interval,fill='red',colour='black')+
              geom_vline(aes(xintercept = mean(data[,num.var])),
                         linetype='dashed',color='blue',size=1))
      }
    }
    
  }
}




