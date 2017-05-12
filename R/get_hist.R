get_hist <- function(x, interval=50){
  if(class(x)=="integer"|class(x)=="numeric"){
    print(ggplot(data.frame(x),aes(x))+geom_histogram(bins=interval,fill='red',colour='black')+
            geom_vline(aes(xintercept = mean(x)),
                       linetype='dashed',color='blue',size=1))
  }
  else if(class(x)=="factor"){
    print(ggplot(data.frame(x),aes(x))+
            geom_bar(fill='red',colour='black'))
  }
  else warning("Histogram cannot be generated for this datatype")
}
