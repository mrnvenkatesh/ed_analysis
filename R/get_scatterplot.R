get_scatterplot <- function(data){
  data <- as.data.frame(data)
  nam <- names(data)
  
  for(i in nam){
    if(class(data[,i])=="numeric"|class(data[,i])=="integer"){

        print(ggplot(data,aes(data[,i]))+
                geom_point())
    }
  }
}
