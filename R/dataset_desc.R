dataset_desc <- function(dataset){
  dataset <- data.frame(dataset)
  variables <- names(dataset)
  datatypes <- sapply(dataset,class)
  total.recs <- nrow(dataset)
  dimension <- dim(dataset)
  NAs <- sum(is.na(dataset))
  summary <- summary(dataset)
  
  list(variables=variables,datatypes=datatypes,total.recs=total.recs,dimension=dimension
       ,NAs=NAs,summary=summary)
  
}

