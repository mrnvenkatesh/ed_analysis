

get_outlier <- function(x){

  list(stats=boxplot.stats(x)$stats,outliers=boxplot.stats(x)$out,
       indices=which(x %in% (boxplot.stats(x)$out)))
}
