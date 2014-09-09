corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  id <- 1:332
  out <- list()
  full_monitors <- c()
  for (i in seq_along(id)){
    if (id[i]>=1 & id[i]<= 9){
      filename = paste(directory,"/00", id[i], ".csv", sep = "")
      out[[i]] <- read.csv(filename)
    }else if (id[i]>=10 & id[i]<= 99){
      filename = paste(directory,"/0", id[i], ".csv", sep = "")
      out[[i]] <- read.csv(filename)
    }else{
      filename = paste(directory,"/", id[i], ".csv", sep = "")
      out[[i]] <- read.csv(filename)
    }
  }
  for (i in seq_along(id)){
    if ((sum(complete.cases(out[[id[i]]]["sulfate"],out[[id[i]]]["nitrate"])))>threshold){
      x <- out[[id[i]]]["sulfate"]
      y <- out[[id[i]]]["nitrate"]
      z <- cor(x, y, use = "na.or.complete", method = c("pearson", "kendall", "spearman"))
      full_monitors <- c(full_monitors, z) 
    }else{0}
  }
  full_monitors
}
 



