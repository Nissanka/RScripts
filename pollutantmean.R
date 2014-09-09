pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)

  data <- c()
  for (i in seq_along(id)){
    if (id[i]>=1 & id[i]<= 9){
      filename = paste(directory,"/00", id[i], ".csv", sep = "")
    }else if (id[i]>=10 & id[i]<= 99){
      filename = paste(directory,"/0", id[i], ".csv", sep = "")
    }else{
        filename = paste(directory,"/", id[i], ".csv", sep = "")
    }
    data <- rbind(data, read.csv(filename))
  }
  round(mean(data[,pollutant],0,na.rm=TRUE), digits=3)
}

