complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  data <- c()
  result <- c()
  output <- c()
  a <- c()
  for (i in seq_along(id)){
    if (id[i]>=1 & id[i]<= 9){
      filename = paste(directory,"/00", id[i], ".csv", sep = "")
      data <- read.csv(filename)
      result = (data[is.na(data$nitrate)==FALSE & is.na(data$sulfate)== FALSE,])
      a = c(id[i], nrow(result))
      output = rbind(output, a)
    }else if (id[i]>=10 & id[i]<= 99){
      filename = paste(directory,"/0", id[i], ".csv", sep = "")
      data <- read.csv(filename)
      result = (data[is.na(data$nitrate)==FALSE & is.na(data$sulfate)== FALSE,])
      a = c(id[i], nrow(result))
      output = rbind(output, a)
    }else{
      filename = paste(directory,"/", id[i], ".csv", sep = "")
      data <- read.csv(filename)
      result = (data[is.na(data$nitrate)==FALSE & is.na(data$sulfate)== FALSE,])
      a = c(id[i], nrow(result))
      output = data.frame(rbind(output, a))
      
    }
    h <- data.frame(output)
    names(h)<-c("id","nobs")
  }
  print(h)
}
  
