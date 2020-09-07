directory = "/home/varshan/Documents/Online Learning/R language/data/specdata"
pm <- function(directory, pollutant, d = 1:332){
  vals <- c()
  file_names <- list.files(path = directory, pattern = "*.csv")
  
  for (i in d){
    file_name = sprintf("%03d.csv",i)
    filepath <- paste(directory,file_name, sep = '/')
    data <- read.csv(filepath)
    d1 <- data[,pollutant]
    d1 <- d1[!is.na(d1)]
    vals <- c(vals,d1)
  }
  round(mean(vals),3)
}
pm(directory, "sulfate", 1:10)
pm(directory,  "nitrate", 70:72)
pm(directory, "nitrate", 23)
pm(directory, "sulfate", 34)


complete <- function(directory, d = 1:332){

  ids = c()
  counts = c()
  
  file_names <- list.files(path = directory, pattern = "*.csv")
  
  for (i in d){
    file_name = sprintf("%03d.csv",i)
    filepath <- paste(directory,file_name, sep = '/')
    data <- read.csv(filepath)
    ids <- c(ids,i)
    completeCases <- data[complete.cases(data),]
    counts <- c(counts,nrow(completeCases))
  }
  data.frame(id = ids, nobs = counts)
}

complete(directory,c(6, 10, 20, 34, 100, 200, 310))
complete(directory, c(2, 4, 8, 10, 12))
complete(directory, 30:25)
complete(directory,54)


corr <- function(directory, threshold=0){
  
  completes <- complete(directory, 1:332)
  completes <- subset(completes, nobs > threshold )
  correlations <- vector()
  
  for (i in completes$id){
    file_name = sprintf("%03d.csv",i)
    filepath <- paste(directory,file_name, sep = '/')
    data <- read.csv(filepath)
    completeCases <- data[complete.cases(data),]
    count <- nrow(completeCases)
    
    if( count >= threshold ) {
      
      correlations <- c(correlations, cor(completeCases$nitrate, completeCases$sulfate))
    }
  
  }
    correlations
}

cr <- corr(directory, 150)
head(cr)





