get.Envir.data <- function(files, length.files, envir.var = NULL) {
  
  # Load libraries
  library(tidyverse)
  library(lubridate)
  
  if (is.null(envir.var)) {
  stop("Please provide an environmental variable for 'arg 3'\n
        Choose from the following:\n
        oxygen, salinity, temperature"
        )
  }
  ########################
  ## Salinity data load ##
  ########################
  if (envir.var == "salinity") {
  
    ## Create initial empty vectors
    new.Time <- numeric()
    new.Salinity <- numeric()
    new.QC.Flag <- numeric()
    
    ## Initialize progress bar
    pb <- txtProgressBar(min = 0, max = length.files, style = 3)
    
    for (i in 1:length.files) {
      # load in data
      data <- read.csv(files[i], header = TRUE, skip = 52)
      
      # tidy the column names and remove an extra row
      data <- data %>% 
        rename("Time" = "X.Time.UTC..yyyy.mm.ddThh.mm.ss.fffZ.",
               "Salinity" = "Practical.Salinity..psu.",
               "QC.Flag" = "Practical.Salinity.QC.Flag") %>% 
        filter(Time != "## END HEADER")
      
      # convert Time variable to a date time
      data$Time <- ymd_hms(data$Time, tz = "UTC")
      
      # append values from file[i] to the end of each vector
      new.Time <- .POSIXct(c(new.Time, data$Time), tz = "UTC")
      new.Salinity <- c(new.Salinity, data$Salinity)
      new.QC.Flag <- c(new.QC.Flag, data$QC.Flag)
      
      # update progress bar
      setTxtProgressBar(pb, i)
    }
    return(data.frame(Time = new.Time,
                      Salinity = new.Salinity,
                      QC.Flag = new.QC.Flag)
    )
  }
  
  ###########################
  ## Temperature data load ##
  ###########################
  else if (envir.var == "temperature") {
    
    ## Create initial empty vectors
    new.Time <- numeric()
    new.Temperature <- numeric()
    new.QC.Flag <- numeric()
    
    ## Initialize progress bar
    pb <- txtProgressBar(min = 0, max = length.files, style = 3)
    
    for (i in 1:length.files) {
      # load in data
      data <- read.csv(files[i], header = TRUE, skip = 52)
      
      # tidy the column names and remove an extra row
      data <- data %>% 
        rename("Time" = "X.Time.UTC..yyyy.mm.ddThh.mm.ss.fffZ.",
               "Temperature" = "Temperature..C.",
               "QC.Flag" = "Temperature.QC.Flag") %>% 
        filter(Time != "## END HEADER")
      
      # convert Time variable to a date time
      data$Time <- ymd_hms(data$Time, tz = "UTC")
      
      # append values from file[i] to the end of each vector
      new.Time <- .POSIXct(c(new.Time, data$Time), tz = "UTC")
      new.Temperature <- c(new.Temperature, data$Temperature)
      new.QC.Flag <- c(new.QC.Flag, data$QC.Flag)
      
      # update progress bar
      setTxtProgressBar(pb, i)
    }
    return(data.frame(Time = new.Time,
                      Temperature = new.Temperature,
                      QC.Flag = new.QC.Flag)
    )
  }
  ######################
  ## Oxygen data load ##
  ######################
  else if (envir.var == "oxygen") {
    
    ## Create initial empty vectors
    new.Time <- numeric()
    new.Oxygen <- numeric()
    new.QC.Flag <- numeric()
    
    ## Initialize progress bar
    pb <- txtProgressBar(min = 0, max = length.files, style = 3)
    
    for (i in 1:length.files) {
      # load in data
      data <- read.csv(files[i], header = TRUE, skip = 52)
      
      # tidy the column names and remove an extra row
      data <- data %>% 
        rename("Time" = "X.Time.UTC..yyyy.mm.ddThh.mm.ss.fffZ.",
               "Oxygen" = "Oxygen.Concentration.Corrected..ml.l.",
               "QC.Flag" = "Oxygen.Concentration.Corrected.QC.Flag") %>% 
        filter(Time != "## END HEADER")
      
      # convert Time variable to a date time
      data$Time <- ymd_hms(data$Time, tz = "UTC")
      
      # append values from file[i] to the end of each vector
      new.Time <- .POSIXct(c(new.Time, data$Time), tz = "UTC")
      new.Oxygen <- c(new.Oxygen, data$Oxygen)
      new.QC.Flag <- c(new.QC.Flag, data$QC.Flag)
      
      # update progress bar
      setTxtProgressBar(pb, i)
    }
    return(data.frame(Time = new.Time,
                      Oxygen = new.Oxygen,
                      QC.Flag = new.QC.Flag)
    )
  }
}
