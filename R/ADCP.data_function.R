#### Function to load Ocean Networks Canada (ONC) ADCP data from the Nortek Time-series .mat files
#### Created by: Rylan J. Command
#### Adapted from Grant Garner's "Working with ADCP .mat files in R" (ADCP handling in R.Rmd) - I learned how to access and manipulate ONC's ADCP data from Grant's work
#### Date created: May 12, 2020
#### Last modified: May 13, 2020

#### FUNCTION DESCRIPTION ####
#### This function is designed to take multiple .mat files downloaded from ONC's Data Search portal (https://data.oceannetworks.ca/DataSearch)
#### It was intended for the Barkley Canyon Upper Slope South 2 MHz ADCP Nortek Time Series .mat files, however it could be modified to 
#### suit other data structures.
#### This function takes as an input a list of .mat files (arg 1) and the number of .mat files (arg 2).
#### From there, it acquires the "time", "range", "meanBackscatter", "u" velocity component, and "v" velocity component of the ADCP data.
#### The output is a dataframe suitable for visualization and analysis of backscatter and current velocities from 0.1 m - 1.51 m above the ADCP


#### Create one large  dataframe with time, range, backscatter, u, and v ####
get.ADCP.data <- function(files, length.files) {
  library(tidyverse)
  library(R.matlab)
  library(lubridate)
  library(reshape2)
  library(viridisLite)
  
  ## Create a progress bar
  pb <- txtProgressBar(min = 0, max = length.files, style = 3)
  
  ## Define initial vectors
  new.time <- numeric()   # define initial vector for "time"
  new.range <- numeric()    # define initial vector for "range"
  new.backscatter <- numeric()    # define initial vector for backscatter
  new.u <- numeric()    # define initial vector for "u" component of current velocity
  new.v <- numeric()    # define initial vector for "v" component of current velocity
  
  for (i in 1:length.files) {
    # load data
    ADCP.data <- readMat(files[i])
    
    # acquire times, determine number of time bins in dataset
    adcp.time <- as.vector(ADCP.data$data[[1]])   # extract the "time" variable from the ADCP data file
    adcp.time <- as.POSIXct((adcp.time - 719529)*86400,   # convert from serial datetime (used by MatLab) to POSIXct (used by R)
                            origin = "1970-01-01")
    time.bins <- length(adcp.time)    # create a variable with the number of time bins in the ADCP data file
    
    # acquire range bins, determine the number of range bins in dataset
    adcp.range <- as.vector(ADCP.data$data[[2]])    # extract the "range" variable from the ADCP data file
    range.bins <- length(adcp.range)    # create a variable with the number of range bins in the ADCP data file
    
    # melt the mean backscatter data to one column of data, position [[18]]
    adcp.back <- data.matrix(ADCP.data$data[[18]])    # extract the "meanBackscatter" variable from the ADCP data file
    adcp.back <- as.data.frame(apply(t(adcp.back), 1, rev))     # transpose the backscatter data into a dataframe
    adcp.back <- suppressMessages(melt(adcp.back))   # melt the dataframe into one long vector
    
    # acquire u and v matrices, convert to long format
    adcp.u <- data.matrix(ADCP.data$data[[3]])    # extract the "u" velocity component from the ADCP data file
    adcp.u <- as.data.frame(apply(t(adcp.u), 1, rev))     # transpose the u data into a dataframe
    adcp.u <- suppressMessages(melt(adcp.u))    # melt the dataframe into one long vector
    
    adcp.v <- data.matrix(ADCP.data$data[[4]])    # extract the "v" velocity component from the ADCP data file
    adcp.v <- as.data.frame(apply(t(adcp.v), 1, rev))     # transpose the v data into a dataframe
    adcp.v <- suppressMessages(melt(adcp.v))    # melt the dataframe into one long vector
    
    # combine data into one long dataframe: repeat each time by number of range.bins, and 
    # all range bins by number of time.bins in dataset
    rep.time <- rep(adcp.time, each = range.bins) 
    rep.range <- rep(adcp.range, times = time.bins)
    
    # append values from file[i] to the end of each vector
    new.time <- .POSIXct(c(new.time, rep.time), tz = "UTC")   # This ensures the timezone attributes are not dropped
    new.range <- c(new.range, rep.range)
    new.backscatter <- c(new.backscatter, adcp.back[, c(2)])
    new.u <- c(new.u, adcp.u[, c(2)])
    new.v <- c(new.v, adcp.v[, c(2)])
    
    # update progress bar
    setTxtProgressBar(pb, i)
  
  }
  # create a dataframe containing all of the time, range, backscatter, u, and v for all of the files in the list
  return(data.frame(time = new.time,
                    range = new.range,
                    backscatter = new.backscatter,
                    u = new.u,
                    v = new.v)
         )
}

#### FUNCTION TO GET PRESSURE DATA FROM THE ADCP ####
get.ADCP.pressure <- function(files, length.files) {
  
  library(tidyverse)
  library(R.matlab)
  library(lubridate)
  library(reshape2)
  library(viridisLite)
  
  ## Create a progress bar
  pb <- txtProgressBar(min = 0, max = length.files, style = 3)
  
  ## Define initial vectors
  new.time <- numeric()   # define initial vector for "time"
  new.pressure <- numeric()   # define intial vector for "pressure"
  new.temperature <- numeric()   # define initial vector for "temperature"
  
  for (i in 1:length.files) {
    # load data
    ADCP.data <- readMat(files[i])
    
    # acquire times, determine number of time bins in dataset
    adcp.time <- as.vector(ADCP.data$data[[1]])   # extract the "time" variable from the ADCP data file
    adcp.time <- as.POSIXct((adcp.time - 719529)*86400,   # convert from serial datetime (used by MatLab) to POSIXct (used by R)
                            origin = "1970-01-01")
    time.bins <- length(adcp.time)    # create a variable with the number of time bins in the ADCP data file
    
    # accquire pressure data
    adcp.press <- as.vector(ADCP.data$data[[12]])
    
    # acquire temperature data
    adcp.temperature <- as.vector(ADCP.data$data[[13]])
    
    # append values from file[i] to the end of each vector
    new.time <- .POSIXct(c(new.time, adcp.time), tz = "UTC")   # This ensures the timezone attributes are not dropped
    new.pressure <- c(new.pressure, adcp.press)
    new.temperature <- c(new.temperature, adcp.temperature)
    
    # update progress bar
    setTxtProgressBar(pb, i)
    
  }
  # create a dataframe containing all of the time and pressure data for all of the files in the list
  return(data.frame(Time = new.time,
                    Pressure = new.pressure,
                    Temperature = new.temperature)
  )
}

#### Function to calculate the direction and magnitude of ADCP current from u and v components ####
## Function takes u and v velocity components as input

current.direction <- function(u, v) {
  
  new.current <- numeric()
  temp <- numeric()
  dir <- numeric()
  
  for (i in 1:length(u)) {
    temp[i] <- atan(v[i] / u[i]) * (180/pi)
    new.current[i] <- sqrt((v[i])^2 + (u[i])^2)
  }
  
  for (i in 1:length(u)) {
    if (u[i] >= 0 & v[i] >= 0)
      dir[i] = abs(temp[i])
    
    else if (u[i] < 0 & v[i] >=0) {
      dir[i] = 180 - abs(temp[i])
    }
    else if (u[i] < 0 & v[i] < 0) {
      dir[i] = 180 + abs(temp[i])
    }
    else if (u[i] >= 0 & v[i] <0) {
      dir[i] = 360 - abs(temp[i])
    }
  }
  return(data.frame(Magnitude = new.current,
                    Direction = dir)
  )
}
