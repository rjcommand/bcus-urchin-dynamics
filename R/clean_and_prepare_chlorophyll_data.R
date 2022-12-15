#' Load chlorophyll-a data from netCDF and get the time stamps
#'
#' @param .path 
#' @param var_name 
#'
#' @return
#' @export
#'
#' @examples
get_chla_data <- function (.path = here::here("data/OceanColour/raw_8day/raw"), var_name = "chlor_a") {
  ## Load all files
  files <- list.files(.path, pattern = "*a.nc", full.names = TRUE)
  length_files <- length(files)

  p <- numeric()  # Create an empty vector to store rasters
  attribute <- numeric()  # Create an empty vector to store "Time-start" attribute
  attribute2 <- numeric()  # Create an empty vector to store "Time-end" attribute
  
  ## Open each netCDF file, save the metadata, get the attributes
  for (i in 1:length_files) {
    nc_data <- try(ncdf4::nc_open(files[i]))  # Open the netCDF file
    # Save the print(nc) dump to a text file
    {
      sink(paste0("data/OceanColour/metadata/", i,".8day.metadata.txt"))
      print(nc_data)
      sink()
    }
    attribute[i] <- try(ncdf4::ncatt_get(nc_data, 0, attname = "time_coverage_start")$value)  # Get the "Time-start" attribute
    attribute2[i] <- try(ncdf4::ncatt_get(nc_data, 0, attname = "time_coverage_end")$value)  # Get the "Time-end" attribute
  }
  
  ## Get a list of each file as a raster
  for (i in 1:length_files) {
    p[i] <- list(raster(files[i], varname = var_name))  # Get the rasters
  }
  x <- stack(p)  # Create a raster stack w/ each file/raster as a layer
  ## Name each layer
  names(x) <- c(as.character(seq(1, length_files)))  # Change the name of each layer to the file number
  ## Save the stack as a .nc (netCDF)
  writeRaster(x = x,
              filename = file.path(paste0("data/OceanColour"), paste0(var_name, "_out_8day.nc")),
              format = "CDF",
              overwrite = TRUE)
  
  return(list(times = cbind(attribute, attribute2),
              length_files = length_files))  # Return the Time-start and Time-end attributes
}


#' Clean the chlorophyll-data and save the data frame
#'
#' @param .path Character string of the path to the directory containing raw
#' chlorophyll-a data (netCDF format).
#' @param var_name Character string of the name of the netCDF attribute where
#' the chlorophyll-a data is stored.
#'
#' @return A data.frame with time and chlorophyll-a concentration.
#' @export
#' 
#' @details Comes from the OceanColour_data.R in msc-chapter2 directory.
#' @examples
clean_chlorophyll_data <- function(.path = here::here("data/OceanColour/raw_8day/raw"), 
                                   var_name = "chlor_a") {
  ## Get the chlorophyll a data as a raster stack .nc
  #ds <- get_chla_data(files = files, length_files = length_files)
  ds <- get_chla_data(.path = .path, var_name = var_name)
  
  #### Read in the netCDF file contents ####
  
  data <- nc_open("data/OceanColour/chlor_a_out_8day.nc")
  # Save the print(nc) dump to a text file
  {
    sink("data/OceanColour/metadata/chlor_a_out_metadata.txt")
    print(data)
    sink()
  }
  
  ## Get the latitude and longitude
  lat <- ncvar_get(data, "latitude", verbose = FALSE)
  lon <- ncvar_get(data, "longitude")
  time <- ncvar_get(data, "z")
  
  
  ## Read in the data from the chlor_a variable and verify the dimensions of the array
  chlor_a_array <- ncvar_get(data, "variable")  # store the data in a 3-dimensional array
  # dim(chlor_a_array)  # see 82 lats, 36 lons, 701 layers (time)
  
  ## What fill value was used for missing data?
  fillvalue <- ncatt_get(data, "variable", "_FillValue")
  # fillvalue  # the fill value is -3.4e+38
  
  ## All done reading the data, we can close the netCDF file
  nc_close(data)
  
  
  #### Working with the data ####
  ## Now we have the entire array of chlor_a values for 190 x 84 grid cells
  ## First, a little housekeeping
  ## Let's replace all of those fill values with the R-standard "NA"
  chlor_a_array[chlor_a_array == fillvalue$value] <- NA
  
  
  #### Plot the sum of the 8-day averages from Sept 2013 - Feb 2020 ####
  chlor_a_sum <- apply(chlor_a_array, c(1, 2), sum, na.rm = TRUE) # sum over rows and columns for each time slice
  
  r <- raster(t(chlor_a_sum), xmn = min(lon), xmx = max(lon), ymn = min(lat), ymx = max(lat), 
              crs = CRS("+proj=eqc +lat_ts=0 +lat_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +lon_0=-128.099701"))
  
  
  ## Extract the timeseries of data at Barkley Canyon Upper Slope
  
  ## Get the mean value for each layer
  y = numeric()
  for (i in 1:ds$length_files) {
    y[i] <- mean(chlor_a_array[, , i], na.rm = TRUE)
  }
  
  ## Dataframe for the 8day bins
  chl.a <- data.frame(Time_start = ds$times[, 1], Time_end = ds$times[, 2], Chl_a = y)  
  
  return(chl.a)
}


#' Prepare chlorophyll-a data for modeling (oxygen GAMM)
#'
#' @param chla_clean A \code{data.frame} with time and chlorophyll-a concentration.
#'
#' @return A \code{data.frame} of time and chlorophyll-a concentration, filtered
#' for dates matching the extent of the BCUS video data, and adding a variable
#' for year and week of year.
#' @export
#'
#' @examples
chla_model_data <- function(chla_clean) {
  chla_clean %>% 
    dplyr::mutate(WoY = lubridate::week(Time_start),
           Year = factor(lubridate::year(Time_start))) %>% 
    dplyr::filter(Time_start >= "2013-05-16" & Time_end <= "2020-02-10")
}
