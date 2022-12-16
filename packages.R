## library() calls go here
library(conflicted)
library(dotenv)
library(targets)
library(tarchetypes)
library(tidyverse)
library(lubridate)  # To work with date-time formats
library(mgcv)  # Fit and evaluate GAMs
library(gratia)  # Tidy GAM visualization
library(data.table)
library(rmarkdown)
library(kableExtra)
library(tidymv)
library(mgcViz)
library(ncdf4)  # Reading netCDF files (for chlorophyll-a data)
library(raster)  # Working with raster data (for chlorophyll-a data and bathymetry)

library(PMCMRplus)  # For non-parametric multiple comparisons
library(ggspatial)  # For scale bar and north arrow on maps
library(mapdata)    # For North America base map
library(maps)       # For plotting maps

library(terra)  # For rasters
library(basemaps)  # For plotting basemaps (Figure S1)