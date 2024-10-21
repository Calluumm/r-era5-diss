install.packages("ncdf4") # For reading the database
install.packages("raster") # Geographic data analysis
install.packages("sp") # Spatial data library
install.packages("RNetCDF") # Other NetCDF reader
install.packages("pracma") # Mathematical dependency for EOF functions
install.packages("pbapply") #so i can tell if things are actually happening
# Can use rgdal if you find a supported fork of it but not recommended
install.packages("RNCEP") #to use the online version of the dataset
install.packages("lubridate") #date formatting
install.packages("tidyverse")
install.packages("sf") #use GIS shapefiles to plot to 
install.packages("ggplot2") #plot to a map
install.packages("RColorBrewer") #colour hueing
install.packages("dplyr")
install.packages("tidyr")

library(ncdf4)
library(raster)
library(sp)
library(RNetCDF)
library(pracma) # for Empirical Orthognal Function
library(stats) # base library needed for Primary Component Analysis
library(pbapply) # prog bar
library(RNCEP) 
library(lubridate)
library(tidyverse)
library(sf)
library(tidyr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
print("Packages Loaded, starting nc load")

# Open the NetCDF file
nc_file <- "fc8f2337e0cc6e9dafa39b57484565a1.nc"
nc_data <- nc_open(nc_file)
print("nc file loaded")

# show what variables you're working with
variables <- names(nc_data$var)
print(variables)

# must be chunked unless you have an ungodly amount of RAM
extract_first_value <- function(var_name, nc_data, chunk_size = 1000) {
  var_info <- nc_data$var[[var_name]]
  var_size <- var_info$size
  num_dims <- length(var_size)
  
  start <- rep(1, num_dims)
  count <- rep(1, num_dims)
  
  # just the first chunk's data
  chunk_data <- ncvar_get(nc_data, var_name, start = start, count = count)
  
  return(chunk_data[1])
}

# Just the first value for each variable to understand whats what
first_values <- sapply(variables, function(var) {
  extract_first_value(var, nc_data)
})

# Frame
first_values_df <- data.frame(variable = variables, first_value = first_values)
print(first_values_df)

# Close the ncdf 
nc_close(nc_data)
print("ncdf file closed")
