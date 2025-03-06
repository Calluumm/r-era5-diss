install.packages("ncdf4") # For reading the database
install.packages("sp") # Spatial data library
install.packages("pracma") # Mathematical dependency for EOF functions
install.packages("RNCEP") #to use the online version of the dataset
install.packages("lubridate") #date formatting
install.packages("sf") #use GIS shapefiles to plot to 
install.packages("ggplot2") #plot to a map
install.packages("RColorBrewer") #colour hueing
install.packages("tidyverse") #data manipulation encompasses 2 below
install.packages("dplyr")
install.packages("tidyr")
install.packages("foreach")
install.packages("ecmwfr") #api calling
install.packages("geodata") #for GIS
install.packages("terra") #raster data
install.packages("devtools") #git imports


library(ncdf4)
library(sp)
library(pracma) # for Empirical Orthognal Function
library(stats) # base library needed for Primary Component Analysis
library(RNCEP) 
library(lubridate)
library(tidyverse)
library(sf)
library(tidyr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(foreach)
library(ecmwfr)
library(geodata)
library(terra)
library(devtools)
# some of these might end up not needed but I'm using them while testing stuff will clean the list fully at a later date
# below is a tiny script just for looking at the first chunk of data and its variables to get a better understanding
# of whats what and what kind of numbers to expect (time is in unix, temperature in kelvin)

print("Packages Loaded, starting nc load")

# Open the NetCDF file
nc_file <- "FILE PATH"
nc_data <- nc_open(nc_file)
print("nc file loaded")

# show what variables you're working with
variables <- names(nc_data$var)
print(variables)

# must be chunked unless you have an ungodly amount of RAM
extract_first_value <- function(var_name, nc_data, chunk_size = 50) {
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

first_values_df <- data.frame(variable = variables, first_value = first_values)
print(first_values_df)

# Close the ncdf 
nc_close(nc_data)
print("ncdf file closed")
