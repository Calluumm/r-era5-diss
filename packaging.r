install.packages("ncdf4") # For reading the database
install.packages("raster") # Geographic data analysis
install.packages("sp") # Spatial data library
install.packages("RNetCDF") # Other NetCDF reader
install.packages("pracma") # Mathematical dependency for EOF functions
# Can use rgdal if you find a supported fork of it but not recommended

library(ncdf4)
library(raster)
library(sp)
library(RNetCDF)
library(pracma) # for Empirical Orthognal Function
library(stats) # base library needed for Primary Component Analysis
