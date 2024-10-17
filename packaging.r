# incase of the others in my dissertation group needing to start using r
# download r
# Change your r.rpath.windows to the R Path (go to your settings.json and add this     "r.rpath.windows": "FILE PATH"
# for example     "r.rpath.windows": "C:\\Program Files\\R\\R-4.1.0\\bin\\R.exe"

# download data from https://cds.climate.copernicus.eu/datasets/reanalysis-era5-pressure-levels-monthly-means?tab=download
# make sure to select necessary vairables and to not exceed maximum request size, make sure to select NETcdf4 as your format and NOT GRIB
# https://confluence.ecmwf.int/display/CKB/ERA5%3A+data+documentation documentation of the era5 dataset and how its formatted with cdf4

install.packages("ncdf4") #for reading the database
install.packages("raster") #geographic data analysis
install.packages("sp") #spacial data library
install.packages("RNetCDF") #other netcdf reader
install.packages("pracma") #mathematical dependancy for EOF functions
#can use rgdal if you find a supported fork of it but not reccomended

library(ncdf4)
library(raster)
library(sp)
library(RNetCDF)
library(pracma) # for Empirical Orthognal Function
library(stats) # base library needed for Primary Component Analysis





















############### api request code if needed later #################
# import cdsapi

# dataset = "reanalysis-era5-pressure-levels-monthly-means"
# request = {
#     "product_type": ["monthly_averaged_reanalysis"],
#     "variable": [
#         "specific_rain_water_content",
#         "temperature",
#         "u_component_of_wind",
#         "v_component_of_wind"
#     ],
#     "pressure_level": [
#         "1", "2", "3",
#         "5", "7", "10",
#         "20", "30", "50",
#         "70", "100", "125",
#         "150", "175", "200",
#         "225", "250", "300",
#         "350", "400", "450",
#         "500", "550", "600",       Knock off pressure levels if request is too large
#         "650", "700", "750",
#         "775", "800", "825",
#         "850", "875", "900",
#         "925", "950", "975",
#         "1000"
#     ],
#     "year": [
#         "1940", "1941", "1942",
#         "1943", "1944", "1945",
#         "1946", "1947", "1948",
#         "1949", "1950", "1951",
#         "1952", "1953", "1954",
#         "1955", "1956", "1957",
#         "1958", "1959", "1960",
#         "1961", "1962", "1963",
#         "1964", "1965", "1966",
#         "1967", "1968", "1969",
#         "1970", "1971", "1972",
#         "1973", "1974", "1975",
#         "1976", "1977", "1978",
#         "1979", "1980", "1981",       Can also downsize overall scope
#         "1982", "1983", "1984",
#         "1985", "1986", "1987",
#         "1988", "1989", "1990",
#         "1991", "1992", "1993",
#         "1994", "1995", "1996",
#         "1997", "1998", "1999",
#         "2000", "2001", "2002",
#         "2003", "2004", "2005",
#         "2006", "2007", "2008",
#         "2009", "2010", "2011",
#         "2012", "2013", "2014",
#         "2015", "2016", "2017",
#         "2018", "2019", "2020",
#         "2021", "2022", "2023",
#         "2024"
#     ],
#     "month": [
#         "01", "02", "03",
#         "04", "05", "06",            # or reduce resolution within the scope
#         "07", "08", "09",
#         "10", "11", "12"
#     ],
#     "time": ["00:00"],
#     "data_format": "netcdf",
#     "download_format": "unarchived",
#     "area": [90, -180, 0, 180]             #to be assessed if this is too large an area to be evaluating; given you only need arctic but all the way 
# }                                          # down to the azores can affect the arctic oscillations

# client = cdsapi.Client()
# client.retrieve(dataset, request).download()
