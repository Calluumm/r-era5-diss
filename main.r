library(ncdf4)
library(stats)
library(pracma)
library(ggplot2)
library(zoo)
setwd("C:/wd/file/example")
source("Functions.r")
print("function")
source("variables.r")
print("variables")
source("plotter.r")
print("plotter")
#employ parallel processing this is so slow i might cry

ph <- "C:\\File\\Path"
vr <- c("temperature") #reference variables for help
vr2 <- c("v_component_of_wind2")
yrs <- 2024
mn <- sprintf("0%d",5:5)
dy <- c(sprintf("0%d",1:9),10:31)
tm <- "00:00"
are <- "77.22/15.63/77.23/15.64"
cds.key <- "cds key"
wf_set_key(user ="cds user", key = cds.key)
nc_file_path2 <- "C:\\second\\path\\yeah.nc"
#Set the following values to NULL if your dataset already represents what you want to see; these exist to refine it further
lat_min <- 76
lat_max <- 81
lon_min <- 9
lon_max <- 29
# svalbard values 76/81/9/29 S/N/W/E
# ALL THESE VARIABLES ARE EXAMPLES CHANGE BASED ON WHAT YOU WANT#
# land data only takes 1 year/1 month but variable days
choice <- readline(prompt = "Choose an option \n1: era5_data \n2: era5_land_data \n3: continue without downloading \n")
if (choice == "1") {
  download_era5_data(yrs, mn, tm, vr, ph, are)
} else if (choice == "2") {
  download_era5_land_data(yrs, mn, dy, tm, vr, ph, are)
} else if (choice == "3") {
  cat("Continuing without downloading...\n")
} else {
  cat("Invalid choice. Continuing without downloading...\n")
}

#Comment out the one you are/aren't using I will merge this with choice above eventually where choice 3 options between the two
#Automatic entry of file path
#file_path <- get_most_recent_nc_file(ph)
# Manual entry of file path
file_path <- paste0(ph, "\\JAN-DEC_T_1940_2024.nc") 

print("Starting to process the NetCDF file...")
nc_data <- nc_open(file_path)
variables <- names(nc_data$var)
print(variables)
if (!all(vr %in% names(variable_mappings))) {
  stop("Invalid variable name. Please check the variable_mappings.")
}

choice2 <- readline(prompt = "Which Dataset are you using, (1: era5_data, 2: era5_land_data, 3:era5_single_levels): ")
if (choice2 == "1") {
  date <- ncvar_get(nc_data, "date")
  date <- as.Date(as.character(date), format = "%Y%m%d")
  valid_time <- date
  #convert from 20180101 to 2018-01-01
} else if (choice2 == "2") {
  valid_time <- ncvar_get(nc_data, "valid_time")
  valid_time <- as.POSIXct(valid_time, origin = "1970-01-01", tz = "UTC") #converts out of unix time
} else if (choice == "3") {
  valid_time <- nc_data$dim[["valid_time"]]$vals
  valid_time <- as.POSIXct(valid_time, origin = "1970-01-01", tz = "UTC")
  print(paste("Length of valid_time:", length(valid_time)))
  print(head(valid_time))
  #print(paste("Length of valid_time_subset:", length(valid_time_subset)))
  #print(paste("Length of retrieved_data:", length(retrieved_data)))
}

valid_time_subset <- valid_time
if (!vr %in% names(variable_mappings)) {
  stop("Invalid variable name. Please check the variable_mappings.")
}

# if (vr == "u_component_of_wind" || vr == "v_component_of_wind") {
#   cat("Choose an option:\n1. Continue without creating a combined u/v vector\n2. Plot a single timeframe with a u/v vector\n3. Create an average u/v vector plot\n4. A line graph of wind magnitude overtime \n")
#   uv_choice <- as.integer(readLines(con = stdin(), n = 1))
#   if (uv_choice == 1) {
#     print("Continuing without creating a combined u/v vector")
#   } else if (uv_choice == 2) {
#     plot <- plot_wind_single(file_path, vr, lat_min, lat_max, lon_min, lon_max)
#     print(p)
#     stop()
#   } else if (uv_choice == 3) {
#     #plot_wind_average(file_path, vr)
#     #print(plot)
#     stop("Being Worked on")
#   } else if (uv_choice == 4) {
#     plot_wind_sd(file_path, vr)
#     print(plot)
#     stop()
#   } else {
#     stop("Invalid choice. Please select a valid option.")
#   }
# }

retrieved_data <- ncvar_get(nc_data, variable_mappings[[vr]])

if (variable_mappings[[vr]] == "t") {
  retrieved_data <- retrieved_data - 273.15
  print("Converted temperature from Kelvin to Celsius.")
}
if (variable_mappings[[vr]] == "t2m") {
  retrieved_data <- retrieved_data - 273.15
  print("Converted temperature from Kelvin to Celsius.")
}
if (vr == "total_precipitation") {
  retrieved_data <- (retrieved_data * 1000 * 28)
  print("Converted from m to mm")
}
if (vr == "snowfall") {
  retrieved_data <- (retrieved_data * 1000 * 28)
  print("Converted from m to mm")
}


datefixer <- readline(prompt = "Does the file provided need date fixings?\n Applicable to non single point datasets\n yes/no \n")
if (datefixer == "yes") {
  if (length(dim(retrieved_data)) == 3) {
    # If it has a time dimension, reshape the data to associate each data point with the correct timestamp
    retrieved_data <- array(retrieved_data, dim = c(dim(retrieved_data)[1] * dim(retrieved_data)[2], dim(retrieved_data)[3]))
    valid_time_subset <- rep(valid_time, each = dim(retrieved_data)[1])
  } else if (length(dim(retrieved_data)) == 2) {
    # If it has no time dimension, use the data as is
    retrieved_data <- retrieved_data
  } else {
    stop("Error: retrieved_data does not have the expected number of dimensions")
  }
} else if (datefixer == "no") {
  print("Continuing without date fixings...")
  if (is.null(retrieved_data)) {
    retrieved_data <- ncvar_get(nc_data, vr)
  }
}

nc_close(nc_data)
df <- data.frame(time = valid_time_subset, value = retrieved_data)

cat("Choose plot type:\n1. Line Plot\n2. Smooth Plot\n3. SD Yearly Plot \n4. Shapefile Plot (only for temperature/single timeframes right now)\n5. Plot shapefile difference between 2 individual timeframes\n6. Plot a shapefile comparison between decades\n7. Plot a shapefile average of an entire ncdf\n8. Plot a linegraph average of monthly averages across a year timeseries.\n")
plot_choice <- as.integer(readLines(con = stdin(), n = 1))


#title <- paste(vr, "in", yrs)
#manual title entries and y_label for simplicity when mass working with plot option 3
title <- ("Summer and Winter V-Component of Wind Trends in West Svalbard (1940–2024)")
unit <- variable_units[[vr]]
if (is.null(unit)) {
  unit <- ""  # Default to empty string if unit is not found
}
y_label <- paste("Average Windspeed (m/s)")

if (plot_choice == 1) {
  plot <- plot_line(df, title, y_label)
} else if (plot_choice == 2) {
  cat("Moving average window size:\n")
  window_size <- as.integer(readLines(con = stdin(), n =1))
  plot <- plot_smooth(df, title, y_label, window_size)
} else if (plot_choice == 3) {
  #top of comment out if not using
  nc_data1 <- nc_open(file_path)
  retrieved_data1 <- ncvar_get(nc_data1, variable_mappings[[vr]])
  nc_close(nc_data1)
  nc_data2 <- nc_open(nc_file_path2)
  retrieved_data2 <- ncvar_get(nc_data2, variable_mappings[[vr2]])
  nc_close(nc_data2)
  combined_data <- ((retrieved_data1 + retrieved_data2) *1000 * 31)
  df_combined <- data.frame(time = valid_time_subset, value = combined_data)
  #bottom
  plot <- plot_sd(file_path, nc_file_path2, vr, title, y_label)
  print(plot)
  stop()
}else if (plot_choice == 4) {
  plot <- plot_shapefile(file_path, vr)
}else if (plot_choice == 5) {
  plot <- plot_shapefile_difference(file_path, nc_file_path2, vr)
} else if (plot_choice ==6) {
  cat("Enter the start of decade 1 (e.g. 1940): \n")
  start_decade1 <- as.integer(readLines(con = stdin(), n = 1))
  cat("Enter the start of decade 2 (e.g. 1950): \n")
  start_decade2 <- as.integer(readLines(con = stdin(), n = 1))
  plot <- plot_shapefile_ranged(file_path, vr, start_decade1, start_decade2)
} else if (plot_choice == 7) {
  plot <- plot_entire_average(file_path, vr)
} else if (plot_choice == 8) { #aggregate plot
  plot <- plot_monthly_overlay(file_path, vr, valid_time)
} else {
  stop("Invalid plot choice.")
}

print(plot)
stop("stopped")
