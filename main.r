library(ncdf4)
library(stats)
library(pracma)
library(ggplot2)
library(zoo)
source("Functions.r")
source("variables.r")
source("plotter.r")
#employ parallel processing this is so slow i might cry

ph <- "FILE_PATH"
vr <- c("temperature")
yrs <- 2024
mn <- sprintf("0%d",5:5)
dy <- c(sprintf("0%d",1:9),10:31)
tm <- "00:00"
are <- "77.22/15.63/77.23/15.64"
cds.key <- "API KEY"
wf_set_key(user ="USERNAME", key = cds.key)
nc_file_path2 <- "SECONDARY NCDF FILE PATH"
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
file_path <- paste0(ph, "\\MANUAL NCDF.nc")

print("Starting to process the NetCDF file...")
nc_data <- nc_open(file_path)
variables <- names(nc_data$var)
print(variables)

choice2 <- readline(prompt = "Which Dataset are you using, (1: era5_data, 2: era5_land_data): ")
if (choice2 == "1") {
  date <- ncvar_get(nc_data, "date")
  date <- as.Date(as.character(date), format = "%Y%m%d")
  valid_time <- date
  #convert from 20180101 to 2018-01-01
} else if (choice2 == "2") {
  valid_time <- ncvar_get(nc_data, "valid_time")
  valid_time <- as.POSIXct(valid_time, origin = "1970-01-01", tz = "UTC") #converts out of unix time
}

valid_time_subset <- valid_time
if (!vr %in% names(variable_mappings)) {
  stop("Invalid variable name. Please check the variable_mappings.")
}
retrieved_data <- ncvar_get(nc_data, variable_mappings[[vr]])

if (variable_mappings[[vr]] == "t") {
  retrieved_data <- retrieved_data - 273.15
  print("Converted temperature from Kelvin to Celsius.")
}

datefixer <- readline(prompt = "Does the file provided need date fixings?\n Applicable to non single point datasets\n yes/no \n")
if (datefixer == "yes") {
  if (length(dim(retrieved_data)) == 3) {
    retrieved_data <- array(retrieved_data, dim = c(dim(retrieved_data)[1] * dim(retrieved_data)[2], dim(retrieved_data)[3]))
    valid_time_subset <- rep(valid_time, each = dim(retrieved_data)[1])
  } else if (length(dim(retrieved_data)) == 2) {
    retrieved_data <- retrieved_data
  } else {
    stop("Error: retrieved_data does not have the expected number of dimensions")
  }
} else if (datefixer == "no") {
  print("Continuing without date fixings...")
}

nc_close(nc_data)
df <- data.frame(time = valid_time_subset, value = retrieved_data)

cat("Choose plot type:\n1. Line Plot\n2. Smooth Plot\n3. SD Yearly Plot \n4. Shapefile Plot (only for temperature/single timeframes right now)\n5. Plot shapefile difference between 2 individual timeframes\n6. Plot a shapefile comparison between decades\n7. Plot a shapefile average of an entire ncdf\n")
plot_choice <- as.integer(readLines(con = stdin(), n = 1))


title <- paste(vr, "in", yrs)
unit <- variable_units[[vr]]
if (is.null(unit)) {
  unit <- ""  # Default to empty string if unit is not found
}
y_label <- paste(vr, "(", unit, ")", sep="")

if (plot_choice == 1) {
  plot <- plot_line(df, title, y_label)
} else if (plot_choice == 2) {
  cat("Moving average window size:\n")
  window_size <- as.integer(readLines(con = stdin(), n =1))
  plot <- plot_smooth(df, title, y_label, window_size)
} else if (plot_choice == 3) {
  cat("Include standard deviations? (yes/no):\n")
  include_sd <- tolower(readLines(con = stdin(), n = 1)) == "yes"
  if (include_sd) {
    cat("Standard deviation factor:\n")
    sd_factor <- as.numeric(readLines(con = stdin(), n = 1))
  }
  plot <- plot_sd(df, title, y_label, include_sd, sd_factor)
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

} else {
  stop("Invalid plot choice.")
}

print(plot)
stop("stopped")
