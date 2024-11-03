library(ggplot2)
library(dplyr)
library(zoo)
library(sf)
library(viridis)
library(ncdf4)
source("variables.r")
source("Functions.r")
library(terra)
library(ggspatial)
library(basemaps)
library(prettymapr)
library(sp)

basemaps::set_defaults(map_service = "esri", map_type = "world_imagery")

# Function to list variables in a NetCDF file
list_nc_variables <- function(nc_file_path) {
  nc_data <- nc_open(nc_file_path)
  variables <- names(nc_data$var)
  nc_close(nc_data)
  return(variables)
}

plot_line <- function(df, title, y_label) {
  plot <- ggplot(df, aes(x = time, y = value)) +
    geom_line(color = "red") +
    labs(title = title, x = "Time", y = y_label) +
    theme_bw()
  filename <- paste0(vr, "_", yrs, "_raw.png")
  ggsave(filename, plot = p, width = 10, height = 6, dpi =300)
  print(plot)
  print("saved")
}

plot_smooth <- function(df, title, y_label, window_size) {
  df <- df %>%
    arrange(time) %>%
    mutate(moving_avg = zoo::rollmean(value, k = window_size, fill = NA, align = "right"))
  
  plot <- ggplot(df, aes(x = time, y = value)) +
    geom_line(color = "red") +
    geom_line(aes(y = moving_avg), color = "blue") +
    labs(title = title, x = "Time", y = y_label) +
    theme_bw()
  filename <- paste0(vr, "_", yrs, "_smoothed.png")
  ggsave(filename, plot = plot, width = 10, height = 6, dpi =300)
  print(plot)
  print("saved")
}

plot_sd <- function(df, title, y_label, include_sd = TRUE, sd_factor = 1) {
  df <- df %>%
    mutate(year = format(time, "%Y")) %>%
    group_by(year) %>%
    summarise(
      mean_value = mean(value, na.rm = TRUE),
      sd_value = sd(value, na.rm = TRUE)
    )
  
  if (include_sd) {
    df <- df %>%
      mutate(
        ymin = mean_value - sd_factor * sd_value,
        ymax = mean_value + sd_factor * sd_value
      )
  }
  
  p <- ggplot(df, aes(x = as.numeric(year), y = mean_value)) +
    geom_line(color = "blue") +
    labs(title = title, x = "Year", y = y_label) +
    theme_bw()
  
  if (include_sd) {
    p <- p + 
      geom_ribbon(aes(ymin = ymin, ymax = ymax), alpha = 0.2, fill = "blue") +
      geom_line(aes(y = ymin), color = "blue", linetype = "dashed") +
      geom_line(aes(y = ymax), color = "blue", linetype = "dashed")
  }
  
  return(p)
  filename <- paste0(vr, "_", yrs, "_average.png")
  ggsave(filename, plot = p, width = 10, height = 6, dpi =300)
  print(plot)
  print("saved")
}

plot_shapefile <- function(file_path, vr) {
  nc_data <- nc_open(file_path)
    lat <- ncvar_get(nc_data, "latitude")
  lon <- ncvar_get(nc_data, "longitude")
  retrieved_data <- ncvar_get(nc_data, variable_mappings[[vr]])
  nc_close(nc_data)
    
  if (length(dim(retrieved_data)) == 3) {
    retrieved_data <- retrieved_data[,,1]
  } else if (length(dim(retrieved_data)) == 2) {
    retrieved_data <- retrieved_data
  } else {
    stop("Error: retrieved_data does not have the expected number of dimensions")
  }
  
  data <- expand.grid(lon = lon, lat = lat)
  data$variable <- as.vector(retrieved_data)
  data <- na.omit(data)
  # PARAMTERS FOR COUNTRY BORDERS
  borders(
  database = "world",
  regions = ".",
  fill = NA,
  colour = "grey50",
  xlim = range(lon),
  ylim = range(lat)
  )
  #change the lims to limit the plot area to where you need
  plot <- ggplot() +
    annotation_map_tile() +
    geom_tile(data = data, aes(x = lon, y = lat, fill = variable)) +
    borders() +
    scale_fill_viridis_c() +
    labs(title = paste(vr, "over the Arctic"), fill = vr) +
    coord_sf(xlim = range(lon), ylim = range(lat), expand = FALSE) +
    theme_minimal()
  
  filename <- paste0(vr, "_shapefileplot_average.png")
  ggsave(filename, plot = plot, width = 10, height = 6, dpi = 300)
  print(plot)
  print("saved")
}


plot_shapefile_difference <- function(file_path, nc_file_path2, vr) {
  # one
  nc_data1 <- nc_open(file_path)
  lat1 <- ncvar_get(nc_data1, "latitude")
  lon1 <- ncvar_get(nc_data1, "longitude")
  retrieved_data1 <- ncvar_get(nc_data1, variable_mappings[[vr]])
  nc_close(nc_data1)
  #two
  nc_data2 <- nc_open(nc_file_path2)
  lat2 <- ncvar_get(nc_data2, "latitude")
  lon2 <- ncvar_get(nc_data2, "longitude")
  retrieved_data2 <- ncvar_get(nc_data2, variable_mappings[[vr]])
  nc_close(nc_data2)
  
  if (!all(dim(retrieved_data1) == dim(retrieved_data2))) {
    stop("Error: The dimensions of the two datasets do not match")
  }
  
  data_diff <- retrieved_data2 - retrieved_data1
  
  if (length(dim(data_diff)) == 3) {
    data_diff <- data_diff[,,1]
  } else if (length(dim(data_diff)) == 2) {
    data_diff <- data_diff
  } else {
    stop("Error: data_diff does not have the expected number of dimensions")
  }
  
  data <- expand.grid(lon = lon1, lat = lat1)
  data$variable <- as.vector(data_diff)
  data <- na.omit(data)
  lon <- lon1
  lat <- lat1
  p <- ggplot(data, aes(x = lon, y = lat, fill = variable)) +
    geom_tile() +
    borders(database = "world", regions = ".", fill = NA, colour = "grey50") +
    coord_sf(xlim = range(lon), ylim = range(lat), expand = FALSE) +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
    labs(title = paste("Difference in", vr, "between two times"),
         x = "Longitude", y = "Latitude", fill = vr)
  
  ggsave(filename = paste0("difference_in_", vr,".png"), plot = p, width = 10, height = 6, dpi = 300)
  print(p)
  print("saved")
}

plot_shapefile_ranged <- function(file_path, vr, start_decade1, start_decade2) {
  nc_data <- nc_open(file_path)
  lat <- ncvar_get(nc_data, "latitude")
  lon <- ncvar_get(nc_data, "longitude")
  date <- ncvar_get(nc_data, "date")
  years <- as.integer(substr(date, 1, 4))
  
  variable <- variable_mappings[[vr]]
  if (is.null(variable)) {
    stop(paste("Variable", vr, "not found in variable_mappings"))
  }
  
  var_data <- ncvar_get(nc_data, variable)
  var_dims <- dim(var_data)
  print(paste("Dimensions of variable", vr, ":", paste(var_dims, collapse = " x ")))
  
  avg_data1 <- calculate_average(nc_data, variable, start_decade1, start_decade1 + 9)
  avg_data2 <- calculate_average(nc_data, variable, start_decade2, start_decade2 + 9)
  diff_data <- avg_data2 - avg_data1
  
  # Debug prints
  print(paste("Latitude length:", length(lat)))
  print(paste("Longitude length:", length(lon)))
  print(paste("avg_data1 dimensions:", dim(avg_data1)))
  print(paste("avg_data2 dimensions:", dim(avg_data2)))
  print(paste("diff_data dimensions:", dim(diff_data)))
  
  if (is.vector(diff_data)) {
    diff_data <- matrix(diff_data, nrow = length(lat), ncol = length(lon))
  }
  
  # Debug print after reshaping
  print(paste("Reshaped diff_data dimensions:", dim(diff_data)))
  
  data <- expand.grid(lon = lon, lat = lat)
  data$variable <- as.vector(diff_data)
  
  p <- ggplot(data, aes(x = lon, y = lat, fill = variable)) +
    geom_tile() +
    borders(
      database = "world",
      regions = ".",
      fill = NA,
      colour = "grey50",
      xlim = range(lon),
      ylim = range(lat)
    ) +
    scale_fill_viridis_c() +
    coord_sf(xlim = range(lon), ylim = range(lat), expand = FALSE) +
    labs(title = paste("Difference in", vr, "between", start_decade1, "and", start_decade2),
         x = "Longitude", y = "Latitude")
    coord_fixed(ratio = 1)  # Ensure the aspect ratio is fixed

  
  print(p)
  nc_close(nc_data)
  ggsave(filename = paste0("Difference in", vr, "between", start_decade1, "and", start_decade2,".png"), plot = p, width = 10, height = 6, dpi = 300)
  return(p)
}

plot_entire_average <- function(file_path, vr) {
  nc_data <- nc_open(file_path)
  lat <- ncvar_get(nc_data, "latitude")
  lon <- ncvar_get(nc_data, "longitude")
  date <- ncvar_get(nc_data, "date")
  years <- as.integer(substr(date, 1, 4))
  avg_data <- calculate_average(nc_data, variable_mappings[[vr]], min(years), max(years))
  nc_close(nc_data)
  
  # Ensure avg_data has the correct dimensions
  if (length(avg_data) != length(lat) * length(lon)) {
    stop("Dimensions of avg_data do not match the grid defined by lat and lon")
  }
  
  # Create a data frame for ggplot
  plot_data <- expand.grid(lon = lon, lat = lat)
  plot_data$avg_data <- as.vector(avg_data)
  
  p <- ggplot(plot_data) +
    geom_raster(aes(x = lon, y = lat, fill = avg_data)) +
    scale_fill_gradientn(
      colors = c("blue", "cyan", "yellow", "red"),
      values = scales::rescale(c(min(plot_data$avg_data), mean(plot_data$avg_data), max(plot_data$avg_data))),
      na.value = "grey50"
    ) +
    borders(
      database = "world",
      regions = ".",
      fill = NA,
      colour = "grey50",
      xlim = range(lon),
      ylim = range(lat)
    ) +
    coord_fixed(xlim = range(lon), ylim = range(lat)) +
    labs(title = paste("Average", vr, "for entire dataset"),
         x = "Longitude", y = "Latitude", fill = vr)
  
  ggsave(filename = paste0("average_", vr, ".png"), plot = p, width = 10, height = 6, dpi = 300)
  print(p)
  print("saved")
}
