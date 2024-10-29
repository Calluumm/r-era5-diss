library(ggplot2)
library(dplyr)
library(zoo)
library(sf)
library(viridis)
library(ncdf4)
source("variables.r")
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

plot_shapefile <- function(nc_file_path, vr) {
  nc_data <- nc_open(nc_file_path)
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
  
  filename <- paste0(vr, "_shapefileplot.png")
  ggsave(filename, plot = plot, width = 10, height = 6, dpi = 300)
  print(plot)
  print("saved")
}
