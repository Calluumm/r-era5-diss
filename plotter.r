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
library(lubridate)
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

plot_sd <- function(nc_file_path1, nc_file_path2, vr, title, y_label) {
  vr <- variable_mappings[[vr]]
  nc_data1 <- nc_open(nc_file_path1)
  time1 <- ncvar_get(nc_data1, "valid_time")
  value1 <- ncvar_get(nc_data1, vr)
  nc_close(nc_data1)
  
  nc_data2 <- nc_open(nc_file_path2)
  time2 <- ncvar_get(nc_data2, "valid_time")
  value2 <- ncvar_get(nc_data2, vr)
  nc_close(nc_data2)

  if (vr %in% c("t", "t2m")) {
    value1 <- value1 - 273.15
    value2 <- value2 - 273.15
  }
  if (vr %in% c("tp", "s")) {
    value1 <- value1 * 1000 * 31
    value2 <- value2 * 1000 * 28
  }

  time1 <- as.POSIXct(time1, origin = "1970-01-01", tz = "UTC")
  time2 <- as.POSIXct(time2, origin = "1970-01-01", tz = "UTC")
  
  df1 <- data.frame(time = time1, value = value1, legend = "Summer (July)")
  df2 <- data.frame(time = time2, value = value2, legend = "Winter (February)")  
  df <- rbind(df1, df2)
  
  df <- df %>%
    mutate(year = year(time)) %>%
    group_by(year, legend) %>%
    summarise(mean_value = mean(value, na.rm = TRUE))
  
  lm_model1 <- lm(mean_value ~ year, data = df[df$legend == "Summer (July)", ])
  lm_model2 <- lm(mean_value ~ year, data = df[df$legend == "Winter (February)", ])
  
  slope1 <- coef(lm_model1)[2]
  slope2 <- coef(lm_model2)[2]
  
  slope_text1 <- paste("Summer Rate of change: ", round(slope1, 4), " m/s per year", sep = "")
  slope_text2 <- paste("Winter Rate of change: ", round(slope2, 4), " m/s per year", sep = "")
  
  # Plot the data
  plot <- ggplot(df, aes(x = year, y = mean_value, color = legend)) +
    geom_line() +
    geom_smooth(method = "lm", se = FALSE) +
    labs(title = title, y = y_label, x = "Year") +
    annotate("text", x = Inf, y = Inf, label = slope_text1, hjust = 1.9, vjust = 2, size = 5, color = "red", parse = FALSE) +
    annotate("text", x = Inf, y = Inf, label = slope_text2, hjust = 1.95, vjust = 3.5, size = 5, color = "blue", parse = FALSE)
  
  # Save the plot
  filename <- "plottedvr_best_fit.png"
  ggsave(filename, plot = plot, width = 10, height = 6, dpi = 300)
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
    print(paste("Reshaped diff_data dimensions:", dim(diff_data)))
  
  data <- expand.grid(lon = lon, lat = lat)
  data$variable <- as.vector(diff_data)
  
  p <- ggplot(data, aes(x = lon, y = lat, fill = vr)) +
    geom_tile() +
    borders(
      database = "world",
      regions = ".",
      fill = NA,
      colour = "grey50",
      xlim = range(lon),
      ylim = range(lat)
    ) +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
    coord_sf(xlim = range(lon), ylim = range(lat), expand = FALSE) +
    labs(
      title = paste("Difference in", vr, "between", start_decade1, "and", start_decade2),
      x = "Longitude", y = "Latitude",
      fill = vr
    ) +
    coord_fixed(xlim = range(lon), ylim = range(lat))

  
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
  
  if (length(avg_data) != length(lat) * length(lon)) {
    stop("Dimensions of avg_data do not match the grid defined by lat and lon")
  }  
  plot_data <- expand.grid(lon = lon, lat = lat)
  plot_data$avg_data <- as.vector(avg_data)
  
  p <- ggplot(plot_data) +
    geom_raster(aes(x = lon, y = lat, fill = avg_data)) +
    scale_fill_gradientn(
      colors = c(low = "blue", mid = "white", high = "red", midpoint = 0),
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


plot_wind_single <- function(file_path, vr, lat_min, lat_max, lon_min, lon_max) {
  nc_data <- nc_open(file_path)
  
  # Use the correct variable names from variable_mappings
  u_var <- variable_mappings[["u_component_of_wind"]]
  v_var <- variable_mappings[["v_component_of_wind"]]
  
  u_component <- ncvar_get(nc_data, u_var)
  v_component <- ncvar_get(nc_data, v_var)
  lon <- ncvar_get(nc_data, "longitude")
  lat <- ncvar_get(nc_data, "latitude")
  
  wind_speed <- sqrt(u_component^2 + v_component^2)
  
  data <- expand.grid(lon = lon, lat = lat)
  data$u_component <- as.vector(u_component)
  data$v_component <- as.vector(v_component)
  data$wind_speed <- as.vector(wind_speed)
  
  nc_close(nc_data)
  
  if (!is.null(lat_min) && !is.null(lat_max)) {
    data <- data[data$lat >= lat_min & data$lat <= lat_max, ]
  }
  if (!is.null(lon_min) && !is.null(lon_max)) {
    data <- data[data$lon >= lon_min & data$lon <= lon_max, ]
  }
  
  p <- ggplot(data, aes(x = lon, y = lat, fill = wind_speed)) +
    geom_tile() +
    borders(
      database = "world",
      regions = ".",
      fill = NA,
      colour = "#353131",
      
    ) +
    scale_fill_gradient2(low = "white", mid = "white", high = "red", midpoint = 0) +
    coord_sf(xlim = c(lon_min, lon_max), ylim = c(lat_min, lat_max), expand = FALSE, clip = "on", crs = 4326) +
    labs(
      title = paste("Wind Speed and Direction"),
      x = "Longitude", y = "Latitude",
      fill = "Wind Speed m/s^2"
    ) +
    coord_fixed(xlim = c(lon_min, lon_max), ylim = c(lat_min, lat_max)) +
    theme(
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      plot.margin = unit(c(0, 0, 0, 0), "cm")
    )
  # Reduce the arrow frequency by averaging nearby arrows
  grid_size <- 1  # Adjust this value as needed
  arrow_data <- data %>%
    mutate(
      lon_bin = cut(lon, breaks = seq(min(lon), max(lon), by = grid_size)),
      lat_bin = cut(lat, breaks = seq(min(lat), max(lat), by = grid_size))
    ) %>%
    group_by(lon_bin, lat_bin) %>%
    summarize(
      lon = mean(lon),
      lat = mean(lat),
      u_component = mean(u_component),
      v_component = mean(v_component),
      wind_speed = mean(wind_speed)
    ) %>%
    ungroup()
  
  # Scale arrow lengths by wind speed
  arrow_scale <- 0.1  # Adjust this value as needed
  p <- p + geom_segment(data = arrow_data, aes(x = lon, y = lat, xend = lon + u_component * arrow_scale, yend = lat + v_component * arrow_scale, size = wind_speed),
                        arrow = arrow(length = unit(0.2, "cm")), color = "black", show.legend = TRUE) +
    scale_size_continuous(name = "Wind Speed", range = c(0.1, 1))  # Adjust the range as needed
    
  ggsave(filename = paste0("uvwind_", yrs, ".png"), plot = p, width = 10, height = 6, dpi = 300)
  print("Saved")
  return(p)
}

plot_wind_average <- function(file_path, vr, lat_min, lat_max, lon_min, lon_max) {
  nc_data <- nc_open(file_path)
  
  u_var <- variable_mappings[["u_component_of_wind"]]
  v_var <- variable_mappings[["v_component_of_wind"]]
  
  u_component <- ncvar_get(nc_data, u_var)
  v_component <- ncvar_get(nc_data, v_var)
  lon <- ncvar_get(nc_data, "longitude")
  lat <- ncvar_get(nc_data, "latitude")
  
  wind_speed <- sqrt(u_component^2 + v_component^2)
  
  data <- expand.grid(lon = lon, lat = lat)
  data$u_component <- as.vector(u_component)
  data$v_component <- as.vector(v_component)
  data$wind_speed <- as.vector(wind_speed)
  
  nc_close(nc_data)
  
  # Apply latitude and longitude limits if they are not NULL
  if (!is.null(lat_min) && !is.null(lat_max)) {
    data <- data[data$lat >= lat_min & data$lat <= lat_max, ]
  }
  if (!is.null(lon_min) && !is.null(lon_max)) {
    data <- data[data$lon >= lon_min & data$lon <= lon_max, ]
  }
  
  if (length(dim(data)) == 3) {
    data <- array(data, dim = c(dim(data)[1] * dim(data)[2], dim(data)[3]))
    valid_time_subset <- rep(valid_time, each = dim(data)[1])
  } else if (length(dim(data)) == 2) {
    data <- data
  } else {
    stop("Error: data does not have the expected number of dimensions")
  }
  averaged_data <- calculate_averages(data)
  
  p <- ggplot(averaged_data, aes(x = lon, y = lat, fill = wind_speed)) +
    geom_tile() +
    borders(
      database = "world",
      regions = ".",
      fill = NA,
      colour = "#353131"
    ) +
    scale_fill_gradient2(low = "white", mid = "white", high = "red", midpoint = 0) +
    coord_sf(xlim = c(lon_min, lon_max), ylim = c(lat_min, lat_max), expand = FALSE, clip = "on", crs = 4326) +
    labs(
      title = paste("Average Wind Speed and Direction"),
      x = "Longitude", y = "Latitude",
      fill = "Wind Speed m/s^2"
    ) +
    coord_fixed(xlim = c(lon_min, lon_max), ylim = c(lat_min, lat_max)) +
    theme(
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      plot.margin = unit(c(0, 0, 0, 0), "cm")
    )
  
  # Reduce the arrow frequency by averaging nearby arrows
  grid_size <- 1  # Adjust this value as needed
  arrow_data <- averaged_data %>%
    mutate(
      lon_bin = cut(lon, breaks = seq(min(lon), max(lon), by = grid_size)),
      lat_bin = cut(lat, breaks = seq(min(lat), max(lat), by = grid_size))
    ) %>%
    group_by(lon_bin, lat_bin) %>%
    summarize(
      lon = mean(lon),
      lat = mean(lat),
      u_component = mean(u_component),
      v_component = mean(v_component),
      wind_speed = mean(wind_speed)
    ) %>%
    ungroup()
  
  arrow_scale <- 0.1  # Adjust this value as needed
  p <- p + geom_segment(data = arrow_data, aes(x = lon, y = lat, xend = pmin(lon + u_component * arrow_scale, lon_max), yend = pmin(lat + v_component * arrow_scale, lat_max)),
                        arrow = arrow(length = unit(0.2, "cm")), color = "black")
  
  ggsave(filename = paste0("uvwind_average_", yrs, ".png"), plot = p, width = 10, height = 6, dpi = 300)
  print("Saved")
  return(p)
}

plot_wind_sd <- function(file_path, vr) {
  nc_data <- nc_open(file_path)  
  u_var <- variable_mappings[["u_component_of_wind"]]
  v_var <- variable_mappings[["v_component_of_wind"]]
  
  u_component <- ncvar_get(nc_data, u_var)
  v_component <- ncvar_get(nc_data, v_var)
  date <- ncvar_get(nc_data, "date")
  date <- as.Date(as.character(date), format = "%Y%m%d")  
  wind_speed <- sqrt(u_component^2 + v_component^2)
  df <- data.frame(time = rep(date, each = dim(u_component)[1] * dim(u_component)[2]), wind_speed = as.vector(wind_speed))

  nc_close(nc_data)
  df <- df %>%
    mutate(year = format(time, "%Y")) %>%
    group_by(year) %>%
    summarise(
      mean_wind_speed = mean(wind_speed, na.rm = TRUE),
      sd_wind_speed = sd(wind_speed, na.rm = TRUE)
    ) %>%
    mutate(
      ymin = mean_wind_speed - sd_wind_speed,
      ymax = mean_wind_speed + sd_wind_speed
    )  
  df <- df %>%
    mutate(
      ymin = ifelse(is.na(ymin), mean_wind_speed, ymin),
      ymax = ifelse(is.na(ymax), mean_wind_speed, ymax)
    )
  overall_mean_wind_speed <- mean(df$mean_wind_speed, na.rm = TRUE)

  p <- ggplot(df, aes(x = as.numeric(year), y = mean_wind_speed)) +
    geom_line(color = "blue") +
    geom_ribbon(aes(ymin = ymin, ymax = ymax), alpha = 0.2, fill = "blue") +
    geom_line(aes(y = ymin), color = "blue", linetype = "dashed") +
    geom_line(aes(y = ymax), color = "blue", linetype = "dashed") +
    geom_hline(yintercept = overall_mean_wind_speed, color = "red", linetype = "dotted", size = 1) +
    labs(title = "Average Wind Speed Over Time", x = "Year", y = "Wind Speed (m/s)") +
    theme_bw()
  
  filename <- paste0("uvwind_", format(Sys.Date(), "%Y"), "_sd.png")
  ggsave(filename, plot = p, width = 10, height = 6, dpi = 300)
  print(p)
  print("Saved")
  return(p)
}

plot_monthly_overlay <- function(file_path, vr, valid_time) {
  nc_data <- nc_open(file_path)
  date <- valid_time
  var <- variable_mappings[[vr]]
  variable_data <- ncvar_get(nc_data, var)
  nc_close(nc_data)

  variable_data <- as.vector(variable_data)

  if (var %in% c("t", "t2m")) {
    variable_data <- variable_data - 273.15
  }

  data <- data.frame(date = date, value = variable_data)
  data$month <- format(data$date, "%m")
  data$year <- format(data$date, "%Y")
  
  monthly_stats <- data %>%
    group_by(month) %>%
    summarize(
      avg_value = mean(value, na.rm = TRUE),
      min_value = min(value, na.rm = TRUE),
      max_value = max(value, na.rm = TRUE)
    )
  
  max_month <- monthly_stats %>% filter(avg_value == max(avg_value))
  min_month <- monthly_stats %>% filter(avg_value == min(avg_value))
  
  plot <- ggplot(monthly_stats, aes(x = as.numeric(month))) +
    geom_line(aes(y = avg_value), color = "blue") +
    scale_x_continuous(breaks = 1:12, labels = month.abb) +
    labs(title = "Monthly Aggregate Temperature Average", x = "Month", y = "Temperature (°C)") +
    theme_grey(base_size = 16) +
    annotate("text", x = as.numeric(max_month$month), y = max_month$avg_value, 
             label = paste("Max:", round(max_month$avg_value, 2), "°C"), vjust = 2, size = 6) +
    annotate("text", x = as.numeric(min_month$month), y = min_month$avg_value, 
             label = paste("Min:", round(min_month$avg_value, 2), "°C"), vjust = 1, size = 6)
  
  ggsave(plot = plot, filename = paste0(vr, "_monthly_overlay.png"), width = 10, height = 6, dpi = 300)
  print(plot)
}
