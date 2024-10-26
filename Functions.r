#  EOF Function
perform_eof <- function(data_matrix) {
  eof_result <- svd(data_matrix)
  return(eof_result)
}

# PCA Function
perform_pca <- function(data_matrix) {
  pca_result <- prcomp(data_matrix, scale. = TRUE)
  return(pca_result)
}

library(foreach)
library(ecmwfr)

# Define the function in Functions.r
# removed dy while working with monthly averages
download_era5_land_data <- function(yr, mn, dy, tm, vr, ph, are) {
  for (i in 1:length(yr)) {
    for (j in 1:length(mn)) {
      request <- list(
        "dataset_short_name" = "reanalysis-era5-land",
        "product_type" = "reanalysis",
        "format" = "netcdf",
        "variable" = vr[1],
        "year" = yr[i],
        "month" = mn[j],
        "day" = dy,
        "time" = tm,
        "area" = are,
        "format" = "netcdf",
        "target" = paste0("era5_land_", yr[i], "_", mn[j], "_", vr[1], ".nc")
      )
      
      file <- wf_request(
        user     = "USERNAME",   # username
        request  = request,  # the request
        transfer = TRUE,     # download the file
        path     = ph,       # store data in path
        verbose = TRUE
      )
    }
  }
}

download_era5_data <- function(yr, mn, tm, vr, ph, are) {
  for (i in 1:length(yr)) {
    for (j in 1:length(mn)) {
      request <- list(
        "dataset_short_name" = "reanalysis-era5-pressure-levels-monthly-means",
        "product_type" = "reanalysis",
        "format" = "netcdf",
        "variable" = vr[1],
        "year" = yr[i],
        "month" = mn[j],
        "time" = tm,
        "area" = are,
        "format" = "netcdf",
        "target" = paste0("era5_", yr[i], "_", mn[j], "_", vr[1], ".nc")
      )
      
      file <- wf_request(
        user     = "USERNAME", #username
        request  = request,  # the request
        transfer = TRUE,     # download the file
        path     = ph,       # store data in path
        verbose = TRUE
      )
    }
  }
}


#svalbard whole area 81/5/75/35
#Create variable re-name centre
# to dynamically use plot function I need to add a
# centre for specifying t = temperature, etc...

# Chunking Function
chunk_data <- function(data, chunk_size) {
  if (is.data.frame(data) || is.matrix(data)) {
    num_rows <- nrow(data)
  } else if (is.array(data)) {
    num_rows <- dim(data)[1]
  } else {
    stop("Unsupported data type")
  }
  num_chunks <- ceiling(num_rows / chunk_size)
  chunks <- vector("list", num_chunks)
  for (i in 1:num_chunks) {
    start_row <- (i - 1) * chunk_size + 1
    end_row <- min(i * chunk_size, num_rows)
    chunks[[i]] <- data[start_row:end_row, , drop = FALSE]
  }
  return(chunks)
}

library(ncdf4)
library(stats)
library(pracma)
library(ggplot2)

# Function to process each chunk and calculate yearly averages
process_chunk <- function(chunk, time_indices, yearly_averages) {
  time_data <- time_indices
  
  # Calculate the yearly averages
  for (i in 1:length(time_data)) {
    year <- as.integer(format(as.Date(time_data[i], origin = "1970-01-01"), "%Y"))
    if (!is.null(yearly_averages[[as.character(year)]])) {
      yearly_averages[[as.character(year)]] <- c(yearly_averages[[as.character(year)]], mean(chunk[i, , , ], na.rm = TRUE))
    } else {
      yearly_averages[[as.character(year)]] <- mean(chunk[i, , , ], na.rm = TRUE)
    }
  }
  
  return(yearly_averages)
}

# Function to read and process data in chunks DIRECTLY from a NetCDF file
process_nc_chunks <- function(nc_file, var_name, chunk_size) {
  # Open the NetCDF file
  nc_data <- nc_open(nc_file)  
  print("Dimensions in the NetCDF file:")
  print(nc_data$dim)  
  dims <- nc_data$var[[var_name]]$size
  print(dims)  
  date_dim <- nc_data$dim$date
  if (is.null(date_dim)) {
    stop("Date dimension not found in the NetCDF file.")
  }
  date_var <- ncvar_get(nc_data, date_dim$name)  
  yearly_averages <- list()  
  num_chunks <- ceiling(dims[1] / chunk_size)  
  for (i in 1:num_chunks) {
    print(paste("Processing chunk", i, "of", num_chunks)) #Very slow loop, ~1 row a second 
    start_row <- (i - 1) * chunk_size + 1
    end_row <- min(i * chunk_size, dims[1])
    count <- end_row - start_row + 1    
    start <- c(start_row, rep(1, length(dims) - 1))
    count <- c(count, dims[-1])    
    chunk <- ncvar_get(nc_data, var_name, start = start, count = count)    
    yearly_averages <- process_chunk(chunk, date_var[start_row:end_row], yearly_averages)
    if (i == 1) { #for debugging
      break
    }
  }
  # Close the NetCDF file
  nc_close(nc_data)
  print("closed")
  return(yearly_averages)
}

# Function to plot the yearly average temperatures
plot_yearly_averages <- function(yearly_averages) {
  valid_years <- !is.na(names(yearly_averages))
  yearly_averages <- yearly_averages[valid_years]
  df <- data.frame(
    year = as.numeric(names(yearly_averages)),
    temperature = sapply(yearly_averages, mean, na.rm = TRUE)
  )
  print("Yearly averages data frame:")
  print(df)
  if (nrow(df) == 0) {
    print("Data frame is empty. No data to plot.")
    return(NULL)
  }

  # Create the line graph with white background and specified y-axis range
  p <- ggplot(df, aes(x = year, y = temperature)) +
    geom_line() +
    labs(title = "Yearly Average Temperature", x = "Year", y = "Temperature") +
    theme_minimal(base_size = 15) +
    theme(panel.background = element_rect(fill = "white", color = NA)) +
    scale_y_continuous(limits = c(y_min, y_max))

  print("Plotting the graph...")
  print(p)

  # Save the plot to a file
  ggsave("yearly_average_temperature.png", plot = p)

  return(p)
}
