library(ncdf4)
library(stats)
library(pracma)
library(ggplot2)
source("Functions.r")
source("variables.r")
#employ parallel processing this is so slow i might cry

ph <- "FILE PATH"
vr <- c("2m_temperature")
yr <- 2018
mn <- sprintf("0%d",1:1)
dy <- c(sprintf("0%d",1:9),10:31)
tm <- 00:00
are <- "77.22/15.63/77.23/15.64"
cds.key <- "API KEY"
wf_set_key(user ="USERNAME", key = cds.key)
# ALL THESE VARIABLES ARE EXAMPLES CHANGE BASED ON WHAT YOU WANT#

choice <- readline(prompt = "Choose an option (1: era5_data, 2: era5_land_data, 3: continue without downloading): ")
if (choice == "1") {
  download_era5_data(yr, mn, tm, vr, ph, are)
} else if (choice == "2") {
  download_era5_land_data(yr, mn, tm, dy, vr, ph, are)
} else if (choice == "3") {
  cat("Continuing without downloading...\n")
} else {3
  cat("Invalid choice. Continuing without downloading...\n")
}

print("Starting to process the NetCDF file...")
nc_data <- nc_open("FILE PATH")
# to reference the file you just downloaded you'll want to have this dir *.nc
variables <- names(nc_data$var)
print(variables)

choice2 <- readline(prompt = "Which Dataset are you using, (1: era5_data, 2: era5_land_data): ")
if (choice2 == "1") {
  valid_time <- ncvar_get(nc_data, "date")
} else if (choice2 == "2") {
  valid_time <- ncvar_get(nc_data, "valid_time")
}
valid_time_subset <- valid_time

if (!vr %in% names(variable_mappings)) {
  stop("Invalid variable name. Please check the variable_mappings.")
}

retrieved_data <- ncvar_get(nc_data, variable_mappings[[vr]])
nc_close(nc_data)
print(retrieved_data)


df <- data.frame(time = valid_time_subset, value = retrieved_data)


title = paste(vr, "in", yr)
y = paste(vr)
plot <- ggplot(df, aes(x = time, y = value)) +
  geom_line() +
  labs(title = title, x = "Time", y = y) +
  theme_minimal()

filename <- paste0(vr, "plot_", yr, ".png")
ggsave(filename, plot = plot, width = 10, height = 6, dpi =300)
print("saved")
stop("stopped")
