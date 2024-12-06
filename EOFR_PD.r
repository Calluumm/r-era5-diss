install.packages("fields")
#install remaining packages if using as a standalone file
library(fields)
library(ncdf4)
library(stats)
library(pracma)
library(ggplot2)
library(zoo)
library(maps)
library(mapdata)

#data download
# https://cds.climate.copernicus.eu/datasets/reanalysis-era5-single-levels-monthly-means?tab=download
# surface pressure, if using a different pressure type check in variables.r what it's referred to as in the dataset 
# then change line 29 to match, if using surface pressure from above keep it as 'sp'
# 
ph <- "C:\\Users\\Student\\Desktop\\Dissertation\\Data related\\data-raw"
file_path <- paste0(ph, "\\FULLAREA_FEB_Pa_1940_2024.nc")
nc_data <- nc_open(file_path)

# Check how the dataset is handling dates, it's shown me several different ones by now
time <- ncvar_get(nc_data, "valid_time")
reference_date <- as.POSIXct("1970-01-01 00:00:00", tz = "UTC") 
time_converted <- reference_date + time
time_converted <- as.Date(time_converted)
print(paste("Time dimension:", length(time_converted)))

lat <- ncvar_get(nc_data, "latitude")
lon <- ncvar_get(nc_data, "longitude")
pres <- ncvar_get(nc_data, "sp")
print(paste("Latitude dimension:", length(lat)))
print(paste("Longitude dimension:", length(lon)))
print(paste("Pressure dimensions:", dim(pres)))

lat <- sort(lat)
lon <- sort(lon)
print(dim(pres))  

pres <- aperm(pres, c(3, 2, 1))
pres_mean <- apply(pres, c(2, 3), mean, na.rm=TRUE)
pres_anomalies <- sweep(pres, c(2, 3), pres_mean)
pres_matrix <- matrix(pres_anomalies, nrow=length(time), ncol=length(lat) * length(lon))
print(paste("Reshaped matrix dimensions:", dim(pres_matrix)))
svd_result <- svd(pres_matrix, nu=0)
eof_modes <- svd_result$v
principal_components <- pres_matrix %*% eof_modes

eof1 <- matrix(eof_modes[,1], nrow=length(lat), ncol=length(lon))
eof1_df <- data.frame(
  lon = rep(lon, each = length(lat)),
  lat = rep(lat, times = length(lon)),
  eof1 = as.vector(t(eof1))
)
# First EOF
png("first_eof_mode_Pa.png", width = 1800, height = 900)
eof1 <- matrix(eof_modes[,1], nrow=length(lat), ncol=length(lon))
image(lon, lat, t(eof1), main="First EOF Mode", xlab="Longitude", ylab="Latitude")
map("world", add=TRUE, col="black")
dev.off()
png("second_eof_mode_Pa.png", width = 1800, height = 900)
eof1 <- matrix(eof_modes[,2], nrow=length(lat), ncol=length(lon))
image(lon, lat, t(eof1), main="Second EOF mode", xlab="Longitude", ylab="Latitude")
map("world", add=TRUE, col="black")
dev.off()
png("third_eof_mode_Pa.png", width = 1800, height = 900)
eof1 <- matrix(eof_modes[,3], nrow=length(lat), ncol=length(lon))
image(lon, lat, t(eof1), main="Third EOF Mode", xlab="Longitude", ylab="Latitude")
map("world", add=TRUE, col="black")
dev.off()
# First PC
png("first_principal_component_Pa.png")
plot(time, principal_components[,1], type="l", main="First Principal Component", xlab="Time", ylab="Amplitude")
dev.off()
png("second_principal_component_Pa.png")
plot(time, principal_components[,2], type="l", main="Second Principal Component", xlab="Time", ylab="Amplitude")
dev.off()
png("third_principal_component_Pa.png")
plot(time, principal_components[,3], type="l", main="Third Principal Component", xlab="Time", ylab="Amplitude")
dev.off()
nc_close(nc_data)
stop("stopped")