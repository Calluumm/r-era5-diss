install.packages("fields")
library(fields)
library(ncdf4)
library(stats)
library(pracma)
library(ggplot2)
library(zoo)
library(maps)
library(mapdata)
library(viridis)

ph <- "C:/Users/Student/Desktop/Dissertation/Data related/data-raw"
file_path <- paste0(ph, "/FULLAREA_FEB_U_V_1940-2024.nc")
nc_data <- nc_open(file_path)


time <- ncvar_get(nc_data, "valid_time")  
time <- as.Date(as.character(time), format="%Y%m%d")
print(paste("Time dimension:", length(time)))

lat <- ncvar_get(nc_data, "latitude")
lon <- ncvar_get(nc_data, "longitude")
u <- ncvar_get(nc_data, "u")
v <- ncvar_get(nc_data, "v")
print(paste("Latitude dimension:", length(lat)))
print(paste("Longitude dimension:", length(lon)))
print(paste("U component dimensions:", dim(u)))
print(paste("V component dimensions:", dim(v)))

lat <- sort(lat)
lon <- sort(lon)
print(dim(u))
print(dim(v))

# Calculate wind speed
wind_speed <- sqrt(u^2 + v^2)
wind_speed <- aperm(wind_speed, c(3, 2, 1))
wind_mean <- apply(wind_speed, c(2, 3), mean, na.rm=TRUE)
wind_anomalies <- sweep(wind_speed, c(2, 3), wind_mean)
wind_matrix <- matrix(wind_anomalies, nrow=length(time), ncol=length(lat) * length(lon))
print(paste("Reshaped matrix dimensions:", dim(wind_matrix)))
svd_result <- svd(wind_matrix, nu=0)
eof_modes <- svd_result$v
principal_components <- wind_matrix %*% eof_modes

eof1 <- matrix(eof_modes[,1], nrow=length(lat), ncol=length(lon))
eof1_df <- data.frame(
  lon = rep(lon, each = length(lat)),
  lat = rep(lat, times = length(lon)),
  eof1 = as.vector(t(eof1))
)
# First EOF
png("first_eof_mode.png", width = 1800, height = 900)
eof1 <- matrix(eof_modes[,1], nrow=length(lat), ncol=length(lon))
image.plot(lon, lat, t(eof1), main="First EOF Mode of Wind Components", xlab="Longitude", ylab="Latitude")
map("world", add=TRUE, col="black")
dev.off()
png("second_eof_mode.png", width = 1800, height = 900)
eof1 <- matrix(eof_modes[,2], nrow=length(lat), ncol=length(lon))
image(lon, lat, t(eof1), main="Second EOF mode of Wind Components", xlab="Longitude", ylab="Latitude")
map("world", add=TRUE, col="black")
dev.off()
png("third_eof_mode.png", width = 1800, height = 900)
eof1 <- matrix(eof_modes[,3], nrow=length(lat), ncol=length(lon))
image(lon, lat, t(eof1), main="Third EOF Mode of Wind Components", xlab="Longitude", ylab="Latitude")
map("world", add=TRUE, col="black")
dev.off()
# First PC
png("first_principal_component.png")
plot(time, principal_components[,1], type="l", main="First Principal Component", xlab="Time", ylab="Amplitude")
dev.off()

nc_close(nc_data)
stop("stopped")