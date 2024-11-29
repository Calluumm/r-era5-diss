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

ph <- "PATH"
file_path <- paste0(ph, "FILENAME")
nc_data <- nc_open(file_path)

time <- ncvar_get(nc_data, "date")
time <- as.Date(as.character(time), format="%Y%m%d")
print(paste("Time dimension:", length(time)))

lat <- ncvar_get(nc_data, "latitude")
lon <- ncvar_get(nc_data, "longitude")
temp <- ncvar_get(nc_data, "t")
print(paste("Latitude dimension:", length(lat)))
print(paste("Longitude dimension:", length(lon)))
print(paste("Temperature dimensions:", dim(temp)))

lat <- sort(lat)
lon <- sort(lon)
print(dim(temp))  

temp <- aperm(temp, c(3, 2, 1))
temp_mean <- apply(temp, c(2, 3), mean, na.rm=TRUE)
temp_anomalies <- sweep(temp, c(2, 3), temp_mean)
temp_matrix <- matrix(temp_anomalies, nrow=length(time), ncol=length(lat) * length(lon))
print(paste("Reshaped matrix dimensions:", dim(temp_matrix)))
svd_result <- svd(temp_matrix, nu=0)
eof_modes <- svd_result$v
principal_components <- temp_matrix %*% eof_modes

eof1 <- matrix(eof_modes[,1], nrow=length(lat), ncol=length(lon))
eof1_df <- data.frame(
  lon = rep(lon, each = length(lat)),
  lat = rep(lat, times = length(lon)),
  eof1 = as.vector(t(eof1))
)
# First EOF
png("first_eof_mode.png", width = 1800, height = 900)
eof1 <- matrix(eof_modes[,1], nrow=length(lat), ncol=length(lon))
image.plot(lon, lat, t(eof1), main="First EOF Mode", xlab="Longitude", ylab="Latitude")
map("world", add=TRUE, col="black")
dev.off()
png("second_eof_mode.png", width = 1800, height = 900)
eof1 <- matrix(eof_modes[,2], nrow=length(lat), ncol=length(lon))
image(lon, lat, t(eof1), main="Second EOF mode", xlab="Longitude", ylab="Latitude")
map("world", add=TRUE, col="black")
dev.off()
png("third_eof_mode.png", width = 1800, height = 900)
eof1 <- matrix(eof_modes[,3], nrow=length(lat), ncol=length(lon))
image(lon, lat, t(eof1), main="Third EOF Mode", xlab="Longitude", ylab="Latitude")
map("world", add=TRUE, col="black")
dev.off()
# First PC
png("first_principal_component.png")
plot(time, principal_components[,1], type="l", main="First Principal Component", xlab="Time", ylab="Amplitude")
dev.off()

nc_close(nc_data)
stop("stopped")
