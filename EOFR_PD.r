#install remaining packages if using as a standalone file
library(fields)
library(ncdf4)
library(stats)
library(pracma)
library(ggplot2)
library(zoo)
library(maps)
library(mapdata)
library(mapproj)
library(RColorBrewer)
source("variables.r")

#data download
# https://cds.climate.copernicus.eu/datasets/reanalysis-era5-single-levels-monthly-means?tab=download
# surface pressure, if using a different pressure type check in variables.r what it's referred to as in the dataset 
# then change line 29 to match, if using surface pressure from above keep it as 'sp'
# 
ph <- "\\FILE_PATH"
file_path <- paste0(ph, "\\FILE_NAME.nc")
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
segmented_colors <- colorRampPalette(c("#ffbf00", "#ff1d1d"))(10)
png("first_eof_mode_Pa_LEGEND_FEB.png", width = 1800, height = 900) #change name
par(mar=c(10, 4, 4, 2) + 0.1) # Increase bottom margin
eof1 <- matrix(eof_modes[,1], nrow=length(lat), ncol=length(lon))
image(lon, lat, t(eof1), main="First EOF Mode", xlab="Longitude", ylab="Latitude", col=segmented_colors)
map("world", add=TRUE, col="black")
image.plot(
  lon, lat, t(eof1),
  legend.only=TRUE,
  col=segmented_colors,
  legend.shrink=1,
  horizontal=TRUE,
  legend.args=list(text="EOF Mode", side=1, line=2),
  axis.args=list(at=seq(min(eof1), max(eof1), length.out=10), labels=round(seq(min(eof1), max(eof1), length.out=10), 3))
)
dev.off()

# png("first_principal_component_Pa_FEB.png")
# plot(time, principal_components[,1], type="l", main="First Principal Component", xlab="Time", ylab="Amplitude")
# dev.off()

if (!is.data.frame(principal_components)) {
  principal_components <- data.frame(time = time, principal_components)
}

principal_components <- principal_components[order(principal_components$time), ]
ph2 <- "\\FILE_PATH" #file_path
file_path2 <- paste0(ph2, "\\FILE_PATH_WITH_UVT.nc") #change to temperature + u + v ncdf
file_path3 <- paste0(ph2, "\\FILE_PATH_WITH_P.nc") #change to precipitation ncdf
nc_data2 <- nc_open(file_path2)
nc_data3 <- nc_open(file_path3)

vr2 <- "2m_temperature" #change to correct temp variable (2m_temperature, temperature, etc)
vr5 <- "total_precipitation"
vr4 <- "u_component_of_wind2"
vr3 <- "v_component_of_wind2"
retrieved_data2 <- ncvar_get(nc_data2, variable_mappings[[vr2]])
retrieved_data2 <- retrieved_data2 - 273.15 #Celcius temperature
retrieved_data3 <- ncvar_get(nc_data2, variable_mappings[[vr3]])
retrieved_data4 <- ncvar_get(nc_data2, variable_mappings[[vr4]])
# retrieved_data3 <- sqrt(retrieved_data3^2 + retrieved_data4^2) #windspeed from u and v components #commented out while I'm checking individual u and v components
retrieved_data5 <- ncvar_get(nc_data3, variable_mappings[[vr5]]) #total precipitation
retrieved_data5 <- retrieved_data5 * 1000 #convert to mm

print("Converted temperature from Kelvin to Celsius.")
nc_close(nc_data2)
# temperature
merged_data <- data.frame(time = principal_components$time, 
                          principal_component = principal_components[,1], 
                          temperature = retrieved_data2)

file_path4 <- paste0(ph2, "\\FILE_NAME.nc") #Change to region to map the coeff of
nc_data4 <- nc_open(file_path4)
retrieved_data6 <- ncvar_get(nc_data4, variable_mappings[[vr2]]) #temperature
retrieved_data6 <- retrieved_data6 - 273.15 #Celcius temperature
time2 <- ncvar_get(nc_data4, "valid_time")
reference_date2 <- as.POSIXct("1970-01-01 00:00:00", tz = "UTC") 
time_converted2 <- reference_date2 + time2
time_converted2 <- as.Date(time_converted2)
print(paste("Time dimension:", length(time_converted2)))

lat2 <- ncvar_get(nc_data4, "latitude")
lon2 <- ncvar_get(nc_data4, "longitude")
temperature2 <- ncvar_get(nc_data4, "t2m")
print(paste("Latitude dimension:", length(lat2)))
print(paste("Longitude dimension:", length(lon2)))
print(paste("Temperature dimensions:", dim(temperature2)))

lat2 <- sort(lat2)
lon2 <- sort(lon2)
print(dim(temperature2))
temperature2 <- aperm(temperature2, c(3, 2, 1))
temp_mean <- apply(temperature2, c(2, 3), mean, na.rm=TRUE)
temp_anomalies <- sweep(temperature2, c(2, 3), temp_mean)
temp_matrix <- matrix(temp_anomalies, nrow=length(time2), ncol=length(lat2) * length(lon2))
print(paste("Reshaped matrix dimensions:", dim(temp_matrix)))
cor_coeff <- apply(temp_matrix, 2, function(x) cor(x, principal_components[,1], use="complete.obs"))
temp_coeff <- matrix(cor_coeff, nrow=length(lat2), ncol=length(lon2))


png("COEFF_HEATMAP_T_FEB.png", width = 1800, height = 900) #Change file name
par(mar=c(5, 4, 4, 2) + 0.1) # push axis label around
image(lon2, lat2, t(temp_coeff), main="Temperature Coefficient", xlab="Longitude", ylab="Latitude", col=segmented_colors)
map("world", add=TRUE, col="black")
image.plot(
  lon, lat, t(temp_coeff),
  legend.only=TRUE,
  col=segmented_colors,
  legend.shrink=1,
  horizontal=TRUE,
  legend.args=list(text="Temperature Coefficient", side=1, line=2),
  axis.args=list(at=seq(min(temp_coeff), max(temp_coeff), length.out=10), labels=round(seq(min(temp_coeff), max(temp_coeff), length.out=10), 3))
)
dev.off()
print("coeff temp heatmat printed")

correlation_coefficient <- cor(merged_data$principal_component, merged_data$temperature)
print(paste("Correlation coefficient (temperature):", correlation_coefficient))

# u component
merged_data_u <- data.frame(time = principal_components$time, 
                            principal_component = principal_components[,1], 
                            u_component = retrieved_data3)

correlation_coefficient_u <- cor(merged_data_u$principal_component, merged_data_u$u_component)
print(paste("Correlation coefficient (u component):", correlation_coefficient_u))

png("pc_and_u_component_against_time.png")
plot(merged_data_u$time, merged_data_u$principal_component, type="l", col="blue", 
     main="1st PC and U Component Time Series", xlab="Time", ylab="Value")
lines(merged_data_u$time, merged_data_u$u_component, col="green")
legend("topright", legend=c("Principal Component", "U Component"), col=c("blue", "green"), lty=1)
dev.off()

# v component
merged_data_v <- data.frame(time = principal_components$time, 
                            principal_component = principal_components[,1], 
                            v_component = retrieved_data4)

correlation_coefficient_v <- cor(merged_data_v$principal_component, merged_data_v$v_component)
print(paste("Correlation coefficient (v component):", correlation_coefficient_v))

png("pc_and_v_component_against_time.png")
plot(merged_data_v$time, merged_data_v$principal_component, type="l", col="blue", 
     main="1st PC and V Component Time Series", xlab="Time", ylab="Value")
lines(merged_data_v$time, merged_data_v$v_component, col="red")
legend("topright", legend=c("Principal Component", "V Component"), col=c("blue", "red"), lty=1)
dev.off()

# precipitation
merged_data_precip <- data.frame(time = principal_components$time, 
                                 principal_component = principal_components[,1], 
                                 precipitation = retrieved_data5)

correlation_coefficient_precip <- cor(merged_data_precip$principal_component, merged_data_precip$precipitation, method = "spearman")
print(paste("Spearman correlation coefficient (precipitation):", correlation_coefficient_precip))

stop("stopped")
