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
source("variables.r") #ensure variables.r is in the same directory

#data download
# https://cds.climate.copernicus.eu/datasets/reanalysis-era5-single-levels-monthly-means?tab=download
# surface pressure, if using a different pressure type check in variables.r what it's referred to as in the dataset 
# then change line 29 to match, if using surface pressure from above keep it as 'sp'
# 
ph <- "\\FILE_PATH"
file_path <- paste0(ph, "\\FILE NAME.nc") #THis should be the EOF calculating file_path so include the whole observable region (for AO 20 degrees and North)
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
segmented_colors <- (c("#0404ac", "#0303cc", "#0202fa", "#1313ff", "#3c3cff", "#7070ff","#8a8afc","#babaff", "white", "#ff5454")) #Change colour scheme to match eof
png("first_eof_mode_Pa_LEGEND_FEB.png", width = 1800, height = 900)
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

png("first_principal_component_Pa_FEB.png")
plot(time, principal_components[,1], type="l", main="First Principal Component", xlab="Time", ylab="Amplitude")
dev.off()

if (!is.data.frame(principal_components)) {
  principal_components <- data.frame(time = time, principal_components)
}

principal_components <- principal_components[order(principal_components$time), ]
ph2 <- "\\FILE_PATH"
file_path2 <- paste0(ph2, "\\FILE_NAME.nc") #nc containing U/V wind data and Temperature data from single levels cds
file_path3 <- paste0(ph2, "\\FILE_NAME.nc") #nc containing precipitation data from single levels cds 
#Both above should be single point locations not areas
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
retrieved_data3 <- sqrt(retrieved_data3^2 + retrieved_data4^2) #windspeed from u and v components
retrieved_data5 <- ncvar_get(nc_data3, variable_mappings[[vr5]]) #total precipitation
retrieved_data5 <- retrieved_data5 * 1000 #convert to mm

print("Converted temperature from Kelvin to Celsius.")
nc_close(nc_data2)
# temperature
merged_data <- data.frame(time = principal_components$time, 
                          principal_component = principal_components[,1], 
                          temperature = retrieved_data2)

correlation_coefficient <- cor(merged_data$principal_component, merged_data$temperature)
print(paste("Correlation coefficient (temperature):", correlation_coefficient))

#windspeed unit
merged_data_wind <- data.frame(time = principal_components$time, 
                               principal_component = principal_components[,1], 
                               windspeed = retrieved_data3)

correlation_coefficient_wind <- cor(merged_data_wind$principal_component, merged_data_wind$windspeed)
print(paste("Correlation coefficient (windspeed):", correlation_coefficient_wind))

# precipitation
merged_data_precip <- data.frame(time = principal_components$time, 
                                 principal_component = principal_components[,1], 
                                 precipitation = retrieved_data5)

correlation_coefficient_precip <- cor(merged_data_precip$principal_component, merged_data_precip$precipitation)
print(paste("Correlation coefficient (precipitation):", correlation_coefficient_precip))

stop("stopped")
