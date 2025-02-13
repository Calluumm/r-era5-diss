source("variables.r")

ph <- "C:\\FILE_PATH\\"
#Follows my naming scheme, fit to whatever needed
MON <- "FEB"
file_path <- paste0(ph, "\\", MON,"_UVT_1940_2024.nc") # temp, u component and v component file; take from single levels
nc_data <- nc_open(file_path)
file_path2 <- paste0(ph, "\\", MON,"_P_1940_2024.nc") # precipitation file
file_path3 <- paste0(ph, "\\", MON,"_S_1940_2024.nc") # snowfall file
nc_data2 <- nc_open(file_path2)
nc_data3 <- nc_open(file_path3)

vr <- c("2m_temperature")
vr2 <- c("total_precipitation")
vr3 <- c("snowfall")
vr4 <- c("v_component_of_wind2")
vr5 <- c("u_component_of_wind2")

retrieved_data <- ncvar_get(nc_data, variable_mappings[[vr]])
retrieved_data <- retrieved_data - 273.15 #Celcius temperature
retrieved_data2 <- ncvar_get(nc_data2, variable_mappings[[vr2]])
retrieved_data3 <- ncvar_get(nc_data3, variable_mappings[[vr3]])
retrieved_data_combined <- (retrieved_data2 + retrieved_data3)* 1000 * 28 #mm precipitation
retrieved_data4 <- ncvar_get(nc_data, variable_mappings[[vr4]])
retrieved_data5 <- ncvar_get(nc_data, variable_mappings[[vr5]])

t_test_temp <- t.test(retrieved_data)
t_test_precip <- t.test(retrieved_data_combined)
t_test_wind_v <- t.test(retrieved_data4)
t_test_wind_u <- t.test(retrieved_data5)

print(t_test_temp)
print(t_test_precip)
print(t_test_wind_v)
print(t_test_wind_u)
stop("stopped")

