library(ncdf4)
ph <- "C:\\Users\\Student\\Desktop\\Dissertation\\Data related\\data-raw" #change
nc_file <- paste0(ph, "\\JUL_P_1940_2024.nc") #change
nc_data <- nc_open(nc_file)
print("nc file loaded")

variables <- names(nc_data$var)
print(variables)
varinv <- "tp" #change
var <- ncvar_get(nc_data, varinv)
dimvar <- dim(var)
var_def <- nc_data$var[[varinv]]
print(dimvar)
dim_names <- sapply(var_def$dim, function(x) x$name)
print(dim_names)
precip_units <- ncatt_get(nc_data, varinv, "units")


print("##########head of variable##########")
print(head(var))
for (dim_name in dim_names) {
  dim_values <- ncvar_get(nc_data, dim_name)
  cat("Values for dimension", dim_name, ":\n")
  print(head(dim_values))  
}
print(precip_units)

# tips for time variables
# dates are often stored as YYYYMMDD
# or they get stored as unix centred on Jan 1st Midnight 1970, where 1 is the second after then and -1 the second before.



nc_close(nc_data)
print("ncdf file closed")