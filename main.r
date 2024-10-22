library(ncdf4)
library(stats)
library(pracma)
library(ggplot2)
source("Functions.r")
#employ parallel processing this is so slow i might cry

nc_file <- "fc8f2337e0cc6e9dafa39b57484565a1.nc"
var_name <- "t"
chunk_size <- 50

print("Starting to process the NetCDF file...")

yearly_averages <- process_nc_chunks(nc_file, var_name, chunk_size)

print("Finished processing the NetCDF file.")
print("Yearly averages:")
print(yearly_averages)

# Plot the yearly average temperatures
plot <- plot_yearly_averages(yearly_averages)

if (is.null(plot)) {
  print("No plot was generated.")
} else {
  print("Plot generated successfully.")
}