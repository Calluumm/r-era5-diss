library(foreach)
library(ecmwfr)
ph <- "PATH"
vr <- c("2m_temperature")
yr <- 2018
mn <- sprintf("0%d",1:2)
dy <- c(sprintf("0%d",1:9),10:31)
tm <- c(paste0("0",0:9,":00"), paste0(10:23,":00"))
cds.key <- "KEY HERE"
wf_set_key(user ="USER HERE", key = cds.key)


for (i in 1:length(yr)) {
  for (j in 1:length(mn)) {
    custome_file_name <- paste0("era5_", vr[1], "_", yr[i],"_",mn[j],".nc")
    request <- list(
      "dataset_short_name" = "reanalysis-era5-land",
      "product_type" = "reanalysis",
      "format" = "netcdf",
      "variable" = vr[1],
      "year" = yr[i],
      "month" = mn[j],
      "day" = dy,
      "time" = tm,
      "area" = "81/5/75/35",
      "format" = "netcdf",
      "target" = custome_file_name
    )
    
    file <- wf_request(
      user     = "USER HERE",   # user ID (for authentification)
      request  = request,  # the request
      transfer = TRUE,     # download the file
      path     = ph,       # store data in current working directory
      verbose = TRUE
    )
    
  }
}


source("collate_era5.R")
source("extract_era5.R")
source("monthmeans_era5.R")
source("utility.R")
#ERA5Handlers
#devtools::install_github("MRPHarris/ERA5handlers")
#might be outdated didnt work for me so I manually import them from my file folder


dat_store_era5 <- "FILE PATH HERE"
era5_fnames <- list.files(dat_store_era5, full.names = TRUE)
queried <- collate_era5(era5_fnames, string = custome_file_name, coords = c(81.00, 75.00))
head(queried)

PH_temp_monthmeans <- monthmeans_era5(queried, total_months = TRUE) %>%
  mutate(month_varmean = monthvarmean - 273.15)

sd_polygon_avtemp <- data.frame(x = c(rep(PH_temp_monthmeans$month),
                                      rev(rep(PH_temp_monthmeans$month))),
                                y = c(PH_temp_monthmeans$month_varmean + PH_temp_monthmeans$month_varsd,
                                      rev(PH_temp_monthmeans$month_varmean - PH_temp_monthmeans$month_varsd)))

month_labs <- c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")
ggplot() + 
  geom_ploygon(data = sd_polygon_avtemp, aes(x = x, y = y), fill = "grey60", color = "NA", alpha = 0.2) +
  geom_line(data = PH_temp_monthmeans, aes(x = month, y= month_varmean)) +
  geom_point(data = PH_temp_monthmeans, aes(x = month, y = month_varmean)) +

  geom_line(data = PH_temp_monthmeans, aes(x = month, y = month_varmean + month_varsd), linetype = "dashed") +
  geom_point(data = PH_temp_monthmeans, aes(x = month, y = month_varmean + month_varsd), shape = 4) +

  geom_line(data = PH_temp_monthmeans, aes(x = month, y = month_varmean - month_varsd), linetype = "dashed") +
  geom_point(data = PH_temp_monthmeans, aes(x = month, y = month_varmean - month_varsd), shape = 4) +

  scale_x_continuous(breaks = seq(1,12,1), labels = month_labs, expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits = c(-40,-5), breaks = seq(-40,-5,5)) +
  labs(x = "month", y = "Temp in C") +
  ggtitle('ERA5 mean monthly 2m temperature during 2000 for COORDS') +
  theme_cowplot(12)
