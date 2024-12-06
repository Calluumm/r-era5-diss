# r-era5-diss
r tools for NetCDF4 era5 database analysis

Data from:
https://cds.climate.copernicus.eu/
You need an account to be able to get an api key/user; once you have an account both of these can be found in user profile
Documentation of the ERA5 dataset and how it's formatted with CDF4:
https://confluence.ecmwf.int/display/CKB/ERA5%3A+data+documentation
If on VSC, do install.packages("vscDebugger", repos = "https://manuelhentschel.r-universe.dev")
inside your R console

Packaging.r should be ran before running main.
main.r has variables at the top to specify both for download and graph formatting

Whole thing is unfinished, looking to add further plotting measures, PCA/EFO features and more fluidity from download -> formatting, parallel processing
for the larger part of the dataset that requires more speed, mapping of the data onto shapefiles with sf (raster discontinued)

For EOF of Pressure values as a standalone file run packaging.r then EOFR_PD.r will be able to produce images like below
![first_eof_mode_Pa](https://github.com/user-attachments/assets/cabef138-cfbd-4fb6-b43c-dc93d39c9e41)
