# r-era5-diss
r tools for NetCDF4 era5 database analysis

In case others on the same database for dissertation need to start using R
Download R
Change your r.rpath.windows to the R Path (go to your settings.json and add this:
"r.rpath.windows": "FILE PATH"
For example: "r.rpath.windows": "C:\\Program Files\\R\\R-4.1.0\\bin\\R.exe"

Data from:
https://cds.climate.copernicus.eu/
You need an account to be able to get an api key/user; once you have an account both of these can be found in user profile
Documentation of the ERA5 dataset and how it's formatted with CDF4:
https://confluence.ecmwf.int/display/CKB/ERA5%3A+data+documentation

If on VSC, do install.packages("vscDebugger", repos = "https://manuelhentschel.r-universe.dev")
inside your R console
