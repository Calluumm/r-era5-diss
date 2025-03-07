### Store of Variables ###
# Not fully filled out you may need to add some that I havent got round to
# variable_mappings is a list of the variables in the dataset and their corresponding names in the NetCDF file
# variable_units is a list of the variables in the dataset and their units
variable_mappings <- list(
    divergence = "d",
    fraction_of_cloud_cover = "cc",
    geopotential = "z",
    ozone_mass_mixing_ratio = "o3",
    potential_vorticity = "pv",
    relative_humidity = "r",
    specific_cloud_ice_water_content = "ciwc",
    specific_cloud_liquid_water_content = "clwc",
    specific_humidity = "q",
    specific_rain_water_content = "crwc",
    specific_snow_water_content = "cswc",
    temperature = "t",
    u_component_of_wind = "u",
    v_component_of_wind = "v",
    u_component_of_wind2 = "u10",
    v_component_of_wind2 = "v10",
    vertical_velocity = "w",
    vorticity = "vo",
    "2m_dewpoint_temperature" = "d2m",
    "2m_temperature" = "t2m",
    skin_temperature = "skt",
    soil_temperature_level_1 = "stl1",
    soil_temperature_level_2 = "stl2",
    soil_temperature_level_3 = "stl3",
    soil_temperature_level_4 = "stl4",
    lake_bottom_temperature = "lblt",
    lake_ice_depth = "licd",
    lake_ice_temperature = "lict",
    lake_mix_layer_depth = "lmld",
    lake_mix_layer_temperature = "lmlt",
    lake_shape_factor = "lshf",
    lake_total_layer_temperature = "ltlt",
    snow_albedo = "asn",
    snow_cover = "snc",
    snow_density = "rsn",
    snow_depth = "sd",
    snow_depth_water_equivalent = "", #to be filled lower
    snowfall = "sf",
    snowmelt = "",
    temperature_of_snow_layer = "",
    skin_reservoir_content = "",
    volumetric_soil_water_layer_1 = "",
    volumetric_soil_water_layer_2 = "",
    volumetric_soil_water_layer_3 = "",
    volumetric_soil_water_layer_4 = "",
    forecast_albedo = "",
    surface_latent_heat_flux = "",
    surface_net_solar_radiation = "",
    surface_net_thermal_radiation = "",
    surface_sensible_heat_flux = "",
    surface_solar_radiation_downwards = "",
    surface_thermal_radiation_downwards = "",
    evaporation_from_bare_soil = "",
    evaporation_from_open_water_surfaces_excluding_oceans = "",
    evaporation_from_the_top_of_canopy = "",
    evaporation_from_vegetation_transpiration = "",
    potential_evaporation = "",
    runoff = "",
    snow_evaporation = "",
    sub_surface_runoff = "",
    surface_runoff = "",
    total_evaporation = "",
    "10m_u_component_of_wind" = "",
    "10m_v_component_of_wind" = "",
    surface_pressure = "Pa",
    total_precipitation = "tp",
    leaf_area_index_high_vegetation = "",
    leaf_area_index_low_vegetation = ""
)

variable_units <- list(
    divergence = "s⁻¹",
    fraction_of_cloud_cover = "%",
    geopotential = "m²/s²",
    ozone_mass_mixing_ratio = "kg/kg",
    potential_vorticity = "K m²/kg/s",
    relative_humidity = "%",
    specific_cloud_ice_water_content = "kg/kg",
    specific_cloud_liquid_water_content = "kg/kg",
    specific_humidity = "kg/kg",
    specific_rain_water_content = "kg/kg",
    specific_snow_water_content = "kg/kg",
    temperature = "C",
    u_component_of_wind = "m/s",
    v_component_of_wind = "m/s",
    vertical_velocity = "Pa/s",
    vorticity = "s⁻¹",
    "2m_dewpoint_temperature" = "K",
    "2m_temperature" = "K",
    skin_temperature = "K",
    soil_temperature_level_1 = "K",
    soil_temperature_level_2 = "K",
    soil_temperature_level_3 = "K",
    soil_temperature_level_4 = "K",
    lake_bottom_temperature = "K",
    lake_ice_depth = "m",
    lake_ice_temperature = "K",
    lake_mix_layer_depth = "m",
    total_precipitation = "mm",
    snowfall = "m"
)
######### dataset 1
##"reanalysis-era5-pressure-levels-monthly-means"
###variables
# "divergence"
# "fraction_of_cloud_cover"
# "geopotential"
# "ozone_mass_mixing_ratio"
# "potential_vorticity"
# "relative_humidity"
# "specific_cloud_ice_water_content"
# "specific_cloud_liquid_water_content"
# "specific_humidity"
# "specific_rain_water_content"
# "specific_snow_water_content"
# "temperature"
# "u_component_of_wind"
# "v_component_of_wind"
# "vertical_velocity"
# "vorticity"





#########dataset 2#########
##"reanalysis-era5-land"
###variables
# "2m_dewpoint_temperature",
# "2m_temperature",
# "skin_temperature",
# "soil_temperature_level_1",
# "soil_temperature_level_2",
# "soil_temperature_level_3",
# "soil_temperature_level_4",
# "lake_bottom_temperature",
# "lake_ice_depth",
# "lake_ice_temperature",
# "lake_mix_layer_depth",
# "lake_mix_layer_temperature",
# "lake_shape_factor",
# "lake_total_layer_temperature",
# "snow_albedo",
# "snow_cover",
# "snow_density",
# "snow_depth",
# "snow_depth_water_equivalent",
# "snowfall",
# "snowmelt",
# "temperature_of_snow_layer",
# "skin_reservoir_content",
# "volumetric_soil_water_layer_1",
# "volumetric_soil_water_layer_2",
# "volumetric_soil_water_layer_3",
# "volumetric_soil_water_layer_4",
# "forecast_albedo",
# "surface_latent_heat_flux",
# "surface_net_solar_radiation",
# "surface_net_thermal_radiation",
# "surface_sensible_heat_flux",
# "surface_solar_radiation_downwards",
# "surface_thermal_radiation_downwards",
# "evaporation_from_bare_soil",
# "evaporation_from_open_water_surfaces_excluding_oceans",
# "evaporation_from_the_top_of_canopy",
# "evaporation_from_vegetation_transpiration",
# "potential_evaporation",
# "runoff",
# "snow_evaporation",
# "sub_surface_runoff",
# "surface_runoff",
# "total_evaporation",
# "10m_u_component_of_wind",
# "10m_v_component_of_wind",
# "surface_pressure",
# "total_precipitation",
# "leaf_area_index_high_vegetation",
# "leaf_area_index_low_vegetation"
