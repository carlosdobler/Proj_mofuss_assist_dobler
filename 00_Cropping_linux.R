
# Load libraries
library(tidyverse)
library(sf)
library(raster)
library(rgrass7)

# Set master dir
master_dir <- "/media/cdobler/extra_storage/google_drive_ciga/shared/000_MoFuSSCountryDatasets/MoFuSS_Peru_linux/LULCC"

# *****************

# Read parameters table, checking if its delimiter is comma or semicolon
read_csv(glue::glue("{master_dir}/SourceData/Parameters_sc.csv")) %>% 
  {if(is.null(.$ParCHR[1])) read_csv2(glue::glue("{master_dir}/SourceData/Parameters_sc.csv")) else .} -> country_parameters

# Append "+"
country_parameters$ParCHR[1] <- str_c("+", country_parameters$ParCHR[1])
country_parameters$ParCHR[2] <- str_c("+", country_parameters$ParCHR[2])

# *****************

read_csv(glue::glue("{master_dir}/TempTables/Supply_parameters_11cat.csv")) %>% 
  {if(is.null(.$Cobertura[1])) read_csv2(glue::glue("{master_dir}/TempTables/Supply_parameters_11cat.csv")) else .} -> tof_vs_for

# *****************

# Import kml pending (kml missing)
# Section scanning files and choosing pending

# Meanwhile:
# User polygon (gcs)
str_c(master_dir, "/TempVector/userarea_GCS.shp") %>% 
  st_read() %>%
  st_set_crs(country_parameters %>% # obsolete if projection is set beforehand (gpkg)
               filter(Var == "ProjGCS") %>% 
               pull(ParCHR)) -> userarea_GCS

# User polygon (utm)
str_c(master_dir, "/TempVector/userarea.shp") %>% 
  st_read() %>%
  st_set_crs(country_parameters %>% 
               filter(Var == "ProjUTM") %>% 
               pull(ParCHR)) -> userarea

# Extent (latlon)
userarea_GCS %>% 
  st_bbox() -> ext_gcs

# Extent (utm)
userarea %>% 
  st_bbox() -> ext

# Resolution (utm)
str_c(master_dir, "/TempTables/Resolution.csv") %>% 
  read_csv() %>% 
  .[1,2] %>% 
  as.numeric() -> res


# *****************

# Create grass databases

# latlon db
country_parameters %>% 
  filter(Var == "ProjGCS") %>% 
  pull(ParCHR) %>% 
  st_crs() %>% 
  .$epsg -> epsg_gcs

glue::glue("grass -e -c EPSG:{epsg_gcs} {master_dir}/grass/gcs") %>% 
  system()

# utm db
country_parameters %>% 
  filter(Var == "ProjUTM") %>% 
  pull(ParCHR) %>% 
  st_crs() %>% 
  .$epsg -> epsg_utm

glue::glue("grass -e -c EPSG:{epsg_utm} {master_dir}/grass/utm") %>% 
  system()

"grass --config path" %>% system(intern = T) -> pth

# *****************

# DTEM

# Detect projection
country_parameters %>% 
  filter(Var == "DTEM_in_GCS") %>% 
  pull(ParCHR) %>% 
  {if (. == "YES") "gcs" else if (. == "NO") "utm"} -> rast_proj

# Initialize grass
initGRASS(gisBase = pth,
          gisDbase = glue::glue("{master_dir}/grass/"),
          location = rast_proj,
          mapset = "PERMANENT",
          override = T)

# Define extension and resolution
if (rast_proj == "gcs") {
  ext_grass <- ext_gcs
  res_grass <- res * 0.000009
  userarea_grass <- userarea_GCS
} else if (rast_proj == "utm") {
  ext_grass <- ext
  res_grass <- res
  userarea_grass <- userarea
}

# Set region
execGRASS("g.region",
          parameters = list(w = as.character(ext_grass[1]),
                            s = as.character(ext_grass[2]),
                            e = as.character(ext_grass[3]),
                            n = as.character(ext_grass[4]),
                            res = as.character(res_grass)))

# Import dtem
glue::glue("{master_dir}/SourceData/InRaster_GCS/DTEM.tif") %>% 
  {execGRASS("r.in.gdal",
             parameters = list(input = .,
                               output = "dtem"))}

# Set resolution to match dtem's
execGRASS("r.info",
          parameters = list(map = "dtem"),
          flags = "g",
          intern = T) %>% 
  .[5] %>% 
  str_split("=", simplify = T) %>% .[,2] %>%
  
  {execGRASS("g.region",
             parameters = list(res = .))}
  
# Import polygon to crop
userarea_GCS %>% 
  writeVECT("userarea")

# Set mask
execGRASS("r.mask",
          parameters = list(vector = "userarea"))

# Crop
execGRASS("r.mapcalc",
          expression = "dtem = dtem",
          flags = "overwrite")

# Remove mask
execGRASS("r.mask",
          flags = "r")

# Change location (to utm)
rast_proj <- "utm"
initGRASS(gisBase = pth,
          gisDbase = glue::glue("{master_dir}/grass/"),
          location = rast_proj,
          mapset = "PERMANENT",
          override = T)

# Define extension and resolution
if (rast_proj == "gcs") {
  ext_grass <- ext_gcs
  res_grass <- res * 0.000009
  userarea_grass <- userarea_GCS
} else if (rast_proj == "utm") {
  ext_grass <- ext
  res_grass <- res
  userarea_grass <- userarea
}

# Set region
execGRASS("g.region",
          parameters = list(w = as.character(ext_grass[1]),
                            s = as.character(ext_grass[2]),
                            e = as.character(ext_grass[3]),
                            n = as.character(ext_grass[4]),
                            res = as.character(res_grass)))

# Reproject
execGRASS("r.proj",
          parameters = list(location = "gcs",
                            input = "dtem",
                            method = "bilinear"))

# Export
execGRASS("r.out.gdal",
          parameters = list(input = "dtem",
                            output = glue::glue("{master_dir}/TempRaster/DEM_c1_c.tif")))

# *****************

# Treecover, gain, loss year

# Change location (to gcs)
rast_proj <- "gcs"
initGRASS(gisBase = pth,
          gisDbase = glue::glue("{master_dir}/grass/"),
          location = rast_proj,
          mapset = "PERMANENT",
          override = T)

# Define extension and resolution
if (rast_proj == "gcs") {
  ext_grass <- ext_gcs
  res_grass <- res * 0.000009
  userarea_grass <- userarea_GCS
} else if (rast_proj == "utm") {
  ext_grass <- ext
  res_grass <- res
  userarea_grass <- userarea
}

# Set region
execGRASS("g.region",
          parameters = list(w = as.character(ext_grass[1]),
                            s = as.character(ext_grass[2]),
                            e = as.character(ext_grass[3]),
                            n = as.character(ext_grass[4]),
                            res = as.character(res_grass)))

# Import treecover
country_parameters %>% 
  filter(Var == "treecover_name") %>% 
  pull(ParCHR) %>% 
  {glue::glue("{master_dir}/SourceData/InRaster_GCS/{.}")} %>% 
  {execGRASS("r.in.gdal",
             parameters = list(input = .,
                               output = "tc2000"))}

# Import gain
country_parameters %>% 
  filter(Var == "gain_name") %>% 
  pull(ParCHR) %>% 
  {glue::glue("{master_dir}/SourceData/InRaster_GCS/{.}")} %>% 
  {execGRASS("r.in.gdal",
             parameters = list(input = .,
                               output = "gain"))}

# Import loss year
country_parameters %>% 
  filter(Var == "lossyear_name") %>% 
  pull(ParCHR) %>% 
  {glue::glue("{master_dir}/SourceData/InRaster_GCS/{.}")} %>% 
  {execGRASS("r.in.gdal",
             parameters = list(input = .,
                               output = "lossyear"))}

# Set resolution to match Hansen's
execGRASS("r.info",
          parameters = list(map = "tc2000"),
          flags = "g",
          intern = T) %>% 
  .[5] %>% 
  str_split("=", simplify = T) %>% .[,2] %>%
  
  {execGRASS("g.region",
             parameters = list(res = .))}

# Set mask
execGRASS("r.mask",
          parameters = list(vector = "userarea"))

# Crop
execGRASS("r.mapcalc",
          expression = "tc2000 = tc2000",
          flags = "overwrite")

execGRASS("r.mapcalc",
          expression = "gain = gain",
          flags = "overwrite")

execGRASS("r.mapcalc",
          expression = "lossyear = lossyear",
          flags = "overwrite")

# Remove mask
execGRASS("r.mask",
          flags = "r")

# Change location (to utm)
rast_proj <- "utm"
initGRASS(gisBase = pth,
          gisDbase = glue::glue("{master_dir}/grass/"),
          location = rast_proj,
          mapset = "PERMANENT",
          override = T)

# Define extension and resolution
if (rast_proj == "gcs") {
  ext_grass <- ext_gcs
  res_grass <- res * 0.000009
  userarea_grass <- userarea_GCS
} else if (rast_proj == "utm") {
  ext_grass <- ext
  res_grass <- res
  userarea_grass <- userarea
}

# Set region
execGRASS("g.region",
          parameters = list(w = as.character(ext_grass[1]),
                            s = as.character(ext_grass[2]),
                            e = as.character(ext_grass[3]),
                            n = as.character(ext_grass[4]),
                            res = as.character(res_grass)))

# Reproject
execGRASS("r.proj",
          parameters = list(location = "gcs",
                            input = "tc2000",
                            method = "bilinear")) # no data over 100 nor under 0

execGRASS("r.proj",
          parameters = list(location = "gcs",
                            input = "gain",
                            method = "nearest"), flags = "overwrite")

execGRASS("r.proj",
          parameters = list(location = "gcs",
                            input = "lossyear",
                            method = "nearest"), flags = "overwrite")

# Export
execGRASS("r.out.gdal",
          parameters = list(input = "tc2000",
                            output = glue::glue("{master_dir}/TempRaster/tc2000_c.tif")))

execGRASS("r.out.gdal",
          parameters = list(input = "gain",
                            output = glue::glue("{master_dir}/TempRaster/gain_c.tif")))

execGRASS("r.out.gdal",
          parameters = list(input = "lossyear",
                            output = glue::glue("{master_dir}/TempRaster/lossyear_c.tif")))


# *****************

# LULC

# Detect projection
country_parameters %>% 
  filter(Var == "LULCt1map_name") %>%
  pull(ParCHR) %>% 
  {glue::glue("{master_dir}/SourceData/InRaster/{.}")} %>% 
  
  gdal_utils(util = "info", source = .) %>% 
  read_table(col_names = "var") %>% 
  slice(str_which(var, "Coordinate System") + 1) %>% 
  pull(var) %>% 
  {if (str_detect(., "UTM")) "utm" else if (str_detect(., "GEOGCS")) "gcs"} -> proj_map

# Alternative to detect and set proj
# country_parameters %>% 
#   filter(Var == "LULCt1map_name") %>%
#   pull(ParCHR) %>% 
#   {glue::glue("{master_dir}/SourceData/InRaster/{.}")} %>% 
#   
#   gdal_utils(util = "info", source = .) %>% 
#   read_table(col_names = "var") %>% 
#   filter(str_detect(var, "EPSG")) %>% 
#   slice(n()) %>% 
#   pull(var) %>% 
#   parse_number() %>% 
#   st_crs()

# Initialize Grass
initGRASS(gisBase = pth,
          gisDbase = glue::glue("{master_dir}/grass/"),
          location = proj_map,
          mapset = "PERMANENT",
          override = T)

# Define extension and resolution
if (proj_map == "gcs") {
  ext_grass <- ext_gcs
  res_grass <- res * 0.000009
  userarea_grass <- userarea_GCS
} else if (proj_map == "utm") {
  ext_grass <- ext
  res_grass <- res
  userarea_grass <- userarea
}

# Set region
execGRASS("g.region",
          parameters = list(w = as.character(ext_grass[1]),
                            s = as.character(ext_grass[2]),
                            e = as.character(ext_grass[3]),
                            n = as.character(ext_grass[4]),
                            res = as.character(res_grass)))

# Resample
