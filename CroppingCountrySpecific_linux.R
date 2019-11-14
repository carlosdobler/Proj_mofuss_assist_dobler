
# Load libraries
library(tidyverse)
library(sf)
library(raster)
library(rgrass7)

# Set master dir
master_dir <- "/media/cdobler/extra_storage/google_drive_ciga/shared/000_MoFuSSCountryDatasets/MoFuSS_Peru_linux/LULCC"

# *****************

# Read country mask
glue::glue("{master_dir}/SourceData/InVector/country_mask.gpkg") %>% # I manually created and placed this file here
  st_read() -> country_mask

# Alternatively:
glue::glue("{master_dir}/SourceData/InVector/Extent_Mask.shp") %>%
  st_read() -> country_mask

# *****************

# Read parameters table, checking if its delimiter is comma or semicolon
read_csv(glue::glue("{master_dir}/SourceData/Parameters_sc.csv")) %>% 
  {if(is.null(.$ParCHR[1])) read_csv2(glue::glue("{master_dir}/SourceData/Parameters_sc.csv")) else .} -> country_parameters

# Append "+"
country_parameters$ParCHR[1] <- str_c("+", country_parameters$ParCHR[1])
country_parameters$ParCHR[2] <- str_c("+", country_parameters$ParCHR[2])

# *****************

# Import shapefiles and additional parameters (why aren't they in the parameters table?)

# User polygon (latlon)
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
  
# Pending:
# DEM_c1<-raster("TempRaster/DEM_c1.tif")
# ProjExtent<-projectExtent(DEM_c1, UTMproj) # Shouldn't be the same as "ext"
# analysis_r<-raster("TempRaster/mask_c.tif") # Doesn't exist

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

# Raster data

# Demand raster is missing
# Builtin_x raster is missing

# AGB

# Detect projection
country_parameters %>% 
  filter(Var == "AGB_in_GCS") %>% 
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

# Import agb
country_parameters %>% 
  filter(Var == "AGB_name") %>% 
  pull(ParCHR) %>% 
  {glue::glue("{master_dir}/SourceData/InRaster/{.}")} %>% 
  {execGRASS("r.in.gdal",
             parameters = list(input = .,
                               output = "agb"))}

# Import polygon to crop
userarea %>% 
  writeVECT("userarea")

# Rasterize polygon
execGRASS("v.to.rast",
          parameters = list(input = "userarea",
                            output = "userarea",
                            use = "val",
                            value = 1))

# Crop
execGRASS("r.mapcalc",
          expression = "agb = if(userarea == 1, agb)",
          flags = "overwrite")

# Export
execGRASS("r.out.gdal",
          parameters = list(input = "agb",
                            output = glue::glue("{master_dir}/TempRaster/agb_c.tif")))
          

# *****************

# Vector data

# Roads and rivers are in latlon - this is not in the parameters table
glue::glue("{master_dir}/SourceData/InVector_GCS/rivers_mod_dobler.gpkg") %>% 
  st_read() -> rivers
  
rivers %>% 
  st_crs() %>% 
  .$proj4string %>% 
  str_detect("longlat") %>% 
  {if (. == T) "gcs" else if (. == F) "utm"} -> rast_proj

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

rivers %>% 
  writeVECT("rivers")

roads %>% 
  writeVECT("roads")

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

# Set mask
execGRASS("r.mask",
          parameters = list(vector = "userarea"))

# Reproject rivers
execGRASS("v.proj", 
          parameters = list(location = "gcs",
                            input = "rivers"))

# Rasterize
execGRASS("v.to.rast",
          parameters = list(input = "rivers",
                            output = "rivers",
                            use = "attr",
                            attribute_column = country_parameters %>% 
                              filter(Var == "rivers_name_ID") %>% 
                              .$ParCHR))

# Reproject roads
execGRASS("v.proj", 
          parameters = list(location = "gcs",
                            input = "roads"))

# Rasterize
execGRASS("v.to.rast",
          parameters = list(input = "roads",
                            output = "roads",
                            use = "attr",
                            attribute_column = country_parameters %>% 
                              filter(Var == "roads_name_ID") %>% 
                              .$ParCHR))

# Remove mask
execGRASS("r.mask",
          flags = "r")

# Export rivers
execGRASS("r.out.gdal",
          parameters = list(input = "rivers",
                            output = glue::glue("{master_dir}/TempRaster/rivers_c.tif")))

# Export roads
execGRASS("r.out.gdal",
          parameters = list(input = "roads",
                            output = glue::glue("{master_dir}/TempRaster/roads_c.tif")))




