
# Load libraries
library(tidyverse)
library(sf)
library(raster)
library(rgrass7)

# Set master dir
master_dir <- "/media/cdobler/extra_storage/google_drive_ciga/shared/000_MoFuSSCountryDatasets/MoFuSS_Peru_linux/LULCC"

# *****************

# Read parameters table, checking if its delimiter is comma or semicolon
read_csv(glue::glue("{master_dir}/SourceData/Parameters.csv")) %>% 
  {if(is.null(.$ParCHR[1])) read_csv2(glue::glue("{master_dir}/SourceData/Parameters.csv")) else .} -> country_parameters

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
# User polygon
str_c(master_dir, "/TempVector/userarea.shp") %>% 
  st_read() %>%
  st_set_crs(country_parameters %>% 
               filter(Var == "ProjUTM") %>% 
               pull(ParCHR)) -> userarea

# Extent
userarea %>% 
  st_bbox() -> ext

# Resolution
str_c(master_dir, "/TempTables/Resolution.csv") %>% 
  read_csv() %>% 
  .[1,2] %>% 
  as.numeric() -> res

# *****************

# Create grass database
country_parameters %>% 
  filter(Var == "ProjUTM") %>% 
  pull(ParCHR) %>% 
  st_crs() %>% 
  .$epsg -> epsg_utm

glue::glue("grass -e -c EPSG:{epsg_utm} {master_dir}/grass/utm") %>% 
  system()

"grass --config path" %>% system(intern = T) -> pth

# Initialize grass
initGRASS(gisBase = pth,
          gisDbase = glue::glue("{master_dir}/grass/"),
          location = "utm",
          mapset = "PERMANENT",
          override = T)

# Set region
execGRASS("g.region",
          parameters = list(w = as.character(ext[1]),
                            s = as.character(ext[2]),
                            e = as.character(ext[3]),
                            n = as.character(ext[4]),
                            res = as.character(res)),
          flags = "a")

# Import polygon to crop
userarea %>% 
  writeVECT("userarea")

# *****************

# DTEM
# Import dtem
glue::glue("{master_dir}/SourceData/InRaster/DTEM.tif") %>% 
  {execGRASS("r.in.gdal",
             parameters = list(input = .,
                               output = "dtem"),
             flags = "overwrite")}

# Set mask
execGRASS("r.mask",
          parameters = list(vector = "userarea"))

# Resample
execGRASS("r.resamp.interp",
          parameters = list(input = "dtem",
                            output = "dtem",
                            method = "bilinear"),
          flags = "overwrite")

# Remove mask
execGRASS("r.mask",
          flags = "r")

# Export
execGRASS("r.out.gdal",
          parameters = list(input = "dtem",
                            output = glue::glue("{master_dir}/TempRaster/DTEM_c.tif")),
          flags = "c")

# *****************

# Hansen

# Import treecover
country_parameters %>% 
  filter(Var == "treecover_name") %>% 
  pull(ParCHR) %>% 
  {glue::glue("{master_dir}/SourceData/InRaster/{.}")} %>% 
  {execGRASS("r.in.gdal",
             parameters = list(input = .,
                               output = "tc2000"),
             flags = "overwrite")}

# Import gain
country_parameters %>% 
  filter(Var == "gain_name") %>% 
  pull(ParCHR) %>% 
  {glue::glue("{master_dir}/SourceData/InRaster/{.}")} %>% 
  {execGRASS("r.in.gdal",
             parameters = list(input = .,
                               output = "gain"),
             flags = "overwrite")}

# Import loss year
country_parameters %>% 
  filter(Var == "lossyear_name") %>% 
  pull(ParCHR) %>% 
  {glue::glue("{master_dir}/SourceData/InRaster/{.}")} %>% 
  {execGRASS("r.in.gdal",
             parameters = list(input = .,
                               output = "lossyear"),
             flags = "overwrite")}

# Set mask
execGRASS("r.mask",
          parameters = list(vector = "userarea"))

# Resample
execGRASS("r.resamp.interp",
          parameters = list(input = "tc2000",
                            output = "tc2000",
                            method = "bilinear"),
          flags = "overwrite")

execGRASS("r.resamp.interp",
          parameters = list(input = "gain",
                            output = "gain",
                            method = "nearest"),
          flags = "overwrite")

execGRASS("r.resamp.interp",
          parameters = list(input = "lossyear",
                            output = "lossyear",
                            method = "nearest"),
          flags = "overwrite")

# Remove mask
execGRASS("r.mask",
          flags = "r")

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

# Import map
country_parameters %>% 
  filter(Var == "LULCt1map_name") %>%
  pull(ParCHR) %>% 
  {glue::glue("{master_dir}/SourceData/InRaster/{.}")} %>% 
  {execGRASS("r.in.gdal",
             parameters = list(input = .,
                               output = "lulc_t1"),
             flags = "overwrite")}
  
# Set mask
execGRASS("r.mask",
          parameters = list(vector = "userarea"))

# Resample
execGRASS("r.resamp.interp",
          parameters = list(input = "lulc_t1",
                            output = "lulc_t1",
                            method = "nearest"),
          flags = "overwrite")

# Remove mask
execGRASS("r.mask",
          flags = "r")

# Reclassify with TOF
tof_vs_for %>% 
  mutate(TOF_c = str_c(Value, " = ", TOF)) %>% 
  dplyr::select(TOF_c) %>% 
  write_csv(glue::glue("{master_dir}/TempRaster/rules.txt"), col_names = F)

execGRASS("r.reclass",
          parameters = list(input = "lulc_t1",
                            output = "lulc_t1_tof",
                            rules = glue::glue("{master_dir}/TempRaster/rules.txt")),
          flags = "overwrite")

unlink(glue::glue("{master_dir}/TempRaster/rules.txt"))

# Export
execGRASS("r.out.gdal",
          parameters = list(input = "lulc_t1",
                            output = glue::glue("{master_dir}/TempRaster/lulc_t1_c.tif")),
          flags = "c")

execGRASS("r.out.gdal",
          parameters = list(input = "lulc_t1_tof",
                            output = glue::glue("{master_dir}/TempRaster/lulc_t1_tof_c.tif")),
          flags = "c")

# Pending: check existence of t2 and t3

# *****************

# Demand pending

# *****************

# IDW pending



