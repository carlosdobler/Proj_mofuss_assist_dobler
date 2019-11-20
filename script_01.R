
library(tidyverse)
library(sf)

import_dir <- "/media/cdobler/extra_storage/google_drive_ciga/shared/000_MoFuSSCountryDatasets/"


# Rivers ----
str_c(import_dir, "NRB_Peru_filedepot/Vector_GCS/Rivers/rios_lineal_idep_ign_100k_geogpsperu.shp") %>%
  st_read() -> rivers

rivers$RASGO_PRIN %>% unique()

rivers %>% 
  select(RASGO_PRIN, RASGO_SECU) %>% 
  rename(rasgo = RASGO_PRIN,
         perenne = RASGO_SECU) %>% 
  mutate(rasgo_id = group_indices(., rasgo)) %>% 

st_write(str_c(import_dir, "MoFuSS_Peru_linux/LULCC/SourceData/InVector_GCS/rivers_mod_dobler.gpkg"))

# rasgo          rasgo_id
# Acéquia             1
# Arroyo              2
# Brazo de Rio        3
# Caño                4
# Estero              5
# Quebrada            6
# Rio                 7


# Roads ----
str_c(import_dir, "NRB_Peru_filedepot/Vector_GCS/Roads/hotosm_per_roads_lines.shp") %>%
  st_read() -> roads

roads$highway %>% unique()

roads %>% 
  select(highway) %>% 
  rename(tipo = highway) %>% 
  mutate(tipo_id = group_indices(., tipo)) %>% 
  
  st_write(str_c(import_dir, "MoFuSS_Peru_linux/LULCC/SourceData/InVector_GCS/roads_mod_dobler.gpkg"))

 #     tipo        tipo_id
 #      bridleway       1
 #       bus_stop       2
 #   construction       3
 #       corridor       4
 #       cycleway       5
 #        footway       6
 #  living_street       7
 #       motorway       8
 #  motorway_link       9
 #           path      10
 #     pedestrian      11
 #          piste      12
 #        primary      13
 #   primary_link      14
 #       proposed      15
 #        raceway      16
 #    residential      17
 #      rest_area      18
 #           road      19
 #      secondary      20
 # secondary_link      21
 #        service      22
 #       services      23
 #          steps      24
 #       tertiary      25
 #  tertiary_link      26
 #          track      27
 #          trunk      28
 #     trunk_link      29
 #   unclassified      30
 #            yes      31


# Zonal stats ----
library(raster)

# Import LUC
str_c(import_dir, "NRB_Peru_filedepot/MINAM_MoFuSS_Peru/SourceDataPeru/Variables Espaciales/InRaster/usos_css.tif") %>% 
  raster() -> luc_11

# Import Asner 100
str_c(import_dir, "NRB_Peru_filedepot/MINAM_MoFuSS_Peru/SourceDataPeru/Variables Espaciales/Adicionales/ACD_Asner_100.tif") %>% 
  raster() -> asner

library(tictoc)

tic()
luc_11 %>% 
  raster::aggregate(fact = 3.3333, fun = modal, na.rm = T) -> luc_11_100
toc() # 75 min

tic()
luc_11_100 %>% 
  resample(asner, method = "ngb") -> luc_11_100_2
toc() # 40 min

tic()
asner %>% 
  resample(luc_11, method = "ngb") -> asner_30
toc()


tic()
zonal(asner, luc_11_100_2, 'median', na.rm = T) -> med_zonal
toc()

tic()
luc_11 %>% 
  crop(extent(asner)) -> luc_11_crop
toc()

tic()
zonal(asner, luc_11_crop, median, na.rm = T) -> med_zonal_2
toc()


# Grass test
library(tidyverse)
library(rgrass7)

# Define projection ESPG code (search here: https://epsg.io/?q=utm+zone+18s+wgs+84)
proj_proj <- 32718
proj_name <- "utm18s"

# Create grass database
glue::glue("grass -e -c EPSG:{proj_proj} {here::here()}/grass_test/{proj_name}") %>% 
  system()

"grass --config path" %>% system(intern = T) -> pth

initGRASS(gisBase = pth,
          gisDbase = glue::glue("{here::here()}/grass_test/"),
          location = proj_name,
          mapset = "PERMANENT",
          override = T)

execGRASS()

