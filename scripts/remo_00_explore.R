library(tidyverse)
library(stars)
library(furrr)
library(lubridate)
library(tictoc)

# Explore 
data_dir <- "/home/cdobler/bucket_mnt/RCM_regridded_data/REMO2015/SAM/daily/average_temperature/"

data_dir %>% 
  list.files(full.names = T) %>% 
  .[str_detect(., "Had")] -> files_had



tas_had_noerr %>% do.call("c", .) %>% st_set_dimensions("time", date_vector)


data_dir %>% 
  list.files(full.names = T) %>% 
  .[str_detect(., "MPI-M")] -> files_mpi

data_dir %>% 
  list.files(full.names = T) %>% 
  .[str_detect(., "Nor")] -> files_nor


files_had %>% 
  .[1:2] %>% 
  read_ncdf() -> tas_had
# 360 units in time
seq(as_date("19700101"), as_date("19701230"), by = "1 day") %>% length() # 364

data_dir %>% 
  list.files(full.names = T) %>% 
  .[2] %>% 
  read_ncdf() -> tas_1971_1975
# time = 1800
seq(as_date("19710101"), as_date("19751230"), by = "1 day") %>% length() # 1825

tas_1970 %>% 
  slice(time, 1) %>% plot()

tas_1970 %>% 
  st_get_dimension_values("lon") %>% 
  {which(near(., -48.6, 0.1))} # 286

tas_1970 %>% 
  st_get_dimension_values("lon") %>% 
  {which(near(., -48.2, 0.1))} # 288

tas_1970 %>% 
  st_get_dimension_values("lat") %>% 
  {which(near(., -1.6, 0.1))} # 282

tas_1970 %>% 
  st_get_dimension_values("lat") %>% 
  {which(near(., -1.09, 0.1))} # 285


tas_1970_belem %>% 
  st_apply("time", mean) %>% 
  as_tibble()


b %>% 
  as_tibble() %>% 
  units::drop_units() %>% 
  {
    ggplot() +
      geom_raster(data = ., aes(x = lon, y = lat, fill = tas)) +
      geom_sf(data = a) +
      coord_sf(crs = st_crs(4326), xlim = c(-49, -47), ylim = c(-2, 0))
  }




# **************************************
# FIRE WEATHER INDEX
# **************************************

belem <- readRDS("data/belem.rds")
st_bbox(belem)

data_dir <- str_glue("/home/cdobler/bucket_mnt/RCM_regridded_data/REMO2015/global_mosaic/daily/fwi_ISIMIP3_bias-adjusted_files/")

data_dir %>%
  list.files(full.names = T) %>% 
  .[1] %>% 
  read_ncdf() -> miau

miau %>% 
  st_get_dimension_values("lon") %>% 
  {which(near(., st_bbox(belem)[1], 0.1))} %>% 
  {. - 10} -> lon_min

miau %>% 
  st_get_dimension_values("lat") %>% 
  {which(near(., st_bbox(belem)[2], 0.1))} %>% 
  {. - 10} -> lat_min
  
miau %>% 
  st_get_dimension_values("lon") %>% 
  {which(near(., st_bbox(belem)[3], 0.1))} %>% 
  {. + 5} %>% 
  {. - lon_min} -> lon_count

miau %>% 
  st_get_dimension_values("lat") %>% 
  {which(near(., st_bbox(belem)[4], 0.1))} %>% 
  {. + 5} %>% 
  {. - lat_min} -> lat_count

system('gcsfuse cmip5_data /home/cdobler/bucket_mnt/')
















