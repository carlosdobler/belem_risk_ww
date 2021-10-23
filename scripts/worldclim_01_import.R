
library(tidyverse)
library(stars)

list.files("data/Link to WorldClim/historical_1970_2000/", full.names = T) %>%
    .[1] %>% 
    read_stars() -> miau
    
miau %>% 
    st_get_dimension_values("x") %>% 
    {which(near(., -48.707, 0.02))} -> x_min
    
miau %>% 
    st_get_dimension_values("y") %>% 
    {which(near(., -1.013, 0.02))} -> y_min

miau %>% 
    st_get_dimension_values("x") %>% 
    {which(near(., -48.138, 0.02))} %>% 
    {. - x_min} -> x_count

miau %>% 
    st_get_dimension_values("y") %>% 
    {which(near(., -1.651, 0.02))} %>% 
    {. - y_min} -> y_count
    
list.files("data/Link to WorldClim/historical_1970_2000/", full.names = T) %>%
    .[1] %>% 
    read_stars(RasterIO = list(nXOff = x_min,
                               nYOff = y_min,
                               nXSize = x_count,
                               nYSize = y_count)) -> miau_2

readRDS("data/belem.rds") -> belem
belem %>% st_write("data/belem.gpkg")

ggplot() + geom_stars(data = miau, aes(x = x, y = y)) + geom_sf(data = belem)


tibble(file = list.files("data/Link to WorldClim/historical_1970_2000/", full.names = T),
       v = file %>% 
           str_sub(-6) %>% 
           str_replace("_", "") %>% 
           str_sub(end = -5) %>% 
           as.numeric()) %>% 
    
    arrange(v) %>% 
    pull(file) %>% 
    
    map(~read_stars(.x, RasterIO = list(nXOff = x_min,
                                   nYOff = y_min,
                                   nXSize = x_count,
                                   nYSize = y_count))) %>% 
    do.call(c, .) %>%
    merge() %>% 
    st_set_dimensions("attributes", values = "variables") %>% 
    setNames("p_1970_2000") -> miau_c1


list.files("data/Link to WorldClim/future_2021_2100_CanESM5_ssp585/", full.names = T) %>% #.[1] %>% 
    map(~read_stars(.x, RasterIO = list(nXOff = x_min,
                                    nYOff = y_min,
                                    nXSize = x_count,
                                    nYSize = y_count)) %>% 
            st_as_stars()) %>% 
    do.call(c, .) %>% 
    st_set_dimensions("band", names = "variables", values = st_get_dimension_values(miau_c1, "variables")) %>% 
    setNames(c("p_2020_2040", "p_2040_2060", "p_2060_2080", "p_2080_2100")) -> miau_c2

c(miau_c1, miau_c2) -> miau
rm(miau_c1, miau_c2)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Bioclimatic variables are derived from the monthly temperature and rainfall values in order to 
# generate more biologically meaningful variables. These are often used in species distribution 
# modeling and related ecological modeling techniques. The bioclimatic variables represent annual 
# trends (e.g., mean annual temperature, annual precipitation) seasonality (e.g., annual range in
# temperature and precipitation) and extreme or limiting environmental factors (e.g., temperature 
# of the coldest and warmest month, and precipitation of the wet and dry quarters). A quarter is 
# a period of three months (1/4 of the year).
# 
# They are coded as follows:
#     
# BIO1 = Annual Mean Temperature
# BIO2 = Mean Diurnal Range (Mean of monthly (max temp - min temp))
# BIO3 = Isothermality (BIO2/BIO7) (×100)
# BIO4 = Temperature Seasonality (standard deviation ×100)
# BIO5 = Max Temperature of Warmest Month
# BIO6 = Min Temperature of Coldest Month
# BIO7 = Temperature Annual Range (BIO5-BIO6)
# BIO8 = Mean Temperature of Wettest Quarter
# BIO9 = Mean Temperature of Driest Quarter
# BIO10 = Mean Temperature of Warmest Quarter
# BIO11 = Mean Temperature of Coldest Quarter
# BIO12 = Annual Precipitation
# BIO13 = Precipitation of Wettest Month
# BIO14 = Precipitation of Driest Month
# BIO15 = Precipitation Seasonality (Coefficient of Variation)
# BIO16 = Precipitation of Wettest Quarter
# BIO17 = Precipitation of Driest Quarter
# BIO18 = Precipitation of Warmest Quarter
# BIO19 = Precipitation of Coldest Quarter


biovars <- c("Annual Mean Temperature",
  "Mean Diurnal Range",
  "Isothermality",
  "Temperature Seasonality",
  "Max Temperature of Warmest Month",
  "Min Temperature of Coldest Month",
  "Temperature Annual Range",
  "Mean Temperature of Wettest Quarter",
  "Mean Temperature of Driest Quarter",
  "Mean Temperature of Warmest Quarter",
  "Mean Temperature of Coldest Quarter",
  "Annual Precipitation",
  "Precipitation of Wettest Month",
  "Precipitation of Driest Month",
  "Precipitation Seasonality (Coefficient of Variation)",
  "Precipitation of Wettest Quarter",
  "Precipitation of Driest Quarter",
  "Precipitation of Warmest Quarter",
  "Precipitation of Coldest Quarter")


# - - - - - - 

c("BCC-CSM2-MR",
  "CNRM-CM6-1",
  "CNRM-ESM2-1",
  # "CanESM5",
  "IPSL-CM6A-LR",
  "MIROC-ES2L",
  "MIROC6",
  "MRI-ESM2-0"
  ) -> models

library(furrr)
library(tidyverse)


availableCores()
supportsMulticore()

options(future.fork.enable = TRUE)

plan(multicore, workers = 7)
plan(multisession, workers = 7)

models %>% 
    future_map(function(i){
        
        download.file(str_glue("https://biogeo.ucdavis.edu/data/worldclim/v2.1/fut/2.5m/wc2.1_2.5m_bioc_{i}_ssp585_2041-2060.zip"),
                      destfile = str_glue("/home/cdobler/Documents/outside_Insync/Data/WorldClim/future_2021_2100_ssp585/wc2.1_2.5m_bioc_{i}_ssp585_2041-2060.zip"),
                      method = "wget")
        
    })

"/home/cdobler/Documents/outside_Insync/Data/WorldClim/future_2021_2100_ssp585/" %>%
    list.files() %>% 
    .[str_detect(., "zip")] %>% 
    str_sub(end = -5) %>% 
    
    walk(function(i){
        
        str_glue("/home/cdobler/Documents/outside_Insync/Data/WorldClim/future_2021_2100_ssp585/{i}.zip") %>% 
            unzip(exdir = "/home/cdobler/Documents/outside_Insync/Data/WorldClim/future_2021_2100_ssp585/tmp")
        
        
        list.files("/home/cdobler/Documents/outside_Insync/Data/WorldClim/future_2021_2100_ssp585/tmp/", 
                   recursive = T,
                   full.names = T) %>% 
            
            file.rename(to = str_glue("/home/cdobler/Documents/outside_Insync/Data/WorldClim/future_2021_2100_ssp585/{i}.tif"))
        
        unlink("/home/cdobler/Documents/outside_Insync/Data/WorldClim/future_2021_2100_ssp585/tmp", recursive = T)
        
    })
    


# CREATE CROPPED FILES *****************************************************************************

# constants
x_min <- 3150L
y_min <- 2182L
x_count <- 16L
y_count <- 20L

tibble(dirpath = list.files("/media/cdobler/JUNIPERUS/data/worldclim/", full.names = T)) %>% 
    mutate(
        
        period = dirpath %>% 
            str_split("_", simplify = T) %>% 
            .[,6] %>% 
            str_replace("-", "_") %>% 
            str_sub(end = -5),
        
        period = ifelse(period == "", "1970_2000", period)
        
    ) -> tb
   
# Historic    
tb %>% 
    filter(period == "1970_2000") %>% 
    mutate(biovar = dirpath %>% 
               str_sub(-6) %>% 
               str_replace("_", "") %>% 
               str_sub(end = -5) %>% 
               as.numeric()) %>% 
    
    arrange(biovar) %>% 
    pull(dirpath) %>% 
    
    read_stars(RasterIO = list(nXOff = x_min,
                               nYOff = y_min,
                               nXSize = x_count,
                               nYSize = y_count)) %>% 
    merge() %>% 
    st_set_dimensions(3, names = "biovars", values = str_glue("bv_{seq_len(19)}")) %>% 
    setNames("p_1970_2000") -> miau_hist

# Future
tb %>% 
    filter(period != "1970_2000") %>% 
    mutate(model = dirpath %>% 
               str_split("_", simplify = T) %>% 
               .[,4] %>% 
               str_replace_all("-", "_")) %>% 
    filter(model != "MRI_ESM2_0") -> tb_future

tb_future %>% 
    pull(model) %>% 
    unique() %>% 
    
    map(function(m){
        
        tb_future %>% 
            filter(model == m) %>% 
            pull(dirpath) %>%
            
            read_stars(RasterIO = list(nXOff = x_min,
                                       nYOff = y_min,
                                       nXSize = x_count,
                                       nYSize = y_count),
                       proxy = F) %>% 
            
            st_set_dimensions(3, names = "biovars", values = str_glue("bv_{seq_len(19)}")) %>%
            merge() %>% 
            st_set_dimensions(4, names = "period", values = c("p_2020_2040",
                                                              "p_2040_2060",
                                                              "p_2060_2080",
                                                              "p_2080_2100")) %>% 
            setNames(m)
    
    }) %>% 
    do.call(c, .) %>% 
    merge() %>% 
    split("period") -> miau_fut

# Differences
seq_len(19) %>% 
    map(function(v){
        
        tb_future %>% 
            pull(model) %>% 
            unique() %>%
            
            map(function(m){
                
                db <- c(miau_hist, 
                        miau_fut %>% 
                            slice(attributes,
                                  which(st_get_dimension_values(miau_fut, 4) == m)))
                
                db %>% 
                    slice(biovars, v) %>% 
                    mutate(a = p_2020_2040 - p_1970_2000,
                           b = p_2040_2060 - p_1970_2000,
                           c = p_2060_2080 - p_1970_2000,
                           d = p_2080_2100 - p_1970_2000) %>% 
                    select(a:d) %>% 
                    merge() %>% 
                    st_set_dimensions("attributes", c("2020-2040 - 1970-2000",
                                                      "2040-2060 - 1970-2000",
                                                      "2060-2080 - 1970-2000",
                                                      "2080-2100 - 1970-2000")) %>% 
                    setNames(m)
                
            }) %>% 
            
            do.call(c, .) %>%
            merge() %>% 
            
            st_apply(c("x", "y", "attributes"), mean) %>% 
            setNames(str_glue("bv_{v}"))
    
    }) %>% 
    
    do.call(c, .) %>% 
    merge() %>%
    st_set_dimensions(4, names = "biovars") %>% 
    split("attributes") -> miau_2

# SAVE ********************************************************************************************

saveRDS(miau_2, "data/worldclim_diff_biovars.rds")
