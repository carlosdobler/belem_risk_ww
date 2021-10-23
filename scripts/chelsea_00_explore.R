library(tidyverse)
library(furrr)
library(stars)

# DOWNLOAD *****************************************************************************************

"/home/cdobler/Desktop/tmp/envidatS3paths.txt" %>% 
    read.table() %>% 
    as_tibble() %>% 
    pull(1) %>% 
    .[str_detect(., "_bio")] -> d

plan(multicore, workers = 6)

seq_along(d) %>% 
    future_walk(function(i){
        
        d[i] %>% 
            str_split("/", simplify = T) %>% 
            .[, ncol(.)] -> n
        
        download.file(d[i],
                      destfile = str_glue("/home/cdobler/Documents/outside_Insync/Data/CHELSEA/{n}"),
                      method = "wget")
        
    })



# EXPLORE ******************************************************************************************

# Obtain coord limits
"/media/cdobler/JUNIPERUS/data/chelsea/" %>% 
    list.files(full.names = T) %>% 
    .[str_detect(., "1981")] %>% 
    
    .[1] %>% 
    read_stars(proxy = T) -> s

s %>% 
    st_get_dimension_values("x") %>% 
    {which(near(., -48.780, 0.005))} -> x_min

s %>% 
    st_get_dimension_values("y") %>% 
    {which(near(., -0.9, 0.0042))} -> y_min

s %>% 
    st_get_dimension_values("x") %>% 
    {which(near(., -48.138, 0.005))} %>% 
    {. - x_min} -> x_count

s %>% 
    st_get_dimension_values("y") %>% 
    {which(near(., -1.651, 0.005))} %>% 
    {. - y_min} -> y_count

"/media/cdobler/JUNIPERUS/data/chelsea/" %>% 
    list.files(full.names = T) %>% 
    .[str_detect(., "1981")] %>% 
    
    .[1] %>% 
    read_stars(RasterIO = list(nXOff = x_min,
                               nYOff = y_min,
                               nXSize = x_count,
                               nYSize = y_count)) %>% 
    st_as_stars() -> ss

# st_read("data/belem.gpkg") -> belem

ggplot() + geom_stars(data = ss) + geom_sf(data = belem)



# CREATE CROPPED FILES *****************************************************************************

tibble(dirpath = list.files("/media/cdobler/JUNIPERUS/data/chelsea/", full.names = T),
       period = dirpath %>% str_split("_", simplify = T) %>% .[,3] %>% str_replace("-", "_"),
       biovar = dirpath %>% str_split("_", simplify = T) %>% .[,2] %>% str_sub(4) %>% as.numeric()) %>%
    mutate(model = ifelse(str_detect(period, "1981"), "hist",
                          dirpath %>% str_split("_", simplify = T) %>% .[,4])) %>% 
    arrange(biovar) -> tb

# tibble(biovar_name = c("Δ Annual Mean Temperature",
#                        "Δ Mean Diurnal Range",
#                        "Δ Isothermality",
#                        "Δ Temperature Seasonality",
#                        "Δ Max Temperature of Warmest Month",
#                        "Δ Min Temperature of Coldest Month",
#                        "Δ Temperature Annual Range",
#                        "Δ Mean Temperature of Wettest Quarter",
#                        "Δ Mean Temperature of Driest Quarter",
#                        "Δ Mean Temperature of Warmest Quarter",
#                        "Δ Mean Temperature of Coldest Quarter",
#                        "Δ Annual Precipitation",
#                        "Δ Precipitation of Wettest Month",
#                        "Δ Precipitation of Driest Month",
#                        "Δ Precipitation Seasonality (Coefficient of Variation)",
#                        "Δ Precipitation of Wettest Quarter",
#                        "Δ Precipitation of Driest Quarter",
#                        "Δ Precipitation of Warmest Quarter",
#                        "Δ Precipitation of Coldest Quarter"),
#        biovar = seq_len(19)) %>% 
#     left_join(tb, by = "biovar") -> tb

plan(multisession, workers = 4)

tb %>% pull(period) %>% unique() %>% 
    future_map(function(p){
        
        if(p == "1981_2010"){
            
            tb %>% 
                filter(period == p) %>%
                
                pmap(function(dirpath, biovar, ...){
                    
                    read_stars(dirpath, RasterIO = list(nXOff = x_min,
                                                        nYOff = y_min,
                                                        nXSize = x_count,
                                                        nYSize = y_count),
                               proxy = F) %>% 
                        setNames(str_glue("bv_{biovar}"))
                }) %>% 
                do.call(c, .) %>% 
                merge() %>% 
                setNames(str_glue("p_{p}"))
            
        } else {
            
            map(seq_len(19), function(v){
                
                tb %>% 
                    filter(period == p, 
                           biovar == v) %>%
                    
                    pmap(function(dirpath, ...){
                        
                        read_stars(dirpath, RasterIO = list(nXOff = x_min,
                                                            nYOff = y_min,
                                                            nXSize = x_count,
                                                            nYSize = y_count),
                                   proxy = F)
                        
                    }) %>% 
                    do.call(c, .) %>% 
                    merge() %>% 
                    
                    st_apply(c("x", "y"), mean) %>% 
                    set_names(str_glue("bv_{v}"))
                
            }) %>%
                
                do.call(c, .) %>% 
                merge() %>% 
                setNames(str_glue("p_{p}"))
            
        }
        
    }) %>% 
    
    do.call(c, .) %>% 
    mutate(a = p_2011_2040 - p_1981_2010,
           b = p_2041_2070 - p_1981_2010,
           c = p_2071_2100 - p_1981_2010) %>% 
    select(5:7) %>% 
    st_set_dimensions(3, names = "biovars") %>% 
    setNames(c("2010-2040 - 1980-2010",
               "2040-2070 - 1980-2010",
               "2070-2100 - 1980-2010")) -> miau_1


# ALTERNATIVE: *****
plan(multisession, workers = 7)

seq_len(19) %>% 
    future_map(function(v){
        
        tb %>%
            pull(model) %>% 
            unique() %>%
            .[str_detect(., "hist", negate = T)] %>% 
            
            map(function(m){
                
                tb %>% 
                    filter(biovar == v,
                           model == m | model == "hist") %>% 
                
                    pmap(function(dirpath, period, ...){
                        
                        read_stars(dirpath, RasterIO = list(nXOff = x_min,
                                                            nYOff = y_min,
                                                            nXSize = x_count,
                                                            nYSize = y_count),
                                   proxy = F) %>% 
                            setNames(str_glue("p_{period}"))
                        
                    }) %>% 
                    do.call(c, .) %>% 
                    
                    mutate(a = p_2011_2040 - p_1981_2010,
                           b = p_2041_2070 - p_1981_2010,
                           c = p_2071_2100 - p_1981_2010) %>% 
                    select(a:c) %>% 
                    merge() %>% 
                    st_set_dimensions("attributes", c("2010-2040 - 1980-2010",
                                                      "2040-2070 - 1980-2010",
                                                      "2070-2100 - 1980-2010")) %>% 
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

saveRDS(miau_2, "data/chelsea_diff_biovars.rds")





