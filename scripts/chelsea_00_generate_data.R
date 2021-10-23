library(tidyverse)
library(furrr)
library(stars)


# DOWNLOAD RAW DATA -------------------------------------------------------------------------------- 

# External data storage
data_dir <- "/media/cdobler/JUNIPERUS/data/chelsea/"

"/home/cdobler/Desktop/tmp/envidatS3paths.txt" %>% # txt with web filepaths 
    read.table() %>% 
    as_tibble() %>% 
    pull(1) %>% 
    .[str_detect(., "_bio")] -> d

plan(multisession, workers = 6) # when RStudio
# plan(multicore, workers = 6) # when standalone R

seq_along(d) %>% 
    future_walk(function(i){
        
        d[i] %>% 
            str_split("/", simplify = T) %>% 
            .[, ncol(.)] -> n
        
        download.file(d[i],
                      destfile = str_c(data_dir, n),
                      method = "wget")
        
    })



# OBTAIN LIMITS TO CROP ----------------------------------------------------------------------------

data_dir %>% 
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

data_dir %>% 
    list.files(full.names = T) %>% 
    .[str_detect(., "1981")] %>% 
    
    .[1] %>% 
    read_stars(RasterIO = list(nXOff = x_min,
                               nYOff = y_min,
                               nXSize = x_count,
                               nYSize = y_count)) %>% 
    st_as_stars() -> ss

# Corroborate
st_read("data/belem.gpkg") -> belem
ggplot() + geom_stars(data = ss) + geom_sf(data = belem)



# CREATE CROPPED FILES -----------------------------------------------------------------------------

tibble(dirpath = list.files(data_dir, full.names = T),
       
       period = dirpath %>% 
           str_split("_", simplify = T) %>% 
           .[,3] %>% 
           str_replace("-", "_"),
       
       biovar = dirpath %>% 
           str_split("_", simplify = T) %>% 
           .[,2] %>% 
           str_sub(4) %>% 
           as.numeric()) %>%
    
    mutate(model = ifelse(str_detect(period, "1981"), "hist",
                          dirpath %>% str_split("_", simplify = T) %>% .[,4])) %>% 
    
    arrange(biovar) -> tb

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
    split("attributes") -> miau

# Save
saveRDS(miau, "data/chelsea_diff_biovars.rds")





