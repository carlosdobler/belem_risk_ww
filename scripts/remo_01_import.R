
# Script to import data from buckets
# **********************************

# Libraries
library(tidyverse)
library(stars)
library(furrr)
library(tictoc)


# INPUT SECTION:
# Un-comment one model:
mod <- "Had"
# mod <- "MPI"
# mod <- "Nor"

# Un-comment variables:
vari <- "maximum_temperature"
va <- "tasmax"

# Name df to save
save_df <- str_glue("{va}_{tolower(mod)}_noerr")

# *********************************





# Bucket directory
data_dir <- str_glue("/home/cdobler/bucket_mnt/RCM_regridded_data/REMO2015/SAM/daily/{vari}/")

# Vector of files
files <- vector()
class(files) <- "try-error"
while(class(files) == "try-error")
{
    try(
        data_dir %>%
            list.files(full.names = T) %>%
            .[str_detect(., mod)] -> files
    )
}


# Function to import without errors
func_safe_import <- function(f)
{
  print(str_glue("Importing: {f} / {length(files)}"))

  x <- vector()
  class(x) <- "try-error"
  while(class(x) == "try-error")
  {
    try(
      read_ncdf(files[f],
                make_time = F, # only for Had
                ncsub = cbind(start = c(283, 280, 1),
                                        count = c(6, 6, NA))) -> x
    )
  }
  return(x)
}

# Parallel config
cores <- availableCores()
plan(multicore, workers = cores - 1)

# Run loop
tic()
seq_along(files) %>%
  future_map(func_safe_import) -> df
toc()

# Save output (a list)
saveRDS(df, str_glue("data/{save_df}.rds"))

print("DONE!")





# **************************************************************************************************
# FOR WHEN DATA IS LOCALLY STORED 

library(tidyverse)
library(stars)
library(furrr)
library(lubridate)

belem <- st_read("data/belem.gpkg")

# table of files
tibble(filepath = "/media/cdobler/JUNIPERUS/data/remo/wetbulb_temp/" %>%
           list.files(full.names = T)) %>% 
    mutate(
        model = filepath %>%
            str_split("_", simplify = T) %>% 
            .[,5] %>% 
            str_replace_all("-", "_"),
        
        ti = filepath %>%
            str_split("_", simplify = T) %>% 
            .[,6] %>% 
            str_sub(end = 8),
        
        tf = filepath %>%
            str_split("_", simplify = T) %>% 
            .[,6] %>% 
            str_sub(10, 17)
        
    ) -> tb_files

# Obtain ncsub vals
tb_files %>%
    pull(filepath) %>%
    .[1] %>% 
    read_ncdf() -> t

t %>% 
    st_get_dimension_values("lon") %>%
    {which(near(., st_bbox(belem)["xmin"], 0.1))} -> x_min

t %>% 
    st_get_dimension_values("lon") %>% 
    {which(near(., st_bbox(belem)["xmax"], 0.1))} %>% 
    {. - x_min} -> x_count

t %>% 
    st_get_dimension_values("lat") %>%
    {which(near(., st_bbox(belem)["ymin"], 0.1))} -> y_min

t %>% 
    st_get_dimension_values("lat") %>% 
    {which(near(., st_bbox(belem)["ymax"], 0.1))} %>% 
    {. - y_min} -> y_count

rm(t)
gc()

# plan(multisession, workers = 7)
plan(multicore, workers = 7)

tb_files %>% 
    pull(model) %>% 
    unique() %>% 
    
    map(function(m){
        
        tb_files %>%
            filter(model == m) %>% 
            slice(1) %>% 
            pull(filepath) %>% 
            read_stars(proxy = T) %>% 
            st_get_dimension_values("time") %>% 
            length() -> time_count
        
        tb_files %>%
            filter(model == m) %>%
            pmap(function(filepath, ti, tf, ...)
            {
                
                # Create date vectors
                if(m == "HadGEM2_ES"){
                    
                    tibble(date = seq(as_date(ti), as_date(tf), by = "1 day")) %>% 
                        mutate(year = year(date),
                               month = month(date),
                               day = day(date)) %>% 
                        group_by(year, month) %>% 
                        summarise(day_min = min(day),
                                  day_max = max(day)) %>% 
                        ungroup() %>%
                        
                        pmap(function(year, month, day_min, day_max)
                        {
                            seq(as_date(str_c(year,
                                              str_pad(month, 2, "left", 0),
                                              str_pad(day_min, 2, "left", 0))),
                                
                                as_date(str_c(year,
                                              str_pad(month, 2, "left", 0),
                                              str_pad(day_max, 2, "left", 0))),
                                
                                length.out = 30)
                        }
                        ) %>% 
                        do.call(c, .) -> date_vector
                    
                } else {
                    
                    seq(as_date(ti), as_date(tf), by = "1 day") -> date_vector
                    
                }
                
                # Read file
                read_ncdf(filepath,
                          make_time = F,
                          make_units = F,
                          ncsub = cbind(start = c(x_min, y_min, 1),
                                        count = c(x_count+1, y_count, time_count))
                          ) %>% 
                    
                    slice(lon, 1:2) %>% 
                    slice(lat, 1:2) %>% 
                    st_set_dimensions("time", date_vector)
                
                
            }) %>% 
            do.call(c, .) %>% 
            setNames(m)
        
    }) -> df

# Save output (a list)
saveRDS(df, str_glue("data/remo_wetbulb.rds"))


# *************************************************************************************************
# FOR WORKING HOURS

library(tidyverse)
library(stars)

"/media/cdobler/JUNIPERUS/data/remo/working_hours/" %>%
    list.files(full.names = T) %>% 
    map(read_ncdf) %>% 
    do.call(c,.) %>%
    st_crop(para) %>% 
    # as_tibble()
    {
        ggplot() +
            geom_stars(data = .) +
            geom_sf(data = para, fill = NA) +
            geom_sf(data = st_sfc(st_point(c(-48.5161, -1.3679)), crs = st_crs(para)), color = "white")
        
    }

