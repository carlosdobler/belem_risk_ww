library(tidyverse)
library(stars)
library(lubridate)


# ************************
# INPUT SECTION
# Un-comment one model:
# mod <- "had"
# mod <- "mpi"
mod <- "nor"

# Un-comment variables:
va <- "precip"
# ************************


# DATE VECTOR
if(mod == "had"){
  
  tibble(date = seq(as_date("1970-01-01"), as_date("2099-12-30"), by = "1 day")) %>% 
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
  
  seq(as_date("19700101"), as_date("21001231"), by = "1 day") -> date_vector

}

# ADD DATE AND SAVE
read_rds(str_glue("data/{va}_{mod}_noerr.rds")) %>% 
  do.call(c, .) %>% 
  st_set_dimensions("time", date_vector) %>%
  
  saveRDS(str_glue("data/{va}_{mod}_wdates.rds"))


