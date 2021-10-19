
# Script to import data from buckets
# **********************************

# Libraries
library(tidyverse)
library(stars)
library(furrr)
library(tictoc)


# INPUT SECTION:
# Un-comment one model:
# mod <- "Had"
# mod <- "MPI"
mod <- "Nor"

# Un-comment variables:
# vari <- "maximum_temperature"
# va <- "tasmax"
# vari <- "minumum_temperature"
# va <- "tasmin"
vari <- "wetbulb_temperature"
va <- "wbtemp"

# Name df to save
save_df <- str_glue("{va}_{tolower(mod)}_noerr")

# *********************************





# Bucket directory
data_dir <- str_glue("/home/cdobler/bucket_mnt/RCM_regridded_data/REMO2015/SAM/daily/{vari}/")

# Vector of files
files <- vector()
class(files) <- "try-error"
while(class(files) == "try-error"){
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
