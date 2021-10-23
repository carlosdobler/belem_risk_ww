To run script: `source("name of file", echo = T)`

Climate indices:
http://etccdi.pacificclimate.org/list_27_indices.shtml
https://link.springer.com/article/10.1007/s00382-020-05272-9

knit: (function(inputFile, encoding) {rmarkdown::render(inputFile, encoding = encoding, output_dir = "output")})

ne_download(scale = "large", type = "land", category = "physical", returnclass = "sf") -> land
sf::st_write(land, here::here("data", "land.gpkg"))