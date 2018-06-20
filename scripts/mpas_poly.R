library(lwgeom)
library(sf)
library(here)
library(janitor)
library(tidyverse)

proj <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

mpas_poly <- read_sf(dsn = here("raw_data","spatial","WDPA_Mar2018"),
                layer = "WDPA_Mar2018_marine-shapefile-polygons",
                quiet = T,
                stringsAsFactors = F) %>% 
  janitor::clean_names() %>%
  filter(wdpaid == 400011) %>%
  st_make_valid() %>% 
  st_cast(to = "POLYGON") %>%
  mutate(area2 = st_area(.),
		 area2 = as.numeric(area2) / 1e6) %>%
  st_centroid() %>% 
  janitor::clean_names() %>% 
  st_transform(crs = proj)
  
saveRDS(mpas_poly, here("data", "mpas_poly.rds"))

ggplot(mpas_poly) +
	geom_sf()