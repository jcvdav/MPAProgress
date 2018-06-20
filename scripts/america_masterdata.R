# Create masterdata for America

## Clear the environment
rm(list = ls())

# Load packages
suppressPackageStartupMessages({
  library(magrittr)
  library(rworldmap)
  library(janitor)
  library(here)
  library(lwgeom)
  library(sf)
  library(tidyverse)
})

# Source functions
source(here("scripts","sfc_as_cols.R"))

deg2rad <- function(deg){
  (deg * pi) / (180)
}


proj <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

countries <- filter(countryExData, EPI_regions %in% c("Latin America and Caribbe", "North America")) %$% ISO3V10

EEZ <- read_sf(dsn = here("raw_data","spatial","World_EEZ_v10_20180221"),
               layer = "eez_v10",
               quiet = T,
               stringsAsFactors = F) %>% 
  clean_names() %>% 
  filter(iso_ter1 %in% countries) %>% 
  st_transform(crs = proj) %>% 
  select(iso3 = iso_ter1)

meow <- read_sf(dsn = here("raw_data","spatial","MEOW"),
                layer = "meow_ecos",
                quiet = T,
                stringsAsFactors = F) %>% 
  clean_names() %>% 
  st_transform(crs = proj)

eez_ecoregion <- EEZ %>% 
  st_intersection(meow) %>% 
  mutate(area = st_area(.),
         eez_ecoregion_area = as.numeric(area) / 1e6,
         eez_ecoregion = paste(iso3, ecoregion, sep = "_")) %>% 
  select(iso3, eez_ecoregion, realm, province, ecoregion, eez_ecoregion_area)

## Fishing effort

mean_binned_fishing <- read.csv(here("data", "mean_yearly_fishing_effort.csv")) %>% 
  filter(mean_fishing_hours > 0)

masked_mean_binned_fishing <- mean_binned_fishing %>% 
  filter(between(lon_bin, -118, -55),
         between(lat_bin, -67, 33)) %>%
  st_as_sf(coords = c("lon_bin", "lat_bin"), crs = proj) %>% 
  st_intersection(y = eez_ecoregion) %>%
  sfc_as_cols(names = c("lon", "lat")) %>% 
  st_set_geometry(NULL) %>% 
  group_by(eez_ecoregion, eez_ecoregion_area) %>% 
  summarize(hours = sum(mean_fishing_hours)) %>% 
  ungroup() %>% 
  select(eez_ecoregion, hours)

## Marine protected areas

mpa_poly <- read_sf(dsn = here("raw_data","spatial","WDPA_Mar2018"),
                    layer = "WDPA_Mar2018_marine-shapefile-polygons",
                    quiet = T,
                    stringsAsFactors = F) %>% 
  clean_names() %>%
  filter(iso3 %in% countries) %>%
  st_make_valid() %>% 
  mutate(dim = st_dimension(.)) %>% 
  filter(dim > 1) %>% 
  st_cast(to = "POLYGON", warn = F, do_split = T) %>% 
  st_cast(to = "POLYGON") %>%
  mutate(mpa_area = st_area(.),
         mpa_area = as.numeric(mpa_area) / 1e6) %>%
  st_transform(crs = proj) %>% 
  select(wdpaid, name, iucn_cat, mpa_area) %>% 
  mutate(source = "polygons")

mpa_points <- read_sf(dsn = here("raw_data", "spatial", "WDPA_Mar2018"),
                      layer = "WDPA_Mar2018_marine-shapefile-points",
                      quiet = T,
                      stringsAsFactors = F) %>% 
  clean_names() %>%
  filter(iso3 %in% countries) %>% 
  sfc_as_cols(names = c("lon", "lat")) %>% 
  mutate(radius = (sqrt(rep_area / pi)) / (cos(deg2rad(lat)) * 111.32)) %>% 
  st_buffer(dist = .$radius) %>% 
  mutate(test_area = st_area(.),
         test_area = as.numeric(test_area) / 1e6,
         diff = test_area - rep_area) %>% 
  st_transform(crs = proj) %>% 
  select(wdpaid, name, iucn_cat, mpa_area = rep_area) %>% 
  mutate(source = "points")

eez_ecoregion_protection <- rbind(mpa_poly, mpa_points) %>% 
  st_intersection(eez_ecoregion) %>% 
  mutate(cropped_mpa_area = st_area(.),
         cropped_mpa_area = as.numeric(cropped_mpa_area / 1e6)) %>% 
  st_set_geometry(NULL) %>%
  group_by(eez_ecoregion, eez_ecoregion_area) %>% 
  summarize(total_protected = sum(cropped_mpa_area)) %>% 
  ungroup() %>% 
  select(eez_ecoregion, total_protected)

## Put fishing effor together with masterdata

masterdata <- eez_ecoregion %>% 
  left_join(masked_mean_binned_fishing, by = "eez_ecoregion") %>% 
  left_join(eez_ecoregion_protection, by = "eez_ecoregion") %>% 
  mutate(norm_hours = hours / eez_ecoregion_area,
         percent_protected = total_protected / eez_ecoregion_area)

saveRDS(masterdata, file = here("data", "america_masterdata.rds"))


