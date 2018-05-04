##################################################
### Create a shapefile of the world coastline ####
##################################################
#                                                #
# The tmap package has a coastline for the World.#
# This script reads it in, converts it to an sf  #
# object, and then exports it as a shapefile.    #
#                                                #
##################################################

## Load packages
library(tmap) # install.packages("tmap")
library(sf) # install.packages("sf")
library(dplyr) # install.packages("dplyr")
library(here) # install.packages("here")
library(janitor) # install.packages("janitor")

## Clear the environment
rm(list = ls())

## Define a projection
proj <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

## Load the dataset
data(World)

# Convert to sf object and reproject
World %<>%
  st_as_sf() %>% 
  st_transform(crs = proj) %>% 
  clean_names() %>% 
  select(iso3 = iso_a3, name)

# Export as shapefile
st_write(obj = World,
         dsn = here("data", "spatial"),
         layer = "world_coastline.shp", driver = "ESRI Shapefile",
         delete_dsn = T,
         delete_layer = T,
         quiet = T)
