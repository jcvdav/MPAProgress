########################################################################
################### About the raw data #################################
########################################################################
# From https://globalfishingwatch.force.com/gfw/s/data-download:       #
#                                                                      #
# Daily Fishing Effort at 10th Degree Resolution by MMSI, 2012-2016    #
# Fishing effort is binned into grid cells 0.1 degrees on a side,      #
# and measured in units of hours. The time is calculated by assigning  #
# an amount of time to each AIS detection (which is half the time      #
# to the previous plus half the time to the next AIS position). To get #
# information on each mmsi, see Global Fishing Watch data on fishing   #
# vessels.                                                             #
#                                                                      #
# More documentation:                                                  #
# https://github.com/GlobalFishingWatch/global-footprint-of-fisheries/blob/master/data_documentation/fishing_effort_byvessel.md

## Load packages
library(sparklyr) #install.packages("sparlyr")
library(here) #install.packages("here")
library(purrr) #install.packages("purrr")
library(dplyr) #install.packages("dplyr")

## Clear the environment
rm(list = ls())

## Install spark on computer (ncomment next line)
# sparklyr::spark_install()

## Establish a connection on sparklyr
sc <- spark_connect(master = "local")


## Read the first rows of a sample dataset of January 1st 2012
gfw <- read.csv(here("raw_data","gfw_daily","2012-01-01.csv"), nrows = 10) %>% 
  map(function(x){"character"})


## Load all data into sparkly local session
gfw_data <- spark_read_csv(sc = sc,
                           name = "gfw_data",
                           path = here("raw_data","gfw_daily"),
                           columns = gfw,
                           infer_schema = F)

## Get total fishing effort by year and lat/lon bin, and then average across years
raster <- gfw_data %>% 
  dplyr::select(date, lat_bin, lon_bin, fishing_hours) %>% 
  mutate(year = SUBSTRING(date, 1, 4),
         month = SUBSTRING(date, 6, 2)) %>%
  select(-date) %>% 
  mutate_all(as.numeric) %>% 
  mutate(lon_bin = lon_bin / 10,
         lat_bin = lat_bin / 10) %>% 
  filter(between(lon_bin, -100, -80),
         between(lat_bin, -8, 8)) %>% 
  group_by(year, month, lat_bin, lon_bin) %>%
  summarize(total_fishing_hours = sum(fishing_hours, na.rm = T)) %>% 
  ungroup() %>%
  compute("raster")

## Reduce the number of partitions and save to CSV file
sdf_coalesce(x = raster, partitions = 1) %>% 
  spark_write_csv(path = here("data", "raster"))

## The output file is now on data/mean_yearly_fishing_effort.csv

## Close connection
spark_disconnect_all()



