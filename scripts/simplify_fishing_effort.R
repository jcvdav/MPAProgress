#####################################################
##### Load and summarize GFW data with sparklyr #####
#####################################################
# This script loads all raw GFW data from different #
# CSV files found in raw_data/gfw_daily. All of     #
# these files have the same structure. This script  #
# will summarize them all together to creat a mean  #
# fishing effort by year.                           #
#####################################################

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
  mutate(date = SUBSTRING(date, 1, 4)) %>% 
  mutate_all(as.numeric) %>% 
  mutate(lon_bin = lon_bi / 10,
         lat_bin = lat_bin / 10) %>% 
  group_by(date, lat_bin, lon_bin) %>%
  summarize(total_fishing_hours = sum(fishing_hours, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(lat_bin, lon_bin) %>% 
  summarize(mean_fishing_hours = mean(total_fishing_hours, na.rm = T)) %>% 
  ungroup() %>%
  compute("raster")


## Reduce the number of partitions and save to CSV file
sdf_coalesce(x = raster, partitions = 1) %>% 
  spark_write_csv(path = here("data", "raster"))

## The output file is now on data/mean_yearly_fishing_effort.csv

## Close connection
spark_disconnect_all()



