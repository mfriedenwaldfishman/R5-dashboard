# setup ----
library(tidyverse)
library(mapview)
library(sf)
library(here)
library(janitor)

# read in data ----
fp_regions <- here("data/spatial/raw/S_USA.AdministrativeForest/")
all_usfs_admin_units <- read_sf(fp_regions) %>% janitor::clean_names()

# R5 au boundaries only ----
r5_au_bounds <- all_usfs_admin_units %>% 
  filter(region == "05")

# name of forests
forestname <- as.vector(unique(r5_au_bounds$forestname))

# quick map ----
mapview(r5_au_bounds)

# save R5 au boundaries ----
sf::write_sf(r5_au_bounds, here("data/spatial/clean/r5_admin_unit_boundaries.shp"))
