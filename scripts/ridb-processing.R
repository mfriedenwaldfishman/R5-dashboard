# setup ----
library(vroom)
library(here)
library(tidyverse)
library(janitor)

# read in data ----
# 2018 ridb data
fp_ridb2018 <- here("data/ridb/raw/reservations2018.csv")
# add sept_out to rm `_` to match other column names
raw_ridb2018 <- vroom(fp_ridb2018)

# 2019 ridb data
fp_ridb2019 <- here("data/ridb/raw/reservations2019.csv")
raw_ridb2019 <- vroom(fp_ridb2019)

# 2020 ridb data
fp_ridb2020 <- here("data/ridb/raw/FY20 Historical Reservations Full.csv")
raw_ridb2020 <- vroom(fp_ridb2020)

# 2020 ridb data
fp_ridb2021 <- here("data/ridb/raw/FY21 Historical Reservations Full.csv")
raw_ridb2021 <- vroom(fp_ridb2021)

# 2018 clean and subset ridb ----
usfs_ridb2018 <- raw_ridb2018 %>% 
  # match colnames
  janitor::clean_names(sep_out = "") %>% 
  # cols of interest
  select(
    historicalreservationid,
    agency,
    parentlocation,
    park,
    sitetype,
    usetype,
    facilityzip,
    facilitystate,
    facilitylongitude,
    facilitylatitude,
    customerzip,
    totalpaid,
    startdate,
    enddate,
    orderdate,
    numberofpeople
  ) %>% 
  # extract 5 digit zip codes 
  mutate(customerzip = str_extract(string = customerzip,
                                   pattern = "[:digit:]{5}")) %>% 
  # rm invalid zip codes
  filter(!customerzip %in% c("00000", "99999")) %>% 
  # filter for just USFS in CA, OR, and WA
  filter(agency == "USFS" & facilitystate %in% c("CA", "OR", "WA")) %>% 
  # rename parent_location to forestname
  rename(forestname = parentlocation) %>% 
  # clean forestname #
  mutate(
    # standardize "National Forest"
    forestname = str_replace(string = forestname,
                             pattern = paste(c(
                               "NF - FS", "NF -FS", "NF- FS",
                               "NF-FS", "-FS", " - FS"),
                               collapse = "|"),
                             replacement = "National Forest"),
    # standardize to title case
    forestname = str_to_title(forestname),
    # fix forestname typos based on spatial data
    forestname = case_when(
      forestname == "Columbia R Gorge Nsanational Forest" ~ "Columbia River Gorge National Scenic Area",
      forestname == "Fremont Winema National Forest" ~ "Fremont-Winema National Forest",
      forestname == "Lake Tahoe Basin National Forest" ~ "Lake Tahoe Basin Management Unit",
      forestname == "Mt. Baker-Snoqu National Forest" ~ "Mt. Baker-Snoqualmie National Forest",
      forestname == "Okanogan - Wenatchee National Forest" ~ "Okanogan-Wenatchee National Forest",
      forestname == "Rogue River - Siskiyou National Forest" ~ "Rogue River-Siskiyou National Forests",
      # keep forestname if none of the previous conditions are TRUE
      TRUE ~ forestname)
  ) %>% 
  # rm forests not in R5; forestname var comes from spatial data script
  filter(forestname %in% forestname_R5) %>% 
  # clean park #
  mutate(
    park = str_to_title(park) # 367 tot
  )
  
  
# 2019-2021 clean and subset ridb ----
usfs_ridb <- raw_ridb2021 %>% 
  # match colnames
  janitor::clean_names(sep_out = "") %>% 
  # cols of interest
  select(
    historicalreservationid,
    agency,
    parentlocation,
    park,
    sitetype,
    usetype,
    facilityzip,
    facilitystate,
    facilitylongitude,
    facilitylatitude,
    customerzip,
    totalpaid,
    startdate,
    enddate,
    orderdate,
    numberofpeople
  ) %>% 
  # extract 5 digit zip codes 
  mutate(customerzip = str_extract(string = customerzip,
                                   pattern = "[:digit:]{5}")) %>% 
  # rm invalid zip codes
  filter(!customerzip %in% c("00000", "99999")) %>% 
  # filter for just USFS in CA, OR, and WA
  filter(agency == "USFS" & facilitystate %in% c("California",
                                                 "Oregon",
                                                 "Washington")) %>% 
  # rename parent_location to forestname
  rename(forestname = parentlocation) %>% 
  # clean forestname
  mutate(
    forestname = case_when(forestname == "SHASTA-TRINITY NATIONAL FOREST" ~ "Shasta-Trinity National Forest",
                           TRUE ~ forestname)
  ) %>%
  # rm forests not in R5; forestname var comes from spatial data script
  filter(forestname %in% forestname_R5) %>% 
  # clean park #
  mutate(
    park = str_to_title(park) # 381 tot (2019) 385 tot (2020) 402 tot (2021)
  )
  
